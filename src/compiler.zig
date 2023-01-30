const std = @import("std");
const testing = std.testing;
const inst = @import("inst.zig");
const Inst = inst.Inst;
const Reg = inst.Reg;
const Op = inst.Op;
const Token = @import("token.zig").Token;
const Env = @import("env.zig").Env;
const Chunk = @import("chunk.zig").Chunk;
const Value = @import("value.zig").Value;
const Allocator = std.mem.Allocator;
const Parser = @import("parser.zig");
const Ast = @import("ast.zig");
const FnCall = Ast.FnCall;
const NodeIdx = Ast.NodeIdx;
const Node = Ast.Node;

pub const CompileError = error{
    OutOfRegisters,
    WrongNumberArguments,
    NotYetImplemented,
} || Chunk.ChunkError || Parser.ParseError;

/// helper/wrapper function to take some source and
/// compile it into a bytecode chunk
pub fn compile(src: []const u8, env: *Env, allocator: Allocator) CompileError!Chunk {
    _ = env;
    var parser = Parser.init(allocator);
    defer parser.deinit();

    var ast = try parser.parse(src);
    defer ast.deinit(allocator);

    var chunk = Chunk.init(allocator);
    var compiler = Compiler{
        .chunk = &chunk,
        .ast = &ast,
    };
    try compiler.compile();

    return chunk;
}

pub const Compiler = struct {
    ast: *Ast,

    chunk: *Chunk,

    /// the next free register
    last_reg: Reg = 0,

    fn allocReg(self: *Compiler) !Reg {
        if (self.last_reg == 255) return CompileError.OutOfRegisters;
        self.last_reg += 1;
        return self.last_reg;
    }

    fn freeReg(self: *Compiler) void {
        if (self.last_reg == 0) {
            std.debug.print("attempt to free last register\n", .{});
            return;
        }
        self.last_reg -= 1;
    }

    /// produces a new chunk from the Ast given
    /// we can assume that the Ast is for a single, top-level expression
    /// and therefore will only produce one chunk
    pub fn compile(self: *Compiler) CompileError!void {
        // start with the root
        try self.evalNode(0);
        // push return for for the last allocated register?
        _ = try self.chunk.pushInst(Inst.init(.ret, .{
            .r = self.last_reg,
        }));
    }

    fn evalNode(
        self: *Compiler,
        idx: NodeIdx,
    ) CompileError!void {
        const node = self.ast.nodes[idx];
        switch (node.tag) {
            .constant => {
                // push constant value into chunk
                _ = try self.chunk.pushInst(Inst.init(
                    .load,
                    .{
                        .r = try self.allocReg(),
                        .u = try self.chunk.pushConst(
                            self.ast.getData(Value, node.children.l),
                        ),
                    },
                ));
            },
            .define => try self.applyDefine(idx),
            .set => try self.applySet(idx),
            .seq => try self.evalSequence(idx),
            .@"if" => try self.applyCond(idx),
            .call => try self.applyNode(idx),
            .symbol => {
                // TOOD: intern strings so we don't have a string
                // pushed to the consts twice
                // get the string into a constant
                _ = try self.chunk.pushInst(Inst.init(
                    .get_global,
                    .{
                        .r = try self.allocReg(),
                        .u = try self.chunk.pushConstStr(
                            self.ast.tokens[node.token_idx].loc.slice,
                        ),
                    },
                ));
            },
            else => return CompileError.NotYetImplemented,
        }
    }

    fn evalSequence(
        self: *Compiler,
        idx: NodeIdx,
    ) CompileError!void {
        var seq_idx: NodeIdx = idx;

        while (true) {
            const seq = self.ast.nodes[seq_idx];
            try self.evalNode(seq.children.l);

            if (seq.children.r == 0) {
                break;
            } else {
                // since there is another value,
                // we should free this register
                self.freeReg();
                seq_idx = seq.children.r;
            }
        }
    }

    fn applyCond(
        self: *Compiler,
        if_idx: NodeIdx,
    ) CompileError!void {
        // where we will store the result.
        // we don't the result register since it will be used outside of the if scope
        const res = try self.allocReg();

        const if_node = self.ast.nodes[if_idx];
        // compile cond
        try self.evalNode(if_idx + 1);
        // we know that the result of compiling the condition should be in last reg
        const cond_reg = self.last_reg;
        // add test
        _ = try self.chunk.pushInst(
            Inst.init(.eq_true, .{ .r = cond_reg }),
        );
        // jump over then expr
        const thn_jump_idx = try self.chunk.pushInst(
            Inst.init(.jmp, 0),
        );
        // compile then
        try self.evalNode(if_node.children.l);
        const then_reg = self.last_reg;
        // move the then branch
        const thn_end_idx = try self.chunk.pushInst(
            Inst.init(.move, .{
                .r = res,
                // the result of then should be in the latest register?
                .r1 = then_reg,
            }),
        );

        var thn_jump_amt = @intCast(inst.ArgI, thn_end_idx - thn_jump_idx);

        if (if_node.children.r != 0) {
            // compile else
            thn_jump_amt += 1;
            const els_jump_idx = try self.chunk.pushInst(
                Inst.init(.jmp, 0),
            );
            try self.evalNode(if_node.children.r);
            // move the else branch
            const else_reg = self.last_reg;
            const els_end_idx = try self.chunk.pushInst(
                Inst.init(.move, .{
                    .r = res,
                    .r1 = else_reg,
                }),
            );
            self.chunk.code[els_jump_idx].data = @intCast(inst.ArgI, els_end_idx - els_jump_idx);

            // free the else register
            self.freeReg();
        }

        // free the then register
        self.freeReg();
        // free the cond register
        self.freeReg();

        // patch the jump instruction
        self.chunk.code[thn_jump_idx].data = thn_jump_amt;
    }

    fn applyDefine(
        self: *Compiler,
        def_idx: NodeIdx,
    ) CompileError!void {
        const def_node = self.ast.nodes[def_idx];
        // get the value by evaluating
        try self.evalNode(def_node.children.r);
        const value_reg = self.last_reg;

        // create a symbol by evaluating
        // TODO: make this use a symbol data type directly

        // copy symbol name with allocator
        const lhs = self.ast.nodes[def_node.children.l];
        _ = try self.chunk.pushInst(Inst.init(
            .define_global,
            .{
                .r = value_reg,
                .u = try self.chunk.pushConstStr(
                    self.ast.tokens[lhs.token_idx].loc.slice,
                ),
            },
        ));

        // pop the register used for value
        self.freeReg();
    }

    fn applySet(
        self: *Compiler,
        def_idx: NodeIdx,
    ) CompileError!void {
        const def_node = self.ast.nodes[def_idx];
        // get the value by evaluating
        try self.evalNode(def_node.children.r);
        const value_reg = self.last_reg;

        // create a symbol by evaluating
        // TODO: make this use a symbol data type directly

        // copy symbol name with allocator
        const lhs = self.ast.nodes[def_node.children.l];
        _ = try self.chunk.pushInst(Inst.init(
            .set_global,
            .{
                .r = value_reg,
                .u = try self.chunk.pushConstStr(
                    self.ast.tokens[lhs.token_idx].loc.slice,
                ),
            },
        ));

        // pop the register used for value
        self.freeReg();
    }

    /// apply a call
    /// we evaluate the caller and the list of arguments
    fn applyNode(
        self: *Compiler,
        call_idx: NodeIdx,
    ) CompileError!void {
        const call_node = self.ast.nodes[call_idx];
        const call_data = self.ast.getData(FnCall, call_node.children.l);
        const caller = self.ast.nodes[call_data.caller_idx];
        // get the token of the caller
        // check if it is a builtin proceedure
        const args_idx = call_node.children.r;
        const caller_tag = self.ast.tokens[caller.token_idx].tag;

        return switch (caller_tag) {
            .plus,
            .minus,
            .asterisk,
            .slash,
            .modulo,
            .gt,
            .lt,
            .gte,
            .lte,
            => self.applyBinOp(caller_tag, call_data, args_idx),
            .@"and",
            .@"or",
            .@"not",
            => self.applyBool(caller_tag, call_data, args_idx),
            // haven't implemented any other functions
            else => CompileError.NotYetImplemented,
        };
    }

    fn applyBinOp(
        self: *Compiler,
        caller_tag: Token.Tag,
        call_data: FnCall,
        args_idx: NodeIdx,
    ) CompileError!void {
        // if we are using modulo then check arity
        if (caller_tag == .modulo and call_data.n_args != 2) {
            return CompileError.WrongNumberArguments;
        }

        if (call_data.n_args == 0) {
            return CompileError.WrongNumberArguments;
        }

        // evaluate each item in the list
        var pair_idx = args_idx;
        // load the first item into a register
        var pair = self.ast.nodes[pair_idx];
        try self.evalNode(pair.children.l);
        const reg0 = self.last_reg;
        pair_idx = pair.children.r;
        // can the instruciton be constantized
        const const_inst = switch (caller_tag) {
            .plus,
            .minus,
            .asterisk,
            .slash,
            => true,
            else => false,
        };

        // if the right child is 0 then we have reached the end of the list
        while (pair_idx != 0) {
            pair = self.ast.nodes[pair_idx];
            // check if the value is a constant
            const is_const = self.ast.nodes[pair.children.l].tag == .constant;

            // apply primitive func to them and store in first register
            if (const_inst and is_const) {
                _ = try self.chunk.pushInst(.{
                    .op = Op.fromPrimitiveTokenTag(caller_tag, is_const),
                    .data = @bitCast(inst.ArgSize, inst.ArgU{
                        .r = reg0,
                        .u = try self.chunk.pushConst(
                            self.ast.getData(Value, self.ast.nodes[pair.children.l].children.l),
                        ),
                    }),
                });
            } else {
                // get the value into a register
                try self.evalNode(pair.children.l);
                const reg1 = self.last_reg;
                // if we are doing arithmetic then we can do the const stuff
                _ = try self.chunk.pushInst(.{
                    .op = Op.fromPrimitiveTokenTag(caller_tag, is_const),
                    .data = @bitCast(inst.ArgSize, inst.Arg3{
                        .r = reg0,
                        .r1 = reg0,
                        .r2 = reg1,
                    }),
                });

                self.freeReg();
            }
            pair_idx = pair.children.r;
        }
    }

    fn applyBool(
        self: *Compiler,
        caller_tag: Token.Tag,
        call_data: FnCall,
        args_idx: NodeIdx,
    ) CompileError!void {
        if (call_data.n_args == 0) {
            return CompileError.WrongNumberArguments;
        }

        // evaluate each item in the list
        var pair_idx = args_idx;

        // load the first item into a register
        var pair = self.ast.nodes[pair_idx];
        try self.evalNode(pair.children.l);
        const reg = self.last_reg;
        pair_idx = pair.children.r;

        // if we are using not then we only need the first item
        if (caller_tag == .@"not") {
            if (call_data.n_args != 1) {
                return CompileError.WrongNumberArguments;
            }

            _ = try self.chunk.pushInst(
                Inst.init(.@"not", .{
                    .r = reg,
                    .r1 = reg,
                }),
            );

            return;
        }

        // index where we start adding jumps
        const start_idx = self.chunk.n_inst - 1;

        const compare: Reg = switch (caller_tag) {
            .@"or" => 1,
            .@"and" => 0,
            else => return CompileError.NotYetImplemented,
        };

        while (pair_idx != 0) {
            // test the last value
            _ = try self.chunk.pushInst(
                Inst.init(.@"test", .{
                    .r = reg,
                    .r1 = reg,
                    .r2 = compare,
                }),
            );
            // get the index of the jump instruction
            _ = try self.chunk.pushInst(
                Inst.init(.jmp, 0),
            );
            // free reg, since we only used it in the test instruction
            self.freeReg();

            // compile this value
            pair = self.ast.nodes[pair_idx];
            try self.evalNode(pair.children.l);

            pair_idx = pair.children.r;
        }

        // patch all the instructions
        const done_idx = self.chunk.n_inst - 1;
        for (self.chunk.code[start_idx..done_idx]) |*jmp, i| {
            if (jmp.op == .jmp) {
                const jmp_amt = @intCast(inst.ArgI, done_idx - (start_idx + i));
                jmp.data = jmp_amt;
            }
        }
    }
};

const eps = std.math.epsilon(f32);

test "compile number literal" {
    const code =
        \\32
    ;
    var parser = Parser.init(std.testing.allocator);
    defer parser.deinit();

    var ast = try parser.parse(code);
    defer ast.deinit(std.testing.allocator);

    var chunk = Chunk{};
    defer chunk.deinit();
    var compiler = Compiler{
        .chunk = &chunk,
        .ast = &ast,
    };
    try compiler.compile();

    try std.testing.expectApproxEqAbs(@as(f32, 32), chunk.consts[0].float, eps);
}

test "if statement" {
    const code =
        \\(if #t 12 -3)
    ;
    var env = Env.init(testing.allocator);
    defer env.deinit();
    var parser = Parser.init(std.testing.allocator);
    defer parser.deinit();

    var ast = try parser.parse(code);
    defer ast.deinit(std.testing.allocator);
    var chunk = Chunk{};
    defer chunk.deinit();
    var compiler = Compiler{
        .chunk = &chunk,
        .ast = &ast,
    };
    try compiler.compile();

    try testing.expect(compiler.last_reg == 1);
    try testing.expect(true == chunk.consts[0].boolean);
    try testing.expect(12 == chunk.consts[1].float);
    try testing.expect(-3 == chunk.consts[2].float);

    try testing.expectEqualSlices(Inst, &.{
        // load the test
        Inst.init(.load, .{ .r = 2, .u = 0 }),
        // test if r1 is false (test skips over jump if false)
        Inst.init(.eq_true, .{ .r = 2 }),
        // jump over thn branch
        Inst.init(.jmp, 3),
        // thn branch
        // load 12 into r3
        Inst.init(.load, .{ .r = 3, .u = 1 }),
        // move into result register
        Inst.init(.move, .{ .r = 1, .r1 = 3 }),
        // jump over else branch
        Inst.init(.jmp, 2),
        //-----------------------------
        // else branch
        Inst.init(.load, .{ .r = 4, .u = 2 }),
        Inst.init(.move, .{ .r = 1, .r1 = 4 }),
        // -----------------------
        // return
        Inst.init(.ret, .{ .r = 1 }),
    }, chunk.code[0..chunk.n_inst]);
}

test "if no else" {
    const code =
        \\(if #t 12)
    ;
    var env = Env.init(testing.allocator);
    defer env.deinit();
    var chunk = try compile(code, &env, std.testing.allocator);
    defer chunk.deinit();

    try testing.expect(true == chunk.consts[0].boolean);
    try testing.expect(12 == chunk.consts[1].float);

    try testing.expectEqualSlices(Inst, &.{
        // load the test
        Inst.init(.load, .{ .r = 2, .u = 0 }),
        // test if r1 is false (test skips over jump if false)
        Inst.init(.eq_true, .{ .r = 2 }),
        // jump over thn branch
        Inst.init(.jmp, 2),
        // thn branch
        // load 12 into r3
        Inst.init(.load, .{ .r = 3, .u = 1 }),
        // move into result register
        Inst.init(.move, .{ .r = 1, .r1 = 3 }),
        // -----------------------
        // return
        Inst.init(.ret, .{ .r = 1 }),
    }, chunk.code[0..chunk.n_inst]);
}

test "primitive call" {
    const code =
        \\(+ 1 2 3)
    ;
    var env = Env.init(testing.allocator);
    defer env.deinit();
    var chunk = try compile(code, &env, std.testing.allocator);
    defer chunk.deinit();

    try testing.expectEqualSlices(Inst, &.{
        // load 1 into 1
        Inst.init(.load, .{ .r = 1, .u = 0 }),
        // add them together and store in 1
        Inst.init(.addconst, .{ .r = 1, .u = 1 }),
        // load 3 into 2 (we popped 2 off since we don't need it anymore)
        Inst.init(.addconst, .{ .r = 1, .u = 2 }),
        // return
        Inst.init(.ret, .{ .r = 1 }),
    }, chunk.code[0..chunk.n_inst]);
}

test "and" {
    const code =
        \\(and #t 33)
        \\(and #t 33 #f)
    ;

    var env = Env.init(testing.allocator);
    defer env.deinit();
    var chunk = try compile(code, &env, std.testing.allocator);
    defer chunk.deinit();

    try testing.expectEqualSlices(Inst, &.{
        // load arg1 into reg1
        Inst.init(.load, .{ .r = 1, .u = 0 }),
        // test if reg1 is false, and store in reg 1
        Inst.init(.@"test", .{ .r = 1, .r1 = 1, .r2 = 0 }),
        // if we are at this instruction then r1 was false and we are done
        Inst.init(.jmp, 1),
        // load arg2 into reg1
        Inst.init(.load, .{ .r = 1, .u = 1 }),

        // load arg1 into reg1
        Inst.init(.load, .{ .r = 1, .u = 2 }),
        // test if reg1 is false, and store in reg 1
        Inst.init(.@"test", .{ .r = 1, .r1 = 1, .r2 = 0 }),
        // if we are at this instruction then r1 was false and we are done
        Inst.init(.jmp, 4),
        // load arg2 into reg1
        Inst.init(.load, .{ .r = 1, .u = 3 }),
        // test if reg1 is false, and store in reg 1
        Inst.init(.@"test", .{ .r = 1, .r1 = 1, .r2 = 0 }),
        // if we are at this instruction then r1 was false and we are done
        Inst.init(.jmp, 1),
        // load arg3 into reg1
        Inst.init(.load, .{ .r = 1, .u = 4 }),

        // return
        Inst.init(.ret, .{ .r = 1 }),
    }, chunk.code[0..chunk.n_inst]);
}

test "or" {
    const code =
        \\(or #f 33)
    ;

    var env = Env.init(testing.allocator);
    defer env.deinit();
    var chunk = try compile(code, &env, std.testing.allocator);
    defer chunk.deinit();

    try testing.expectEqualSlices(Inst, &.{
        // load arg1 into reg1
        Inst.init(.load, .{ .r = 1, .u = 0 }),
        // test if reg1 is false, and store in reg 1
        Inst.init(.@"test", .{ .r = 1, .r1 = 1, .r2 = 1 }),
        // if we are at this instruction then r1 was false and we are done
        Inst.init(.jmp, 1),
        // load arg2 into reg1
        Inst.init(.load, .{ .r = 1, .u = 1 }),
        // return
        Inst.init(.ret, .{ .r = 1 }),
    }, chunk.code[0..chunk.n_inst]);
}

test "not" {
    const code =
        \\(not #f)
    ;

    var env = Env.init(testing.allocator);
    defer env.deinit();
    var chunk = try compile(code, &env, std.testing.allocator);
    defer chunk.deinit();

    try testing.expectEqualSlices(Inst, &.{
        // load arg1 into reg1
        Inst.init(.load, .{ .r = 1, .u = 0 }),
        // test if reg1 is false, and store in reg 1
        Inst.init(.@"not", .{ .r = 1, .r1 = 1 }),
        // return
        Inst.init(.ret, .{ .r = 1 }),
    }, chunk.code[0..chunk.n_inst]);
}

test "not too many" {
    const code =
        \\(not #f 33)
    ;

    var env = Env.init(testing.allocator);
    defer env.deinit();
    try testing.expectError(CompileError.WrongNumberArguments, compile(code, &env, std.testing.allocator));
}

test "define" {
    const code =
        \\(define global 32)
    ;

    var env = Env.init(testing.allocator);
    defer env.deinit();

    var chunk = try compile(code, &env, std.testing.allocator);
    defer chunk.deinit();

    try testing.expectEqualSlices(Inst, &.{
        // load 32 into reg 1
        Inst.init(.load, .{ .r = 1, .u = 0 }),
        // asign the the symbol in const[r] the value in reg1
        Inst.init(.define_global, .{ .u = 1, .r = 1 }),
        // return nothing
        Inst.init(.ret, .{ .r = 0 }),
    }, chunk.code[0..chunk.n_inst]);
}

test "get global" {
    const code =
        \\global
    ;

    var env = Env.init(testing.allocator);
    defer env.deinit();

    var chunk = try compile(code, &env, std.testing.allocator);
    defer chunk.deinit();

    try testing.expectEqualSlices(Inst, &.{
        // get the global
        Inst.init(.get_global, .{ .r = 1, .u = 0 }),
        // return nothing
        Inst.init(.ret, .{ .r = 1 }),
    }, chunk.code[0..chunk.n_inst]);
}

test "set global" {
    const code =
        \\(set! global 32)
    ;

    var env = Env.init(testing.allocator);
    defer env.deinit();

    var chunk = try compile(code, &env, std.testing.allocator);
    defer chunk.deinit();

    try testing.expectEqualSlices(Inst, &.{
        // load 32 into reg 1
        Inst.init(.load, .{ .r = 1, .u = 0 }),
        // asign the the symbol in const[r] the value in reg1
        Inst.init(.set_global, .{ .u = 1, .r = 1 }),
        // return nothing
        Inst.init(.ret, .{ .r = 0 }),
    }, chunk.code[0..chunk.n_inst]);
}
