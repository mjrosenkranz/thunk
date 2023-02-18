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
const LocalList = @import("locals.zig").LocalList;

pub const CompileError = error{
    OutOfRegisters,
    WrongNumberArguments,
    NotYetImplemented,
} || Chunk.ChunkError || Parser.ParseError || @import("assoc.zig").AssocError;

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

const Slot = union(enum) {
    reg: Reg,
    constant: u16,
};

pub const Compiler = struct {
    ast: *Ast,

    chunk: *Chunk,

    locals: LocalList = .{},

    /// the next free register in our reg allocator
    /// reg 0 is reserved to mean nothing I guess
    last_reg: Reg = 0,

    fn allocReg(self: *Compiler) !Reg {
        if (self.last_reg == 255) return CompileError.OutOfRegisters;
        self.last_reg += 1;
        return self.last_reg;
    }

    // TODO: need to be refactor the way constants and registers
    // are used so that we are not allocating registers for immediates
    // and instead only using them for locals
    fn freeReg(self: *Compiler) void {
        if (self.last_reg == 0) {
            std.debug.print("attempt to free last register\n", .{});
            return;
        }
        self.last_reg -= 1;
    }

    /// allocates a reg for an immediate or returns the
    /// register if it is just a register
    fn toReg(self: *Compiler, slot: Slot) !Reg {
        return switch (slot) {
            .constant => |c| blk: {
                const reg = try self.allocReg();
                _ = try self.chunk.pushInst(Inst.init(.load, .{
                    .r = reg,
                    .u = c,
                }));
                break :blk reg;
            },
            .reg => |reg| reg,
        };
    }

    /// places the constant in given register
    /// or moves value from local register to given
    /// returns the index of the instruction used
    fn putInReg(self: *Compiler, slot: Slot, dest: Reg) !usize {
        return switch (slot) {
            .constant => |c| try self.chunk.pushInst(Inst.init(.load, .{
                .r = dest,
                .u = c,
            })),
            .reg => |reg| try self.chunk.pushInst(
                Inst.init(.move, .{
                    .r = dest,
                    // the result of then should be in the latest register?
                    .r1 = reg,
                }),
            ),
        };
    }

    /// produces a new chunk from the Ast given
    /// we can assume that the Ast is for a single, top-level expression
    /// and therefore will only produce one chunk
    pub fn compile(self: *Compiler) CompileError!void {
        _ = try self.chunk.pushInst(Inst.init(
            .ret,
            .{ .r = (try self.evalNode(0)).reg },
        ));
    }

    fn evalNode(
        self: *Compiler,
        idx: NodeIdx,
    ) CompileError!Slot {
        const node = self.ast.nodes[idx];
        return switch (node.tag) {
            .constant => .{
                .constant = try self.chunk.pushConst(
                    self.ast.getData(Value, node.children.l),
                ),
            },
            // .define => try self.applyDefine(idx),
            // .set => try self.applySet(idx),
            // .let => try self.applyLet(idx),
            .seq => try self.evalSequence(idx),
            .@"if" => try self.applyCond(idx),
            .call => try self.applyNode(idx),
            .symbol => return try self.evalSymbol(idx),
            else => {
                std.debug.print("bad node[{}]: {}\n", .{ idx, node.tag });
                return CompileError.NotYetImplemented;
            },
        };
        // return .{ .reg = self.last_reg };
    }

    fn evalSymbol(
        self: *Compiler,
        idx: NodeIdx,
    ) CompileError!Slot {
        const symbol = self.ast.nodes[idx];
        // TOOD: intern strings so we don't have a string
        // pushed to the consts twice
        const symbol_tok = self.ast.tokens[symbol.token_idx];
        // first check if this is in our locals
        if (self.locals.get(symbol_tok)) |bind| {
            return .{ .reg = bind.reg };
        } else {
            std.debug.print("cannot find '{s}'\n", .{symbol_tok.loc.slice});
            return error.NotYetImplemented;
            // // get the string into a constant
            // const reg = try self.allocReg();
            // _ = try self.chunk.pushInst(Inst.init(
            //     .get_global,
            //     .{
            //         .r = try self.allocReg(),
            //         .u = try self.chunk.pushConstStr(
            //             self.ast.tokens[symbol.token_idx].loc.slice,
            //         ),
            //     },
            // ));

            // return .{ .reg = reg };
        }
    }

    fn evalSequence(
        self: *Compiler,
        idx: NodeIdx,
    ) CompileError!Slot {
        var seq_idx: NodeIdx = idx;

        while (true) {
            const seq = self.ast.nodes[seq_idx];
            const slot = try self.evalNode(seq.children.l);

            const reg = try self.toReg(slot);

            if (seq.children.r == 0) {
                return .{ .reg = reg };
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
    ) CompileError!Slot {
        // where we will store the result.
        // we don't the result register since it will be used outside of the if scope
        const res = try self.allocReg();
        // const res = self.last_reg;

        const if_node = self.ast.nodes[if_idx];
        // compile cond
        // we know that the result of compiling the condition should be in last reg
        const cond_reg = try self.toReg(try self.evalNode(if_idx + 1));
        // add test
        _ = try self.chunk.pushInst(
            Inst.init(.eq_true, .{ .r = cond_reg }),
        );
        // jump over then expr
        const thn_jump_idx = try self.chunk.pushInst(
            Inst.init(.jmp, 0),
        );
        // compile then
        const then_slot = try self.evalNode(if_node.children.l);
        // move the then branch
        const thn_end_idx = try self.putInReg(then_slot, res);

        var thn_jump_amt = @intCast(inst.ArgI, thn_end_idx - thn_jump_idx);

        if (if_node.children.r != 0) {
            // compile else
            thn_jump_amt += 1;
            const els_jump_idx = try self.chunk.pushInst(
                Inst.init(.jmp, 0),
            );
            const else_slot = try self.evalNode(if_node.children.r);
            // move the else branch
            const els_end_idx = try self.putInReg(else_slot, res);

            self.chunk.code[els_jump_idx].data = @intCast(inst.ArgI, els_end_idx - els_jump_idx);

            // free the else register
            self.freeReg();
        }

        // free the then register
        // self.freeReg();
        // free the cond register
        // self.freeReg();

        // patch the jump instruction
        self.chunk.code[thn_jump_idx].data = thn_jump_amt;

        return .{ .reg = res };
    }

    fn applyDefine(
        self: *Compiler,
        def_idx: NodeIdx,
    ) CompileError!void {
        const def_node = self.ast.nodes[def_idx];
        // get the value by evaluating
        const slot = try self.evalNode(def_node.children.r);

        // create a symbol by evaluating
        // TODO: make this use a symbol data type directly

        // copy symbol name with allocator
        const lhs = self.ast.nodes[def_node.children.l];
        switch (slot) {
            .constant => {
                _ = try self.chunk.pushInst(Inst.init(
                    .define_global,
                    .{
                        .r = self.allocReg(),
                        .u = try self.chunk.pushConstStr(
                            self.ast.tokens[lhs.token_idx].loc.slice,
                        ),
                    },
                ));
            },
        }
        // allocate a register for the value
        _ = try self.chunk.pushInst(Inst.init(
            .define_global,
            .{
                // .r = value_reg,
                .r = 0,
                .u = try self.chunk.pushConstStr(
                    self.ast.tokens[lhs.token_idx].loc.slice,
                ),
            },
        ));

        // pop the register used for value
        self.freeReg();
    }

    fn applyLet(
        self: *Compiler,
        let_idx: NodeIdx,
    ) CompileError!void {
        const let = self.ast.nodes[let_idx];
        const alist_idx = let.children.l;

        // consume let
        // add all bindings to the list (Local -> register)
        // this means first we evaluate the expr of each binidng
        // then we add the name o our stack with the current depth
        //
        // r = last_reg
        // (let ((x 33)
        //       (y 44))
        //      (let ((z 55))
        //           (+ x y z)))
        //
        // name reg depth
        //   x  r+1  1
        //   y  r+2  1
        //   z  r+3  2

        // node we getting the binding from
        var b_node = self.ast.nodes[alist_idx];
        while (true) {

            // (x 33)
            const binding = self.ast.nodes[b_node.children.l];

            // TODO: assert that this is a symbol?
            const variable = self.ast.nodes[binding.children.l];

            // compile the binding
            try self.locals.assoc(self.ast.tokens[variable.token_idx], .{
                .reg = try self.evalNode(binding.children.r),
                .depth = 0,
            });

            if (b_node.children.r == 0) break;
            b_node = self.ast.nodes[b_node.children.r];
        }

        // compile body
        _ = try self.evalNode(let.children.r);

        // TODO: pop all consumed registers
        // TODO: pop them from the assoc list as well
    }

    fn applySet(
        self: *Compiler,
        def_idx: NodeIdx,
    ) CompileError!void {
        const def_node = self.ast.nodes[def_idx];
        // get the value by evaluating
        const value_reg = try self.evalNode(def_node.children.r);

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
    ) CompileError!Slot {
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
    ) CompileError!Slot {
        // if we are using modulo then check arity
        if (caller_tag == .modulo and call_data.n_args != 2) {
            return CompileError.WrongNumberArguments;
        }

        if (call_data.n_args == 0) {
            return CompileError.WrongNumberArguments;
        }

        const res_reg = try self.allocReg();

        // evaluate each item in the list
        var pair_idx = args_idx;
        // load the first item into a register
        var pair = self.ast.nodes[pair_idx];

        const first_slot = try self.evalNode(pair.children.l);

        // regardless of what the first arg is, we need to put it
        // in the result register
        _ = try self.putInReg(first_slot, res_reg);

        pair_idx = pair.children.r;

        const const_inst = switch (caller_tag) {
            .plus,
            .minus,
            .asterisk,
            .slash,
            => true,
            else => false,
        };

        // var n_args_used: u8 = 0;

        // if the right child is 0 then we have reached the end of the list
        while (pair_idx != 0) {
            pair = self.ast.nodes[pair_idx];

            const arg_slot = try self.evalNode(pair.children.l);

            switch (arg_slot) {
                .constant => |c| {
                    // if this is an instruction that can use an immediate
                    // then we can use that version
                    if (const_inst) {
                        _ = try self.chunk.pushInst(.{
                            .op = Op.fromPrimitiveTokenTag(caller_tag, true),
                            .data = @bitCast(inst.ArgSize, inst.ArgU{
                                .r = res_reg,
                                .u = c,
                            }),
                        });
                    } else {
                        // otherwise, we allocate a register for it
                        const reg2 = try self.toReg(arg_slot);

                        _ = try self.chunk.pushInst(.{
                            .op = Op.fromPrimitiveTokenTag(caller_tag, false),
                            .data = @bitCast(inst.ArgSize, inst.Arg3{
                                .r = res_reg,
                                .r1 = res_reg,
                                .r2 = reg2,
                            }),
                        });
                    }
                },
                // otherwise, we use the normal instruction
                .reg => |r| {
                    _ = try self.chunk.pushInst(.{
                        .op = Op.fromPrimitiveTokenTag(caller_tag, false),
                        .data = @bitCast(inst.ArgSize, inst.Arg3{
                            .r = res_reg,
                            .r1 = res_reg,
                            .r2 = r,
                        }),
                    });
                },
            }

            pair_idx = pair.children.r;
        }

        return .{ .reg = res_reg };
    }

    fn applyBool(
        self: *Compiler,
        caller_tag: Token.Tag,
        call_data: FnCall,
        args_idx: NodeIdx,
    ) CompileError!Slot {
        if (call_data.n_args == 0) {
            return CompileError.WrongNumberArguments;
        }

        const res_reg = try self.allocReg();
        // evaluate each item in the list
        var pair_idx = args_idx;

        // load the first item into a register
        var pair = self.ast.nodes[pair_idx];
        const first_slot = try self.evalNode(pair.children.l);
        _ = try self.putInReg(first_slot, res_reg);

        pair_idx = pair.children.r;

        // if we are using not then we only need the first item
        if (caller_tag == .@"not") {
            if (call_data.n_args != 1) {
                return CompileError.WrongNumberArguments;
            }

            _ = try self.chunk.pushInst(
                Inst.init(.@"not", .{
                    .r = res_reg,
                    .r1 = res_reg,
                }),
            );

            return .{ .reg = res_reg };
        }

        // index where we start adding jumps
        const start_idx = self.chunk.n_inst - 1;

        const compare: Reg = switch (caller_tag) {
            .@"or" => 1,
            .@"and" => 0,
            else => unreachable,
        };

        while (pair_idx != 0) {
            // test the last value
            _ = try self.chunk.pushInst(
                Inst.init(.@"test", .{
                    .r = res_reg,
                    .r1 = res_reg,
                    .r2 = compare,
                }),
            );
            // get the index of the jump instruction
            _ = try self.chunk.pushInst(
                Inst.init(.jmp, 0),
            );

            // compile this value
            pair = self.ast.nodes[pair_idx];
            _ = try self.putInReg(try self.evalNode(pair.children.l), res_reg);

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

        // free reg, since we only used it in the test instruction
        // self.freeReg();

        return .{ .reg = res_reg };
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

    try testing.expectEqualSlices(Inst, &.{
        // load the number
        Inst.init(.load, .{ .r = 1, .u = 0 }),
        // return
        Inst.init(.ret, .{ .r = 1 }),
    }, chunk.code[0..chunk.n_inst]);
}

test "compile two number literals" {
    const code =
        \\32
        \\55
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
    try std.testing.expectApproxEqAbs(@as(f32, 55), chunk.consts[1].float, eps);

    try testing.expectEqualSlices(Inst, &.{
        // load the number
        Inst.init(.load, .{ .r = 1, .u = 0 }),
        // load the other number
        Inst.init(.load, .{ .r = 1, .u = 1 }),
        // return
        Inst.init(.ret, .{ .r = 1 }),
    }, chunk.code[0..chunk.n_inst]);
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
        Inst.init(.jmp, 2),
        // thn branch
        // load 12 into r3
        Inst.init(.load, .{ .r = 1, .u = 1 }),
        // jump over else branch
        Inst.init(.jmp, 1),
        //-----------------------------
        // else branch
        Inst.init(.load, .{ .r = 1, .u = 2 }),
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
        Inst.init(.jmp, 1),
        // thn branch
        // load 12 into r3
        Inst.init(.load, .{ .r = 1, .u = 1 }),
        // -----------------------
        // return
        Inst.init(.ret, .{ .r = 1 }),
    }, chunk.code[0..chunk.n_inst]);
}

test "if with var in cond" {
    const code =
        \\(if x 12)
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
    try compiler.locals.assoc(.{
        .tag = .symbol,
        .loc = .{
            .line = 1,
            .col = 5,
            .slice = code[4..5],
        },
    }, .{
        .reg = 55,
        .depth = 0,
    });
    try compiler.compile();

    try testing.expectEqualSlices(Inst, &.{
        // test if r1 is false (test skips over jump if false)
        Inst.init(.eq_true, .{ .r = 55 }),
        // jump over thn branch
        Inst.init(.jmp, 1),
        // thn branch
        // load 12 into r3
        Inst.init(.load, .{ .r = 1, .u = 0 }),
        // -----------------------
        // return
        Inst.init(.ret, .{ .r = 1 }),
    }, chunk.code[0..chunk.n_inst]);
}

test "if with var in then else" {
    const code =
        \\(if #f thn els)
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

    try compiler.locals.assoc(.{
        .tag = .symbol,
        .loc = .{
            .slice = code[7..10],
        },
    }, .{
        .reg = 55,
        .depth = 0,
    });

    try compiler.locals.assoc(.{
        .tag = .symbol,
        .loc = .{
            .slice = code[11..14],
        },
    }, .{
        .reg = 33,
        .depth = 0,
    });
    try compiler.compile();

    try testing.expectEqualSlices(Inst, &.{
        // load the test
        Inst.init(.load, .{ .r = 2, .u = 0 }),
        // test if r1 is false (test skips over jump if false)
        Inst.init(.eq_true, .{ .r = 2 }),
        // jump over thn branch
        Inst.init(.jmp, 2),
        // thn branch
        // load 12 into r3
        Inst.init(.move, .{ .r = 1, .r1 = 55 }),
        // jump over else branch
        Inst.init(.jmp, 1),
        //-----------------------------
        // else branch
        Inst.init(.move, .{ .r = 1, .r1 = 33 }),
        // -----------------------
        // return
        Inst.init(.ret, .{ .r = 1 }),
    }, chunk.code[0..chunk.n_inst]);
}

test "primitive call" {
    const code =
        \\(+ 1 2 x)
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

    try compiler.locals.assoc(.{
        .tag = .symbol,
        .loc = .{
            .slice = code[7..8],
        },
    }, .{
        .reg = 55,
        .depth = 0,
    });

    try compiler.compile();

    try testing.expectEqualSlices(Inst, &.{
        // load 1 into 1
        Inst.init(.load, .{ .r = 1, .u = 0 }),
        // add them together and store in 1
        Inst.init(.addconst, .{ .r = 1, .u = 1 }),
        // load 3 into 2 (we popped 2 off since we don't need it anymore)
        Inst.init(.add, .{ .r = 1, .r1 = 1, .r2 = 55 }),
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

test "locals" {
    const code =
        \\(let ((x 33)
        \\      (y 44))
        \\          (+ x y))
    ;
    var parser = Parser.init(testing.allocator);
    defer parser.deinit();

    var ast = try parser.parse(code);
    defer ast.deinit(testing.allocator);

    var chunk = Chunk.init(testing.allocator);
    var compiler = Compiler{
        .chunk = &chunk,
        .ast = &ast,
    };
    try compiler.compile();

    for (compiler.locals.keys[0..compiler.locals.idx]) |key, i| {
        std.debug.print("({s}, {})\n", .{
            key.loc.slice,
            compiler.locals.values[i].reg,
        });
    }

    chunk.disassemble();

    try testing.expectEqualSlices(Inst, &.{
        // load 33 into reg 1
        Inst.init(.load, .{ .r = 1, .u = 0 }),
        // load 44 into reg 2
        Inst.init(.load, .{ .r = 2, .u = 1 }),

        // add the two registers
        Inst.init(.add, .{ .r = 3, .r1 = 1, .r2 = 2 }),

        Inst.init(.ret, .{ .r = 0 }),
    }, chunk.code[0..chunk.n_inst]);
}
