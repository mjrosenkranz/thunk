const std = @import("std");
const testing = std.testing;
const inst = @import("inst.zig");
const Inst = inst.Inst;
const Reg = inst.Reg;
const Env = @import("env.zig").Env;
const Chunk = @import("chunk.zig").Chunk;
const Value = @import("value.zig").Value;
const Allocator = std.mem.Allocator;
const Parser = @import("parser.zig");
const Ast = @import("ast.zig");
const NodeIdx = Ast.NodeIdx;
const Node = Ast.Node;

pub const CompileError = error{
    OutOfRegisters,
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

    var chunk = Chunk{};
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

    /// produces a new chunk from the Ast given
    /// we can assume that the Ast is for a single, top-level expression
    /// and therefore will only produce one chunk
    pub fn compile(self: *Compiler) CompileError!void {
        // start with the root
        const root = self.ast.nodes[0];
        try self.compileNode(root.children.l);

        // push return for for the last allocated register?
        _ = try self.chunk.pushInst(Inst.init(.ret, .{
            //.r = self.last_reg,
            //.r1 = self.last_reg,
        }));
    }

    pub fn compileNode(
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
            .@"if" => try self.compileCond(idx),
            else => return CompileError.NotYetImplemented,
        }
    }

    pub fn compileCond(
        self: *Compiler,
        if_idx: NodeIdx,
    ) CompileError!void {
        // where we will store the result
        const res = try self.allocReg();

        const if_node = self.ast.nodes[if_idx];
        // compile cond
        try self.compileNode(if_idx + 1);
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
        try self.compileNode(if_node.children.l);
        // move the then branch
        const thn_end_idx = try self.chunk.pushInst(
            Inst.init(.move, .{
                .r = res,
                // the result of then should be in the latest register?
                .r1 = self.last_reg,
            }),
        );

        var thn_jump_amt = @intCast(inst.ArgI, thn_end_idx - thn_jump_idx);

        // TODO: check if we have an else
        if (if_node.children.r != 0) {
            // compile else
            thn_jump_amt += 1;
            const els_jump_idx = try self.chunk.pushInst(
                Inst.init(.jmp, 0),
            );
            try self.compileNode(if_node.children.r);
            // move the else branch
            const els_end_idx = try self.chunk.pushInst(
                Inst.init(.move, .{
                    .r = res,
                    .r1 = self.last_reg,
                }),
            );
            self.chunk.code[els_jump_idx].data = @intCast(inst.ArgI, els_end_idx - els_jump_idx);
        }

        // patch the jump instruction
        self.chunk.code[thn_jump_idx].data = thn_jump_amt;
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
    var compiler = Compiler{
        .chunk = &chunk,
        .ast = &ast,
    };
    try compiler.compile();

    try std.testing.expectApproxEqAbs(@as(f32, 32), chunk.consts[0].float, eps);

    std.debug.print("\n", .{});
    chunk.disassemble();
}

test "if statement" {
    const code =
        \\(if #t 12 -3)
    ;
    var env = Env.init(testing.allocator);
    defer env.deinit();
    var chunk = try compile(code, &env, std.testing.allocator);

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
        Inst.init(.ret, .{}),
    }, chunk.code[0..chunk.n_inst]);
}

test "if no else" {
    const code =
        \\(if #t 12)
    ;
    var env = Env.init(testing.allocator);
    defer env.deinit();
    var chunk = try compile(code, &env, std.testing.allocator);

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
        Inst.init(.ret, .{}),
    }, chunk.code[0..chunk.n_inst]);
}
