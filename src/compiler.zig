const std = @import("std");
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
    next_reg: Reg = 0,

    fn allocReg(self: *Compiler) !Reg {
        if (self.next_reg == 255) return CompileError.OutOfRegisters;
        const reg = self.next_reg;
        self.next_reg += 1;
        return reg;
    }

    /// produces a new chunk from the Ast given
    /// we can assume that the Ast is for a single, top-level expression
    /// and therefore will only produce one chunk
    pub fn compile(self: *Compiler) CompileError!void {
        // start with the root
        const root = self.ast.nodes[0];
        try self.compileNode(self.ast.nodes[root.children.l]);

        // push return for for the last allocated register?
        _ = try self.chunk.pushInst(Inst.init(.ret, .{
            .r = self.next_reg - 1,
            .r1 = self.next_reg - 1,
        }));
    }

    pub fn compileNode(
        self: *Compiler,
        node: Node,
    ) CompileError!void {
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
            else => return CompileError.NotYetImplemented,
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
    var compiler = Compiler{
        .chunk = &chunk,
        .ast = &ast,
    };
    try compiler.compile();

    try std.testing.expectApproxEqAbs(@as(f32, 32), chunk.consts[0].float, eps);

    std.debug.print("\n", .{});
    chunk.disassemble();
}
