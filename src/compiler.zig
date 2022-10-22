const std = @import("std");
const testing = std.testing;
const Chunk = @import("chunk.zig").Chunk;
const Inst = @import("inst.zig").Inst;
const Value = @import("value.zig").Value;
const scanner = @import("scanner.zig");
const Scanner = scanner.Scanner;
const Tag = scanner.Tag;
const Token = scanner.Token;
const Loc = scanner.Loc;

/// Takes our source code and compiles it to bytecode for the vm
pub const Compiler = struct {
    const Self = @This();

    /// use a scanner get all the tokens out of the code
    scanner: Scanner,

    /// use a parser to figure out what to do with them tokens
    parser: Parser,

    pub fn compile(self: *Self, src: []const u8) ?Chunk {
        self.scanner = Scanner.init(src);
        self.parser = .{};
    }
};

pub const Parser = struct {
    prev: Token,
    cur: Token,

    pub fn float(loc: Loc, src: []const u8) !f32 {
        return try std.fmt.parseFloat(f32, src[loc.start..loc.end]);
    }
};

const eps = std.math.epsilon(f32);
test "parse number literal" {
    const code =
        \\125
    ;
    var scan = Scanner.init(code);
    const t = scan.next().?;
    try testing.expect(t.tag == .number);
    try testing.expectApproxEqAbs(@as(f32, 125.0), try Parser.float(t.loc, code), eps);
}
