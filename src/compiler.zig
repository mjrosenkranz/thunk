const std = @import("std");
const testing = std.testing;
const Chunk = @import("chunk.zig").Chunk;
const Inst = @import("inst.zig").Inst;
const Value = @import("value.zig").Value;
const scan = @import("scanner.zig");
const Scanner = scan.Scanner;
const Tag = scan.Tag;
const Token = scan.Token;
const Loc = scan.Loc;

/// Takes our source code and compiles it to bytecode for the vm
pub const Compiler = struct {
    const Self = @This();

    /// use a parser to figure out what to do with them tokens
    parser: Parser = undefined,
    /// the chunk we are currently working on
    chunk: *Chunk = undefined,

    pub fn compile(self: *Self, src: []const u8, chunk: *Chunk) !bool {
        self.parser = Parser.init(src);
        self.chunk = chunk;

        self.parser.advance();
        try self.expression();

        // TODO: eof
        // try self.parser.consume(.eof, error.ExpectedExpression);

        return false;
    }

    pub fn expression(self: *Self) !void {
        // basically only handle constants rn
        try self.parser.consume(.number, error.ExpectedFloat);
        // if it's consumed that means we can add an immediate constant
        _ = try self.chunk.pushImm(try self.parser.float());

        // return error.ExpectedExpression;
    }
};

pub const Parser = struct {
    const Self = @This();

    /// use a scanner get all the tokens out of the code
    scanner: Scanner,
    prev: Token,
    curr: Token,

    pub fn init(src: []const u8) Self {
        var self: Self = undefined;
        self.scanner = Scanner.init(src);
        self.advance();
        return self;
    }

    /// gets us the next token in current and saves the previous
    pub fn advance(self: *Self) void {
        self.prev = self.curr;
        if (self.scanner.next()) |n| {
            self.curr = n;
        }
    }

    /// we expect the current token to have tag, if not we throw err
    pub fn consume(self: *Self, tag: Tag, err: anyerror) !void {
        if (self.curr.tag == tag) {
            self.advance();
            return;
        }
        return err;
    }

    /// creates a float value from current state
    pub fn float(
        self: *Self,
    ) !Value {
        const f = try std.fmt.parseFloat(
            f32,
            self.scanner.buf[self.prev.loc.start..self.prev.loc.end],
        );
        return Value{ .float = f };
    }
};

const eps = std.math.epsilon(f32);

test "compile number literal" {
    const code =
        \\125
    ;
    var chunk = Chunk{};
    var compiler = Compiler{};
    _ = try compiler.compile(code, &chunk);
    try testing.expectApproxEqAbs(@as(f32, 125.0), chunk.imms[0].float, eps);
}

test "fail to parse number literal" {
    const code =
        \\symbol
    ;

    var chunk = Chunk{};
    var compiler = Compiler{};
    try testing.expectError(error.ExpectedFloat, compiler.compile(code, &chunk));
}
