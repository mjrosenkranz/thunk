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

// TODO: use these
const CompileErrors = error{
    ChunkError,
    // parsing
    ExpectedEOF,
    ExpectedSymbol,
    // syntax
    ExpectedClosingParen,
    ExpectedOpeningParen,
    InvalidCharacter,
};

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

        try self.expr();

        // TODO: eof
        try self.parser.consume(.eof, error.ExpectedEOF);

        return false;
    }

    /// parses/compiles an expression
    fn expr(self: *Self) anyerror!void {
        switch (self.parser.curr.tag) {
            .symbol => {
                // we have nothing to do for symbols rn
                try self.parser.consume(.symbol, error.ExpectedSymbol);
            },
            .number => {
                try self.parser.consume(.number, error.ExpectedNumber);
                _ = try self.chunk.pushImm(try self.parser.float());
            },
            .lparen => {
                self.parser.advance();
                try self.list();
            },
            .plus => {
                self.parser.advance();
                std.debug.print("compile-todo: plus\n", .{});
            },
            .eof => return,
            else => {
                std.debug.print("unexpected tag {}\n", .{self.parser.curr.tag});
                return error.ExpectedExpression;
            },
        }
    }

    /// parses/compiles a non quoted list ()
    fn list(self: *Self) anyerror!void {
        while (true) {
            if (self.parser.curr.tag == .rparen or self.parser.curr.tag == .eof) {
                try self.parser.consume(.rparen, error.ExpectedClosingParen);
                return;
            }
            try self.expr();
        }
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
        self.curr = self.scanner.next();
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
        const str = self.scanner.buf[self.prev.loc.start..self.prev.loc.end];
        const f = std.fmt.parseFloat(f32, str) catch |err| {
            std.debug.print("not float: {s}\n", .{str});
            return err;
        };
        return Value{ .float = f };
    }
};

const eps = std.math.epsilon(f32);

test "compile number literal" {
    const code =
        \\125.02
    ;
    var chunk = Chunk{};
    var compiler = Compiler{};
    _ = try compiler.compile(code, &chunk);
    try testing.expectApproxEqAbs(@as(f32, 125.02), chunk.imms[0].float, eps);
}

test "parse symbol" {
    // TODO: currently, nothing is compiled with symbols
    const code =
        \\symbol
    ;

    var chunk = Chunk{};
    var compiler = Compiler{};
    _ = try compiler.compile(code, &chunk);
}

test "simple expression" {
    const code =
        \\(125)
    ;
    var chunk = Chunk{};
    var compiler = Compiler{};
    _ = try compiler.compile(code, &chunk);
    try testing.expectApproxEqAbs(@as(f32, 125), chunk.imms[0].float, eps);
}

test "no closing" {
    const code =
        \\(125
    ;

    var chunk = Chunk{};
    var compiler = Compiler{};
    try testing.expectError(CompileErrors.ExpectedClosingParen, compiler.compile(code, &chunk));
}

test "no opening" {
    const code =
        \\125)
    ;

    var chunk = Chunk{};
    var compiler = Compiler{};
    try testing.expectError(CompileErrors.ExpectedEOF, compiler.compile(code, &chunk));
}

test "empty expression" {
    const code =
        \\()
    ;
    var chunk = Chunk{};
    var compiler = Compiler{};
    _ = try compiler.compile(code, &chunk);
    const code2 =
        \\(() () (()))
    ;

    chunk = Chunk{};
    compiler = Compiler{};
    _ = try compiler.compile(code2, &chunk);
}

test "plus expression" {
    const code =
        \\(+ 125 13)
    ;
    var chunk = Chunk{};
    var compiler = Compiler{};
    _ = try compiler.compile(code, &chunk);
    try testing.expectApproxEqAbs(@as(f32, 125), chunk.imms[0].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 13), chunk.imms[1].float, eps);
}
