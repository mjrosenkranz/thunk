const std = @import("std");
const testing = std.testing;
const Chunk = @import("chunk.zig").Chunk;
const inst = @import("inst.zig");
const Inst = inst.Inst;
const Reg = inst.Reg;
const Value = @import("value.zig").Value;
const scan = @import("scanner.zig");
const Scanner = scan.Scanner;
const Tag = scan.Tag;
const Token = scan.Token;
const Loc = scan.Loc;
const env = @import("env.zig");

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
    // compiling
    OutOfRegisters,
};

/// Takes our source code and compiles it to bytecode for the vm
pub const Compiler = struct {
    const Self = @This();

    /// use a parser to figure out what to do with them tokens
    parser: Parser = undefined,
    /// the chunk we are currently working on
    chunk: *Chunk = undefined,

    /// register allocator
    last_reg: Reg = 0,

    fn allocReg(self: *Self) !Reg {
        if (self.last_reg == 255) return CompileErrors.OutOfRegisters;

        self.last_reg += 1;
        return self.last_reg;
    }

    pub fn compile(self: *Self, src: []const u8, chunk: *Chunk) !bool {
        self.parser = Parser.init(src);
        self.chunk = chunk;

        _ = try self.expr();

        // TODO: eof
        try self.parser.consume(.eof, error.ExpectedEOF);

        // add a return
        _ = try chunk.pushInst(.{ .ret = .{} });

        return false;
    }

    /// parses/compiles an expression
    fn expr(self: *Self) anyerror!Reg {
        var reg: Reg = 0;
        switch (self.parser.curr.tag) {
            .symbol => {
                // we have nothing to do for symbols rn
                try self.parser.consume(.symbol, error.ExpectedSymbol);
            },
            .number => {
                try self.parser.consume(.number, error.ExpectedNumber);
                const idx = try self.chunk.pushImm(try self.parser.float());
                // allocate registor for the number
                reg = try self.allocReg();

                _ = try self.chunk.pushInst(Inst{
                    .load = .{
                        .r = reg,
                        .u = idx,
                    },
                });
            },
            .lparen => {
                self.parser.advance();
                reg = try self.list();
            },
            .plus => {
                // TODO: should be able to safely discard all registers here
                // since this expression is immediate
                self.parser.advance();
                reg = try self.binop(.{ .add = .{} });
            },
            .minus => {
                self.parser.advance();
                reg = try self.binop(.{ .sub = .{} });
            },
            .asterisk => {
                self.parser.advance();
                reg = try self.binop(.{ .mul = .{} });
            },
            .slash => {
                self.parser.advance();
                reg = try self.binop(.{ .div = .{} });
            },
            .eof => {},
            else => {
                std.debug.print("unexpected tag {}\n", .{self.parser.curr.tag});
                return error.ExpectedExpression;
            },
        }

        return reg;
    }

    fn binop(self: *Self, op: Inst) anyerror!Reg {
        var bop: Inst = op;

        const r1 = try self.expr();
        const r2 = try self.expr();
        const reg = try self.allocReg();
        try switch (bop) {
            .add,
            .sub,
            .mul,
            .div,
            => |*i| {
                i.* = .{
                    .r = reg,
                    .r1 = r1,
                    .r2 = r2,
                };
            },
            else => error.InvalidBinOpInst,
        };
        _ = try self.chunk.pushInst(bop);
        return reg;
    }

    /// parses/compiles a non quoted list ()
    fn list(self: *Self) anyerror!Reg {
        var last: Reg = 0;
        while (true) {
            if (self.parser.curr.tag == .rparen or self.parser.curr.tag == .eof) {
                try self.parser.consume(.rparen, error.ExpectedClosingParen);
                return last;
            }
            last = try self.expr();
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

    try testing.expectEqualSlices(Inst, &.{
        .{ .load = .{ .r = 1, .u = 0 } },
        .{ .load = .{ .r = 2, .u = 1 } },
        .{ .add = .{ .r = 3, .r1 = 1, .r2 = 2 } },
        .{ .ret = .{} },
    }, chunk.code[0..chunk.n_inst]);
}

test "minus expression" {
    const code =
        \\(- 3 13)
    ;
    var chunk = Chunk{};
    var compiler = Compiler{};
    _ = try compiler.compile(code, &chunk);
    try testing.expectApproxEqAbs(@as(f32, 3), chunk.imms[0].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 13), chunk.imms[1].float, eps);

    try testing.expectEqualSlices(Inst, &.{
        .{ .load = .{ .r = 1, .u = 0 } },
        .{ .load = .{ .r = 2, .u = 1 } },
        .{ .sub = .{ .r = 3, .r1 = 1, .r2 = 2 } },
        .{ .ret = .{} },
    }, chunk.code[0..chunk.n_inst]);
}

test "mul expression" {
    const code =
        \\(* 3 13)
    ;
    var chunk = Chunk{};
    var compiler = Compiler{};
    _ = try compiler.compile(code, &chunk);
    try testing.expectApproxEqAbs(@as(f32, 3), chunk.imms[0].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 13), chunk.imms[1].float, eps);

    try testing.expectEqualSlices(Inst, &.{
        .{ .load = .{ .r = 1, .u = 0 } },
        .{ .load = .{ .r = 2, .u = 1 } },
        .{ .mul = .{ .r = 3, .r1 = 1, .r2 = 2 } },
        .{ .ret = .{} },
    }, chunk.code[0..chunk.n_inst]);
}

test "div expression" {
    const code =
        \\(/ 3 13)
    ;
    var chunk = Chunk{};
    var compiler = Compiler{};
    _ = try compiler.compile(code, &chunk);
    try testing.expectApproxEqAbs(@as(f32, 3), chunk.imms[0].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 13), chunk.imms[1].float, eps);

    try testing.expectEqualSlices(Inst, &.{
        .{ .load = .{ .r = 1, .u = 0 } },
        .{ .load = .{ .r = 2, .u = 1 } },
        .{ .div = .{ .r = 3, .r1 = 1, .r2 = 2 } },
        .{ .ret = .{} },
    }, chunk.code[0..chunk.n_inst]);
}
