const std = @import("std");
const testing = std.testing;
const Chunk = @import("chunk.zig").Chunk;
const inst = @import("inst.zig");
const Inst = inst.Inst;
const Arg3 = inst.Arg3;
const Reg = inst.Reg;
const Value = @import("value.zig").Value;
const String = @import("value.zig").String;
const scan = @import("scanner.zig");
const Scanner = scan.Scanner;
const Tag = scan.Tag;
const Token = scan.Token;
const Loc = scan.Loc;
const Env = @import("env.zig").Env;

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
    // language
    ExpectedSubexpression,
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

    /// global environment
    global_env: *Env = undefined,

    fn allocReg(self: *Self) !Reg {
        if (self.last_reg == 255) return CompileErrors.OutOfRegisters;

        self.last_reg += 1;
        return self.last_reg;
    }

    pub fn compile(
        self: *Self,
        src: []const u8,
        chunk: *Chunk,
        env: *Env,
    ) !bool {
        self.parser = Parser.init(src);
        self.chunk = chunk;
        self.global_env = env;

        while (!self.parser.match(.eof)) {
            _ = try self.statement();
        }

        try self.parser.consume(.eof, error.ExpectedEOF);

        // add a return
        _ = try chunk.pushInst(Inst.init(.ret, .{}));
        return false;
    }

    // TODO: this should be maybe register
    fn statement(self: *Self) anyerror!Reg {
        if (self.parser.match(.lparen)) {
            self.parser.advance();
        } else {
            // otherwise pass on to expression
            return try self.expr();
        }

        // if we have a keyword that is a statement, do that
        switch (self.parser.curr.tag) {
            .define => {
                self.parser.advance();
                // TODO: lambda def list
                // next must be symbol
                try self.parser.consume(.symbol, error.ExpectedSymbol);
                // record symbol in const

                // TODO: intern the strings!
                const str = self.parser.prev.loc.slice;

                const str_idx = try self.chunk.pushConstStr(str);
                // get reg from following expr
                _ = try self.chunk.pushInst(Inst.init(
                    .define_global,
                    .{
                        .r = try self.expr(),
                        .u = str_idx,
                    },
                ));

                // create a slot in the global env for the variable
                var res = try self.global_env.map.getOrPut(str);
                if (!res.found_existing) {
                    res.value_ptr.* = Value.empty;
                }
            },
            // same as above
            .set => {
                self.parser.advance();
                // next must be symbol
                try self.parser.consume(.symbol, error.ExpectedSymbol);

                const str = self.parser.prev.loc.slice;

                // TODO: intern the strings!
                const str_idx = try self.chunk.pushConstStr(str);
                _ = try self.chunk.pushInst(Inst.init(
                    .set_global,
                    .{
                        .r = try self.expr(),
                        .u = str_idx,
                    },
                ));
            },
            else => return self.list(),
        }

        try self.parser.consume(.rparen, error.ExpectedClosingParen);

        return 0;
    }

    /// parses/compiles an expression
    fn expr(self: *Self) anyerror!Reg {
        var reg: Reg = 0;
        switch (self.parser.curr.tag) {
            // literals
            .symbol => {
                // we have nothing to do for symbols rn
                try self.parser.consume(.symbol, error.ExpectedSymbol);
            },
            .@"true" => {
                try self.parser.consume(.@"true", error.ExpectedTrue);
                const idx = try self.chunk.pushConst(.{ .boolean = true });
                // allocate registor for the number
                reg = try self.allocReg();

                _ = try self.chunk.pushInst(Inst.init(
                    .load,
                    .{
                        .r = reg,
                        .u = idx,
                    },
                ));
            },
            .@"false" => {
                try self.parser.consume(.@"false", error.ExpectedFalse);
                const idx = try self.chunk.pushConst(.{ .boolean = false });
                // allocate registor for the number
                reg = try self.allocReg();

                _ = try self.chunk.pushInst(Inst.init(
                    .load,
                    .{
                        .r = reg,
                        .u = idx,
                    },
                ));
            },
            .number => {
                try self.parser.consume(.number, error.ExpectedNumber);
                const idx = try self.chunk.pushConst(try self.parser.float());
                // allocate registor for the number
                reg = try self.allocReg();

                _ = try self.chunk.pushInst(Inst.init(
                    .load,
                    .{
                        .r = reg,
                        .u = idx,
                    },
                ));
            },

            // conditionals
            .@"if" => {
                self.parser.advance();
                // register the result is stored in
                reg = try self.allocReg();

                // get test expr
                const tst = try self.expr();

                // add test
                _ = try self.chunk.pushInst(
                    Inst.init(.eq_true, .{ .r = tst }),
                );

                // jump over then expr
                const thn_jump_idx = try self.chunk.pushInst(
                    Inst.init(.jmp, 0),
                );

                // load then expr
                // TODO: catch error if expr no exist
                const thn = try self.expr();
                // move the then branch
                const thn_end_idx = try self.chunk.pushInst(
                    Inst.init(.move, .{
                        .r = reg,
                        .r1 = thn,
                    }),
                );

                // jump over the number of instructions in the thn branch
                var thn_jump_amt = @intCast(inst.ArgI, thn_end_idx - thn_jump_idx);

                // if current is not a paren, then there is an else expression
                if (!self.parser.match(.rparen)) {
                    // add to the thn jump this jump instr
                    thn_jump_amt += 1;
                    const els_jump_idx = try self.chunk.pushInst(
                        Inst.init(.jmp, 0),
                    );
                    // add a jump for
                    const els = try self.expr();
                    // move the else branch
                    const els_end_idx = try self.chunk.pushInst(
                        Inst.init(.move, .{
                            .r = reg,
                            .r1 = els,
                        }),
                    );

                    self.chunk.code[els_jump_idx].data = @intCast(inst.ArgI, els_end_idx - els_jump_idx);
                }

                self.chunk.code[thn_jump_idx].data = thn_jump_amt;
            },

            // TODO: these should be builtin proceedures
            .plus => {
                // TODO: should be able to safely discard all registers here
                // since this expression is constediate
                self.parser.advance();
                reg = try self.binop(.add);
            },
            .minus => {
                self.parser.advance();
                reg = try self.binop(.sub);
            },
            .asterisk => {
                self.parser.advance();
                reg = try self.binop(.mul);
            },
            .slash => {
                self.parser.advance();
                reg = try self.binop(.div);
            },
            // delimiters
            .lparen => {
                self.parser.advance();
                reg = try self.list();
            },
            .rparen => {
                // rparen should be taken care of by list
                return CompileErrors.ExpectedEOF;
            },
            .eof => {},

            else => {
                std.debug.print("unexpected tag {}\n", .{self.parser.curr.tag});
                return error.ExpectedExpression;
            },
        }

        return reg;
    }

    fn binop(self: *Self, op: inst.Op) anyerror!Reg {
        const r1 = try self.expr();
        const r2 = try self.expr();
        const reg = try self.allocReg();

        try switch (op) {
            .add,
            .sub,
            .mul,
            .div,
            => {
                _ = try self.chunk.pushInst(.{
                    .op = op,
                    .data = @bitCast(inst.ArgSize, Arg3{
                        .r = reg,
                        .r1 = r1,
                        .r2 = r2,
                    }),
                });
                return reg;
            },
            else => return error.InvalidBinOpInst,
        };
    }

    /// parses/compiles a non quoted list ()
    fn list(self: *Self) anyerror!Reg {
        // if the next value is a right paren then this is invalid
        if (self.parser.match(.rparen)) {
            return CompileErrors.ExpectedSubexpression;
        }
        var last: Reg = 0;
        while (true) {
            if (self.parser.match(.rparen) or self.parser.match(.eof)) {
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

    /// shorthand for if the current token tag is expected
    pub inline fn match(self: Self, tag: Tag) bool {
        return self.curr.tag == tag;
    }

    /// we expect the current token to have tag, if not we throw err
    pub fn consume(self: *Self, tag: Tag, err: anyerror) !void {
        if (self.match(tag)) {
            self.advance();
            return;
        }
        return err;
    }

    /// creates a float value from current state
    pub fn float(
        self: *Self,
    ) !Value {
        const str = self.prev.loc.slice;
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
    var env = Env.init(testing.allocator);
    defer env.deinit();
    _ = try compiler.compile(code, &chunk, &env);
    try testing.expectApproxEqAbs(@as(f32, 125.02), chunk.consts[0].float, eps);
}

test "parse symbol" {
    // TODO: currently, nothing is compiled with symbols
    const code =
        \\symbol
    ;

    var chunk = Chunk{};
    var compiler = Compiler{};
    var env = Env.init(testing.allocator);
    defer env.deinit();
    _ = try compiler.compile(code, &chunk, &env);
}

test "simple expression" {
    const code =
        \\(125)
    ;
    var chunk = Chunk{};
    var compiler = Compiler{};
    var env = Env.init(testing.allocator);
    defer env.deinit();
    _ = try compiler.compile(code, &chunk, &env);
    try testing.expectApproxEqAbs(@as(f32, 125), chunk.consts[0].float, eps);
}

test "no closing" {
    const code =
        \\(125
    ;

    var chunk = Chunk{};
    var compiler = Compiler{};
    var env = Env.init(testing.allocator);
    defer env.deinit();
    try testing.expectError(CompileErrors.ExpectedClosingParen, compiler.compile(code, &chunk, &env));
}

test "too many closing" {
    const code =
        \\(125))
    ;

    var chunk = Chunk{};
    var compiler = Compiler{};
    var env = Env.init(testing.allocator);
    defer env.deinit();
    try testing.expectError(CompileErrors.ExpectedEOF, compiler.compile(code, &chunk, &env));
}

test "no opening" {
    const code =
        \\125)
    ;

    var chunk = Chunk{};
    var compiler = Compiler{};
    var env = Env.init(testing.allocator);
    defer env.deinit();
    try testing.expectError(CompileErrors.ExpectedEOF, compiler.compile(code, &chunk, &env));
}

test "too many closing" {
    const code =
        \\((125)
    ;

    var chunk = Chunk{};
    var compiler = Compiler{};
    var env = Env.init(testing.allocator);
    defer env.deinit();
    try testing.expectError(CompileErrors.ExpectedClosingParen, compiler.compile(code, &chunk, &env));
}

test "empty expression" {
    const code =
        \\()
    ;
    var chunk = Chunk{};
    var compiler = Compiler{};
    var env = Env.init(testing.allocator);
    defer env.deinit();
    try testing.expectError(CompileErrors.ExpectedSubexpression, compiler.compile(code, &chunk, &env));
}

test "plus expression" {
    const code =
        \\(+ 125 13)
    ;
    var chunk = Chunk{};
    var compiler = Compiler{};
    var env = Env.init(testing.allocator);
    defer env.deinit();
    _ = try compiler.compile(code, &chunk, &env);
    try testing.expectApproxEqAbs(@as(f32, 125), chunk.consts[0].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 13), chunk.consts[1].float, eps);

    try testing.expectEqualSlices(Inst, &.{
        Inst.init(.load, .{ .r = 1, .u = 0 }),
        Inst.init(.load, .{ .r = 2, .u = 1 }),
        Inst.init(.add, .{ .r = 3, .r1 = 1, .r2 = 2 }),
        Inst.init(.ret, .{}),
    }, chunk.code[0..chunk.n_inst]);
}

test "minus expression" {
    const code =
        \\(- 3 13)
    ;
    var chunk = Chunk{};
    var compiler = Compiler{};
    var env = Env.init(testing.allocator);
    defer env.deinit();
    _ = try compiler.compile(code, &chunk, &env);
    try testing.expectApproxEqAbs(@as(f32, 3), chunk.consts[0].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 13), chunk.consts[1].float, eps);

    try testing.expectEqualSlices(Inst, &.{
        Inst.init(.load, .{ .r = 1, .u = 0 }),
        Inst.init(.load, .{ .r = 2, .u = 1 }),
        Inst.init(.sub, .{ .r = 3, .r1 = 1, .r2 = 2 }),
        Inst.init(.ret, .{}),
    }, chunk.code[0..chunk.n_inst]);
}

test "mul expression" {
    const code =
        \\(* 3 13)
    ;
    var chunk = Chunk{};
    var compiler = Compiler{};
    var env = Env.init(testing.allocator);
    defer env.deinit();
    _ = try compiler.compile(code, &chunk, &env);
    try testing.expectApproxEqAbs(@as(f32, 3), chunk.consts[0].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 13), chunk.consts[1].float, eps);

    try testing.expectEqualSlices(Inst, &.{
        Inst.init(.load, .{ .r = 1, .u = 0 }),
        Inst.init(.load, .{ .r = 2, .u = 1 }),
        Inst.init(.mul, .{ .r = 3, .r1 = 1, .r2 = 2 }),
        Inst.init(.ret, .{}),
    }, chunk.code[0..chunk.n_inst]);
}

test "div expression" {
    const code =
        \\(/ 3 13)
    ;
    var chunk = Chunk{};
    var compiler = Compiler{};
    var env = Env.init(testing.allocator);
    defer env.deinit();
    _ = try compiler.compile(code, &chunk, &env);
    try testing.expectApproxEqAbs(@as(f32, 3), chunk.consts[0].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 13), chunk.consts[1].float, eps);

    try testing.expectEqualSlices(Inst, &.{
        Inst.init(.load, .{ .r = 1, .u = 0 }),
        Inst.init(.load, .{ .r = 2, .u = 1 }),
        Inst.init(.div, .{ .r = 3, .r1 = 1, .r2 = 2 }),
        Inst.init(.ret, .{}),
    }, chunk.code[0..chunk.n_inst]);
}

test "bool expression" {
    const code =
        \\(/ #f #t)
    ;
    var chunk = Chunk{};
    var compiler = Compiler{};
    var env = Env.init(testing.allocator);
    defer env.deinit();
    _ = try compiler.compile(code, &chunk, &env);
    try testing.expect(false == chunk.consts[0].boolean);
    try testing.expect(true == chunk.consts[1].boolean);

    try testing.expectEqualSlices(Inst, &.{
        Inst.init(.load, .{ .r = 1, .u = 0 }),
        Inst.init(.load, .{ .r = 2, .u = 1 }),
        Inst.init(.div, .{ .r = 3, .r1 = 1, .r2 = 2 }),
        Inst.init(.ret, .{}),
    }, chunk.code[0..chunk.n_inst]);
}

test "define var" {
    const code =
        \\(define foo 13)
    ;

    var chunk = Chunk.init(testing.allocator);
    defer chunk.deinit();
    var compiler = Compiler{};
    var env = Env.init(testing.allocator);
    defer env.deinit();
    _ = try compiler.compile(code, &chunk, &env);
    try testing.expectEqualSlices(u8, chunk.consts[0].string.slice(), "foo");
    try testing.expect(chunk.consts[1].float == 13);
    try testing.expectEqualSlices(Inst, &.{
        Inst.init(.load, .{ .r = 1, .u = 1 }),
        Inst.init(.define_global, .{ .u = 0, .r = 1 }),
        Inst.init(.ret, .{}),
    }, chunk.code[0..chunk.n_inst]);

    try testing.expect(env.map.get("foo") != null);
}

test "define var" {
    const code =
        \\(define foo 13)
        \\(set! foo 43)
    ;

    var chunk = Chunk.init(testing.allocator);
    defer chunk.deinit();
    var compiler = Compiler{};
    var env = Env.init(testing.allocator);
    defer env.deinit();
    _ = try compiler.compile(code, &chunk, &env);
    try testing.expectEqualSlices(u8, chunk.consts[0].string.slice(), "foo");
    try testing.expect(chunk.consts[1].float == 13);
    try testing.expectEqualSlices(u8, chunk.consts[2].string.slice(), "foo");
    try testing.expect(chunk.consts[3].float == 43);
    try testing.expectEqualSlices(Inst, &.{
        Inst.init(.load, .{ .r = 1, .u = 1 }),
        Inst.init(.define_global, .{ .u = 0, .r = 1 }),
        Inst.init(.load, .{ .r = 2, .u = 3 }),
        Inst.init(.set_global, .{ .u = 2, .r = 2 }),
        Inst.init(.ret, .{}),
    }, chunk.code[0..chunk.n_inst]);

    try testing.expect(env.map.get("foo") != null);
}

test "if statement" {
    const code =
        \\(if #t 12 -3)
    ;
    var chunk = Chunk{};
    var compiler = Compiler{};
    var env = Env.init(testing.allocator);
    defer env.deinit();
    _ = try compiler.compile(code, &chunk, &env);
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

test "if statement no else" {
    const code =
        \\(if #t 12)
    ;
    var chunk = Chunk{};
    var compiler = Compiler{};
    var env = Env.init(testing.allocator);
    defer env.deinit();
    _ = try compiler.compile(code, &chunk, &env);
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
        //-----------------------------

        Inst.init(.ret, .{}),
    }, chunk.code[0..chunk.n_inst]);
}
