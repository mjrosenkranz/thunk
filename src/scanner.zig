const std = @import("std");
const Token = @import("token.zig");
const SrcLoc = @import("file.zig").SrcLoc;
const Tag = Token.Tag;
const testing = std.testing;

pub const Scanner = struct {
    const Self = @This();

    const State = enum {
        start,
        number,
        symbol,
        keyword,
        identifier,
        comment,
        // starts with a symbol that may be
        // a number
        maybe_number,
        period,
        sharp,

        // we may be in a gt or a gte
        greater,
        less,
    };

    /// where are we?
    offset: usize = 0,
    /// where this token started
    start: usize = 0,

    /// current line
    line: u32 = 1,
    /// current column
    col: u32 = 1,
    /// buffer we are tokenizing
    buf: []const u8,

    pub fn next(self: *Self) Token {
        self.start = self.offset;

        var tok = Token{
            .tag = .eof,
            .loc = .{
                .line = self.line,
                .col = self.col,
            },
        };

        if (self.offset >= self.buf.len)
            return tok;

        var state: State = .start;
        while (true) {
            const c = self.buf[self.offset];

            switch (state) {
                .start => {
                    // consume whitespace and reset
                    if (is_whitespace(c)) {
                        self.start = self.offset + 1;
                        // self.col += 1;

                        if (c == '\n') {
                            self.line += 1;
                            self.col = 0;
                        }

                        tok.loc.col = self.col;
                    } else if (is_digit(c)) {
                        state = .number;
                        tok.tag = .number;
                    } else if (is_alpha(c)) {
                        state = .identifier;
                        continue;
                        // tok.tag = .symbol;
                    } else {
                        // otherwise it is a single character to handle
                        switch (c) {
                            '(' => {
                                tok.tag = .lparen;
                                break;
                            },
                            ')' => {
                                tok.tag = .rparen;
                                break;
                            },
                            '>' => {
                                state = .greater;
                                tok.tag = .gt;
                            },
                            '<' => {
                                state = .less;
                                tok.tag = .lt;
                            },
                            ';' => {
                                state = .comment;
                            },
                            ':' => {
                                state = .keyword;
                            },
                            '+' => {
                                state = .maybe_number;
                                tok.tag = .plus;
                            },
                            '.' => {
                                state = .period;
                            },
                            '-' => {
                                state = .maybe_number;
                                tok.tag = .minus;
                            },
                            '*' => {
                                tok.tag = .asterisk;
                                break;
                            },
                            '/' => {
                                tok.tag = .slash;
                                break;
                            },
                            '%' => {
                                tok.tag = .modulo;
                                break;
                            },

                            '"' => {
                                tok.tag = .quote;
                                break;
                            },
                            '\'' => {
                                tok.tag = .single_quote;
                                break;
                            },
                            '`' => {
                                tok.tag = .backtick;
                                break;
                            },
                            ',' => {
                                tok.tag = .comma;
                                break;
                            },
                            '\\' => {
                                tok.tag = .backslash;
                                break;
                            },
                            '#' => {
                                tok.tag = .sharp;
                                state = .sharp;
                            },
                            else => {
                                tok.tag = .unknown;
                                break;
                            },
                        }
                    }

                    // otherwise it must be a symbol character
                },
                .sharp => {
                    // next character should be t for true and f for false
                    if (c == 't') {
                        tok.tag = .@"true";
                    } else if (c == 'f') {
                        tok.tag = .@"false";
                    } else {
                        tok.tag = .unknown;
                    }
                    break;
                },
                .number => {
                    if (!is_digit(c)) {
                        if (c == '.') {
                            state = .period;
                        } else {
                            self.offset -= 1;
                            // self.col -= 1;
                            break;
                        }
                    }
                },
                .identifier => {
                    if (self.maybe_identifier()) |id_tag| {
                        tok.tag = id_tag;
                        self.offset -= 1;
                        // self.col -= 1;
                        break;
                    } else {
                        state = .symbol;
                    }
                },
                .symbol => {
                    tok.tag = .symbol;
                    if (!is_symbol(c)) {
                        self.offset -= 1;
                        // self.col -= 1;
                        break;
                    }
                },
                .keyword => {
                    tok.tag = .keyword;
                    if (!is_symbol(c)) {
                        self.offset -= 1;
                        // self.col -= 1;
                        break;
                    }
                },
                .comment => {
                    // its a comment until a newline
                    switch (c) {
                        '\n' => {
                            self.start = self.offset + 1;
                            self.line += 1;
                            self.col = 0;
                            state = .start;
                        },
                        else => {},
                    }
                },
                .maybe_number => {
                    // if we hit a delimiter then this is
                    // an identifier
                    if (is_delim(c)) {
                        self.offset -= 1;
                        // self.col -= 1;
                        break;
                    }

                    // if its a number then this is a number
                    if (is_digit(c)) {
                        state = .number;
                        tok.tag = .number;
                    } else {
                        state = .symbol;
                        tok.tag = .symbol;
                    }
                },
                .period => {
                    // if we hit a delimiter then this is
                    // a period
                    if (is_delim(c)) {
                        self.offset -= 1;
                        // self.col -= 1;
                        tok.tag = .period;
                        break;
                    } else if (is_digit(c)) {
                        state = .number;
                        tok.tag = .number;
                    } else {
                        // otherwise its a symbol
                        state = .symbol;
                        tok.tag = .symbol;
                    }
                },
                .greater => {
                    if (is_delim(c)) {
                        self.offset -= 1;
                        // self.col -= 1;
                        tok.tag = .gt;
                        break;
                    } else if (c == '=') {
                        tok.tag = .gte;
                        break;
                    } else {
                        // otherwise its a symbol
                        state = .symbol;
                        tok.tag = .symbol;
                    }
                },
                .less => {
                    if (is_delim(c)) {
                        self.offset -= 1;
                        // self.col -= 1;
                        tok.tag = .lt;
                        break;
                    } else if (c == '=') {
                        tok.tag = .lte;
                        break;
                    } else {
                        // otherwise its a symbol
                        state = .symbol;
                        tok.tag = .symbol;
                    }
                },
            }

            // nothing left, return
            const new_idx = self.offset + 1;
            if (new_idx == self.buf.len) {
                break;
            } else {
                self.offset = new_idx;
            }
        }

        self.offset += 1;
        self.col += @intCast(u32, self.offset - self.start);
        tok.loc.slice = self.buf[self.start..self.offset];

        if (state == .comment) {
            tok.tag = .eof;
        }

        return tok;
    }

    /// attempt to increasae index, returns false if cant
    fn incOffset(self: *Self) ?u8 {
        self.offset += 1;
        self.col += 1;
        if (self.offset == self.buf.len)
            return null;

        return self.buf[self.offset];
    }

    /// attempt to compare if the buffer contains the rest of this value
    pub fn compareId(self: *Self, id: []const u8) bool {
        // cant fit in the rest of the buffer so cant match
        if (self.offset + id.len > self.buf.len) return false;
        for (id) |item, index| {
            if (self.buf[self.offset + index] != item) return false;
        }

        // if successful we increase the offset
        self.col += @intCast(u32, id.len);
        self.offset += id.len;
        return true;
    }

    /// figure out which identifier this is or just a symbol
    inline fn maybe_identifier(self: *Self) ?Tag {
        switch (self.buf[self.offset]) {
            'a' => {
                if (self.compareId("and")) {
                    return .@"and";
                }
            },
            'b' => {
                if (self.compareId("begin")) {
                    return .begin;
                }
            },
            'c' => {
                if (self.incOffset()) |c| {
                    switch (c) {
                        'o' => {
                            if (self.compareId("ond")) {
                                return .cond;
                            }
                        },
                        'a' => {
                            if (self.compareId("ase")) {
                                return .case;
                            }
                        },
                        else => {},
                    }
                }
            },
            'i' => {
                if (self.compareId("if")) {
                    return .@"if";
                }
            },

            'd' => {
                if (self.incOffset()) |c| {
                    switch (c) {
                        'o' => {
                            self.offset += 1;
                            return .do;
                        },
                        'e' => {
                            if (self.compareId("efine")) {
                                return .define;
                            }
                        },
                        else => {},
                    }
                }
            },
            'l' => {
                if (self.incOffset()) |c| {
                    switch (c) {
                        'a' => {
                            if (self.compareId("ambda")) {
                                return .lambda;
                            }
                        },
                        'e' => {
                            if (self.compareId("et")) {
                                const cc = self.buf[self.offset];
                                if (is_symbol(cc)) {
                                    if (self.compareId("*")) {
                                        return .let_star;
                                    }
                                    if (self.compareId("rec")) {
                                        return .letrec;
                                    }
                                } else {
                                    return .let;
                                }
                            }
                        },
                        else => {},
                    }
                }
            },
            'n' => {
                if (self.compareId("not")) {
                    return .@"not";
                }
            },
            'o' => {
                if (self.compareId("or")) {
                    return .@"or";
                }
            },
            's' => {
                if (self.compareId("set!")) {
                    return .set;
                }
            },
            else => {},
        }

        return null;
    }

    inline fn is_whitespace(c: u8) bool {
        return switch (c) {
            '\t', '\r', '\n', ' ' => true,
            else => false,
        };
    }

    inline fn is_delim(c: u8) bool {
        if (is_whitespace(c)) return true;

        return switch (c) {
            '(', ')' => true,
            else => false,
        };
    }

    inline fn is_digit(c: u8) bool {
        return switch (c) {
            '0'...'9' => true,
            else => false,
        };
    }

    inline fn is_alpha(c: u8) bool {
        return switch (c) {
            'a'...'z', 'A'...'Z' => true,
            else => false,
        };
    }

    /// valid characters that can be in a symbol
    inline fn is_symbol(c: u8) bool {
        return switch (c) {
            // extended characters
            '!',
            '$',
            '%',
            '&',
            '*',
            '+',
            '-',
            '.',
            '/',
            ':',
            '<',
            '=',
            '>',
            '?',
            '@',
            '^',
            '_',
            '~',
            // alpha numeric
            '0'...'9',
            'a'...'z',
            'A'...'Z',
            => true,
            else => false,
        };
    }
};

pub fn testScanner(
    src: []const u8,
    strs: []const []const u8,
    tags: []const Tag,
) !void {
    var scan = Scanner{
        .buf = src,
    };
    for (strs) |str, i| {
        const t = scan.next();
        if (str.len > 0) {
            try testing.expect(std.mem.eql(
                u8,
                t.loc.slice,
                str,
            ));
        }
        try testing.expect(t.tag == tags[i]);
    }
}

pub fn testScannerLoc(
    src: []const u8,
    strs: []const []const u8,
    tags: []const Tag,
    locs: []const SrcLoc,
) !void {
    var scan = Scanner{
        .buf = src,
    };
    for (strs) |str, i| {
        const t = scan.next();
        if (str.len > 0) {
            try testing.expect(std.mem.eql(
                u8,
                t.loc.slice,
                str,
            ));
        }
        try testing.expect(t.tag == tags[i]);
        if (t.loc.line != locs[i].line) {
            std.debug.print("[{}] line expected: {} got: {}\n", .{
                i,
                locs[i].line,
                t.loc.line,
            });
            return error.TestUnexpectedResult;
        }
        if (t.loc.col != locs[i].col) {
            std.debug.print("[{}] col expected: {} got: {}\n", .{
                i,
                locs[i].col,
                t.loc.col,
            });
            return error.TestUnexpectedResult;
        }
    }
}

pub fn testScannerTokens(
    src: []const u8,
    strs: []const []const u8,
    toks: []const Token,
) !void {
    var scan = Scanner{
        .buf = src,
    };
    for (strs) |str, i| {
        const t = scan.next();
        try testing.expect(std.mem.eql(
            u8,
            t.loc.slice,
            str,
        ));
        toks[i].print();
        t.print();
        try testing.expect(t.tag == toks[i].tag);
        try testing.expect(t.loc.line == toks[i].loc.line);
        try testing.expect(t.loc.col == toks[i].loc.col);
    }
}

// test "get next token" {
//     const code =
//         \\ (hello 123 world)
//     ;
//     const expected_str = [_][]const u8{
//         "(",
//         "hello",
//         "123",
//         "world",
//         ")",
//     };
//     const expected_tag = [_]Token{
//         .{
//             .tag = .lparen,
//             .loc = .{ .line = 0, .col = 1 },
//         },
//         .{
//             .tag = .symbol,
//             .loc = .{ .line = 0, .col = 2 },
//         },
//         .{
//             .tag = .number,
//             .loc = .{ .line = 0, .col = 8 },
//         },
//         .{
//             .tag = .symbol,
//             .loc = .{ .line = 0, .col = 12 },
//         },
//         .{
//             .tag = .rparen,
//             .loc = .{ .line = 0, .col = 17 },
//         },
//     };
//
//     try testScannerTokens(code, &expected_str, &expected_tag);
// }

test "valid symbol" {
    const code =
        \\symbol!
        \\symbol?
        \\.symbol
        \\sym->bol
        \\sym!$%&*+-./:>=>?@^_~all
        \\:keyword
    ;

    const expected_str = [_][]const u8{
        "symbol!",
        "symbol?",
        ".symbol",
        "sym->bol",
        "sym!$%&*+-./:>=>?@^_~all",
        ":keyword",
    };
    const expected_tag = [_]Tag{
        .symbol,
        .symbol,
        .symbol,
        .symbol,
        .symbol,
        .keyword,
    };

    try testScanner(code, &expected_str, &expected_tag);
}

test "numbers" {
    const code =
        \\125?3
        \\(+ -32)
        \\- */ % .
        \\12.4 0.5 .2
    ;

    const expected_str = [_][]const u8{
        "125",
        "?",
        "3",
        "(",
        "+",
        "-32",
        ")",
        "-",
        "*",
        "/",
        "%",
        ".",
        "12.4",
        "0.5",
        ".2",
    };
    const expected_tag = [_]Tag{
        .number,
        .unknown,
        .number,
        .lparen,
        .plus,
        .number,
        .rparen,
        .minus,
        .asterisk,
        .slash,
        .modulo,
        .period,
        .number,
        .number,
        .number,
    };

    try testScanner(code, &expected_str, &expected_tag);
}

test "comment" {
    const code =
        \\;; a comment
        \\ 125
        \\ ( start ;; comment
    ;

    const expected_str = [_][]const u8{
        "125",
        "(",
        "start",
    };
    const expected_tag = [_]Tag{
        .number,
        .lparen,
        .symbol,
    };

    try testScanner(code, &expected_str, &expected_tag);
}

test "literals" {
    const code =
        \\#t #f '()
    ;

    const expected_str = [_][]const u8{
        "#t",
        "#f",
    };
    const expected_tag = [_]Tag{
        .@"true",
        .@"false",
    };

    try testScanner(code, &expected_str, &expected_tag);
}

test "special characters" {
    const code =
        \\" ' ` , \ #
    ;

    const expected_str = [_][]const u8{
        \\"
        ,
        \\'
        ,
        \\`
        ,
        \\,
        ,
        \\\
        ,
        \\#
    };
    const expected_tag = [_]Tag{
        .quote,
        .single_quote,
        .backtick,
        .comma,
        .backslash,
        .sharp,
    };

    try testScanner(code, &expected_str, &expected_tag);
}

test "just a number" {
    const code =
        \\125
    ;

    const expected_str = [_][]const u8{
        "125",
    };
    const expected_tag = [_]Tag{
        .number,
    };

    try testScanner(code, &expected_str, &expected_tag);
}

test "just a symbol" {
    const code =
        \\symbol
    ;

    const expected_str = [_][]const u8{
        "symbol",
    };
    const expected_tag = [_]Tag{
        .symbol,
    };

    try testScanner(code, &expected_str, &expected_tag);
}

test "if expr" {
    const code =
        \\(if thn els)
    ;

    const expected_str = [_][]const u8{
        "(",
        "if",
        "thn",
        "els",
        ")",
    };
    const expected_tag = [_]Tag{
        .lparen,
        .@"if",
        .symbol,
        .symbol,
        .rparen,
    };

    try testScanner(code, &expected_str, &expected_tag);
}

test "identifiers" {
    const code =
        \\and
        \\begin
        \\case
        \\cond
        \\define
        \\do
        \\if
        \\lambda
        \\let
        \\let*
        \\letrec
        \\or
        \\set!
    ;

    const expected_str = [_][]const u8{
        \\and
        ,
        \\begin
        ,
        \\case
        ,
        \\cond
        ,
        \\define
        ,
        \\do
        ,
        \\if
        ,
        \\lambda
        ,
        \\let
        ,
        \\let*
        ,
        \\letrec
        ,
        \\or
        ,
        \\set!
        ,
    };
    const expected_tag = [_]Tag{
        .@"and",
        .begin,
        .case,
        .cond,
        .define,
        .do,
        .@"if",
        .lambda,
        .let,
        .let_star,
        .letrec,
        .@"or",
        .set,
    };

    try testScanner(code, &expected_str, &expected_tag);
}

test "greater than and less than" {
    const code =
        \\> >= < <= <34> >
    ;

    const expected_str = [_][]const u8{
        ">",
        ">=",
        "<",
        "<=",
        "<34>",
        ">",
    };
    const expected_tag = [_]Tag{
        .gt,
        .gte,
        .lt,
        .lte,
        .symbol,
        .gt,
    };

    try testScanner(code, &expected_str, &expected_tag);
}

test "boolean logic" {
    const code =
        \\and not or
    ;

    const expected_str = [_][]const u8{
        "and",
        "not",
        "or",
    };
    const expected_tag = [_]Tag{
        .@"and",
        .@"not",
        .@"or",
    };

    try testScanner(code, &expected_str, &expected_tag);
}

test "if" {
    const code =
        \\(if (> 2 3) thn els)
    ;

    const expected_str = [_][]const u8{
        "(",
        "if",
        "(",
        ">",
        "2",
        "3",
        ")",
        "thn",
        "els",
        ")",
        "",
    };
    const expected_tag = [_]Tag{
        .lparen,
        .@"if",
        .lparen,
        .gt,
        .number,
        .number,
        .rparen,
        .symbol,
        .symbol,
        .rparen,
        .eof,
    };

    try testScanner(code, &expected_str, &expected_tag);
}

test "positions" {
    const code =
        \\(if (> 2 3)
        \\    thn
        \\    els)
    ;

    const expected_str = [_][]const u8{
        "(",
        "if",
        "(",
        ">",
        "2",
        "3",
        ")",
        "thn",
        "els",
        ")",
        "",
    };
    const expected_tag = [_]Tag{
        .lparen,
        .@"if",
        .lparen,
        .gt,
        .number,
        .number,
        .rparen,
        .symbol,
        .symbol,
        .rparen,
        .eof,
    };

    const expected_locs = [_]SrcLoc{
        .{ .line = 1, .col = 1 },
        .{ .line = 1, .col = 2 },
        .{ .line = 1, .col = 5 },
        .{ .line = 1, .col = 6 },
        .{ .line = 1, .col = 8 },
        .{ .line = 1, .col = 10 },
        .{ .line = 1, .col = 11 },
        .{ .line = 2, .col = 5 },
        .{ .line = 3, .col = 5 },
        .{ .line = 3, .col = 8 },
    };

    try testScannerLoc(code, &expected_str, &expected_tag, &expected_locs);
}
