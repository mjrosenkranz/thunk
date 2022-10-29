const std = @import("std");
const testing = std.testing;

pub const Loc = struct {
    start: usize,
    end: usize,
};

pub const Tag = enum {
    // delimiters
    lparen,
    rparen,

    symbol,
    number,

    // math symbols
    plus,
    minus,
    asterisk,
    slash,
    modulus,

    period,

    // quotes and stuff
    quote,
    single_quote,
    backtick,
    comma,
    backslash,
    sharp,

    // type keywords
    t, // true
    f, // false
    nil,

    unknown,
    eof,
};

/// Token type
pub const Token = struct {
    tag: Tag,
    loc: Loc,
};

pub const Scanner = struct {
    const Self = @This();

    const State = enum {
        start,
        number,
        symbol,
        identifier,
        comment,
        // starts with a symbol that may be
        // a number
        maybe_number,
        period,
        sharp,
    };

    /// where are we?
    idx: usize = 0,
    /// buffer we are tokenizing
    buf: []const u8,

    pub fn init(buffer: []const u8) Self {
        return Self{
            .buf = buffer,
        };
    }

    pub fn next(self: *Self) Token {
        var tok = Token{
            .tag = .eof,
            .loc = .{
                .start = self.idx,
                .end = 0,
            },
        };

        if (self.idx >= self.buf.len)
            return tok;

        var state: State = .start;
        while (true) {
            const c = self.buf[self.idx];

            switch (state) {
                .start => {
                    // consume whitespace and reset
                    if (is_whitespace(c)) {
                        tok.loc.start = self.idx + 1;
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
                            ';' => {
                                state = .comment;
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
                                tok.tag = .modulus;
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
                        tok.tag = .t;
                    } else if (c == 'f') {
                        tok.tag = .f;
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
                            self.idx -= 1;
                            break;
                        }
                    }
                },
                .identifier => {
                    if (self.maybe_identifier()) |id_tag| {
                        tok.tag = id_tag;
                        self.idx -= 1;
                        break;
                    } else {
                        state = .symbol;
                    }
                },
                .symbol => {
                    tok.tag = .symbol;
                    if (!is_symbol(c)) {
                        self.idx -= 1;
                        break;
                    }
                },
                .comment => {
                    // its a comment until a newline
                    switch (c) {
                        '\n' => {
                            tok.loc.start = self.idx + 1;
                            state = .start;
                        },
                        else => {},
                    }
                },
                .maybe_number => {
                    // if we hit a delimiter then this is
                    // an identifier
                    if (is_delim(c)) {
                        self.idx -= 1;
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
                        self.idx -= 1;
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
            }

            // nothing left, return
            const new_idx = self.idx + 1;
            if (new_idx == self.buf.len) {
                break;
            } else {
                self.idx = new_idx;
            }
        }

        self.idx += 1;
        tok.loc.end = self.idx;

        if (state == .comment) {
            tok.tag = .eof;
        }

        return tok;
    }

    /// attempt to increasae index, returns false if cant
    fn incIdx(self: *Self) bool {
        self.idx += 1;
        if (self.idx == self.buf.len)
            return false;

        return true;
    }

    /// figure out which identifier this is or just a symbol
    inline fn maybe_identifier(self: *Self) ?Tag {
        var c = self.buf[self.idx];
        if (c == 'n' and self.incIdx()) {
            c = self.buf[self.idx];
            if (c == 'i' and self.incIdx()) {
                c = self.buf[self.idx];
                if (c == 'l') {
                    if (self.incIdx()) {
                        c = self.buf[self.idx];
                        if (!is_symbol(c)) {
                            return .nil;
                        }
                    } else {
                        return .nil;
                    }
                }
            }
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

test "init" {
    const code =
        \\ (hello world)
    ;

    var scan = Scanner.init(code);
    _ = scan;
}

pub fn testScanner(
    src: []const u8,
    strs: []const []const u8,
    tags: []const Tag,
) !void {
    var scan = Scanner.init(src);
    for (strs) |str, i| {
        const t = scan.next();
        std.debug.print("{}: {s}\n", .{ t, scan.buf[t.loc.start..t.loc.end] });
        try testing.expect(std.mem.eql(
            u8,
            scan.buf[t.loc.start..t.loc.end],
            str,
        ));
        try testing.expect(t.tag == tags[i]);
    }
}

test "get next token" {
    const code =
        \\ (hello 123 world)
    ;

    const expected_str = [_][]const u8{
        "(",
        "hello",
        "123",
        "world",
        ")",
    };
    const expected_tag = [_]Tag{
        .lparen,
        .symbol,
        .number,
        .symbol,
        .rparen,
    };

    try testScanner(code, &expected_str, &expected_tag);
}

test "valid symbol" {
    const code =
        \\symbol!
        \\symbol?
        \\.symbol
        \\sym->bol
        \\sym!$%&*+-./:>=>?@^_~all
    ;

    const expected_str = [_][]const u8{
        "symbol!",
        "symbol?",
        ".symbol",
        "sym->bol",
        "sym!$%&*+-./:>=>?@^_~all",
    };
    const expected_tag = [_]Tag{
        .symbol,
        .symbol,
        .symbol,
        .symbol,
        .symbol,
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
        .modulus,
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
        \\#t #f nil
    ;

    const expected_str = [_][]const u8{
        "#t",
        "#f",
        "nil",
    };
    const expected_tag = [_]Tag{
        .t,
        .f,
        .nil,
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
