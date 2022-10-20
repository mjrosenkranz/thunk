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
    times,
    divide,
    modulus,

    period,

    // quotes and stuff
    quote,
    single_quote,
    backtick,
    comma,
    backslash,
    sharp,

    unknown,
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
        comment,
        // starts with a symbol that may be
        // a number
        maybe_number,
        period,
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

    pub fn next(self: *Self) ?Token {
        var tok = Token{
            .tag = .unknown,
            .loc = .{
                .start = self.idx,
                .end = 0,
            },
        };

        var state: State = .start;
        while (true) {
            // nothing left, return
            if (self.idx == self.buf.len) {
                return null;
            }

            const c = self.buf[self.idx];

            switch (state) {
                .start => {
                    // consume whitespace and reset
                    if (is_whitespace(c)) {
                        tok.loc.start = self.idx + 1;
                    } else if (is_digit(c)) {
                        state = .number;
                    } else if (is_alpha(c)) {
                        state = .symbol;
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
                                tok.tag = .times;
                                break;
                            },
                            '/' => {
                                tok.tag = .divide;
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
                                break;
                            },
                            else => {
                                tok.tag = .unknown;
                                break;
                            },
                        }
                    }

                    // otherwise it must be a symbol character
                },
                .number => {
                    if (!is_digit(c)) {
                        if (c == '.') {
                            state = .period;
                        } else {
                            self.idx -= 1;
                            tok.tag = .number;
                            break;
                        }
                    }
                },
                .symbol => {
                    if (!(is_symbol(c))) {
                        self.idx -= 1;
                        tok.tag = .symbol;
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
                    } else {
                        state = .symbol;
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
                    } else {
                        // otherwise its a symbol
                        state = .symbol;
                    }
                },
            }

            self.idx += 1;
        }

        self.idx += 1;
        tok.loc.end = self.idx;

        return tok;
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

    var scan = Scanner.init(code);
    var i: usize = 0;
    while (scan.next()) |t| {
        try testing.expect(std.mem.eql(
            u8,
            scan.buf[t.loc.start..t.loc.end],
            expected_str[i],
        ));
        try testing.expect(t.tag == expected_tag[i]);
        _ = expected_tag[i];

        i += 1;
    }
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
    };

    var scan = Scanner.init(code);
    var i: usize = 0;
    while (scan.next()) |t| {
        try testing.expect(std.mem.eql(
            u8,
            scan.buf[t.loc.start..t.loc.end],
            expected_str[i],
        ));
        try testing.expect(t.tag == expected_tag[i]);
        _ = expected_tag[i];

        i += 1;
    }
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
        .times,
        .divide,
        .modulus,
        .period,
        .number,
        .number,
        .number,
    };

    var scan = Scanner.init(code);
    var i: usize = 0;
    while (scan.next()) |t| {
        try testing.expect(std.mem.eql(
            u8,
            scan.buf[t.loc.start..t.loc.end],
            expected_str[i],
        ));
        try testing.expect(t.tag == expected_tag[i]);
        _ = expected_tag[i];

        i += 1;
    }
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

    var scan = Scanner.init(code);
    var i: usize = 0;
    while (scan.next()) |t| {
        try testing.expect(std.mem.eql(
            u8,
            scan.buf[t.loc.start..t.loc.end],
            expected_str[i],
        ));
        try testing.expect(t.tag == expected_tag[i]);
        _ = expected_tag[i];

        i += 1;
    }
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

    var scan = Scanner.init(code);
    var i: usize = 0;
    while (scan.next()) |t| {
        try testing.expect(std.mem.eql(
            u8,
            scan.buf[t.loc.start..t.loc.end],
            expected_str[i],
        ));
        try testing.expect(t.tag == expected_tag[i]);
        _ = expected_tag[i];

        i += 1;
    }
}
