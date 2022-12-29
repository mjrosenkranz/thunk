const std = @import("std");
const SrcLoc = @import("file.zig").SrcLoc;

pub const Token = @This();

tag: Tag,
loc: SrcLoc,

/// helper for debug printing a token
pub fn print(t: Token) void {
    std.debug.print("<{s}:{d}:{}> {}: '{s}'\n", .{
        t.loc.file,
        t.loc.line,
        t.loc.col,
        t.tag,
        t.loc.slice,
    });
}

pub const Tag = enum {
    // delimiters
    lparen,
    rparen,

    // atoms
    symbol,
    keyword,
    number,
    @"false",
    @"true",

    // --- builtin symbols ---

    // math symbols
    plus,
    minus,
    asterisk,
    slash,
    modulus,
    gt,
    lt,
    gte,
    lte,

    // boolean logic
    @"and",
    @"or",
    @"not",

    // ---------------------

    // quotes and stuff
    quote,
    single_quote,
    backtick,
    comma,
    backslash,
    sharp,
    period,

    // --- special forms ---
    begin,
    case,
    cond,
    define,
    do,
    lambda,
    @"if",
    let,
    let_star,
    letrec,
    set,
    // ---------------------

    unknown,
    eof,
};
