const std = @import("std");
const Value = @import("value.zig").Value;
const Allocator = std.mem.Allocator;
const Scanner = @import("scanner.zig").Scanner;
const Token = @import("token.zig");
const Ast = @import("ast.zig");
const NodeIdx = Ast.NodeIdx;
const Node = Ast.Node;
const DATA_ALIGN = Ast.DATA_ALIGN;

pub const Parser = @This();

const NodeList = std.ArrayList(Node);
const TokenList = std.ArrayList(Token);
const NodeDataList = std.ArrayListAligned(u8, DATA_ALIGN);

/// all the nodes in this tree
nodes: NodeList,

/// all the tokens from which these nodes were created
tokens: TokenList,

/// extra data for our nodes
data: NodeDataList,

allocator: Allocator,

pub fn init(allocator: Allocator) Parser {
    return Parser{
        .allocator = allocator,
        .tokens = TokenList.init(allocator),
        .nodes = NodeList.init(allocator),
        .data = NodeDataList.init(allocator),
    };
}

pub fn deinit(parser: *Parser) void {
    parser.nodes.deinit();
    parser.tokens.deinit();
    parser.data.deinit();
}

pub fn pushData(
    parser: *Parser,
    comptime T: type,
    t: T,
) !NodeIdx {

    // allocate the required number of bytes
    const idx = @intCast(NodeIdx, parser.data.items.len);
    var size: u8 = @sizeOf(T);
    const align_t = @alignOf(T);

    comptime std.debug.assert(align_t % DATA_ALIGN == 0);

    while (size > 0) : (size -= 1) {
        try parser.data.append(0xaa);
    }

    const ptr = @ptrCast([*]T, @alignCast(align_t, &parser.data.items[idx]));
    ptr[0] = t;

    return idx;
}

const ParseError = error{
    UndexpectedRightParen,
    UnexpectedTag,
    UnexpectedEof,

    SyntaxError,
} || Allocator.Error;

/// Parse the whole source into a tree
pub fn parse(parser: *Parser, src: []const u8) ParseError!Ast {
    try parser.nodes.append(.{
        .tag = .root,
        .token_idx = 0,
        .children = .{},
    });

    // TODO: ensure capacity so that we dont need try everywere
    // create a scanner
    var scanner = Scanner.init(src);
    const first_tok = scanner.next();

    const id = try parser.parseExpr(first_tok, &scanner);
    // modify the root to use the new found child
    parser.nodes.items[0].children = .{
        .l = id,
    };

    return Ast{
        .tokens = parser.tokens.toOwnedSlice(),
        .nodes = parser.nodes.toOwnedSlice(),
        .data = parser.data.toOwnedSlice(),
    };
}

/// this is the first stop in our parsing journey.
/// here we parse the top level statements.
/// This should be just one when we are just inside a single sexpr
pub fn parseExpr(parser: *Parser, tok: Token, scanner: *Scanner) ParseError!NodeIdx {
    // we done!
    if (tok.tag == .eof) return 0;
    return switch (tok.tag) {
        .number => try parser.parseNum(tok),
        // these are all builtin symbols
        .plus,
        .minus,
        .asterisk,
        .slash,
        .modulus,
        .gt,
        .lt,
        .gte,
        .lte,
        .symbol,
        => try parser.parseSymbol(tok),
        .lparen => try parser.parseApply(tok, scanner),
        .rparen => ParseError.UndexpectedRightParen,
        else => {
            std.debug.print("UnexpectedTag: {}\n", .{tok.tag});
            return ParseError.UnexpectedTag;
        },
    };
}

// TODO: parse other kinds of numbers
pub fn parseNum(
    parser: *Parser,
    tok: Token,
) ParseError!NodeIdx {
    // try to parse the number
    const f = std.fmt.parseFloat(f32, tok.loc.slice) catch {
        return ParseError.SyntaxError;
    };
    const data_idx = try parser.pushData(Value, .{ .float = f });

    // push the token
    const token_idx = @intCast(u32, parser.tokens.items.len);
    try parser.tokens.append(tok);

    const node_idx = @intCast(u32, parser.nodes.items.len);
    // parse the number into a value
    // push the node
    try parser.nodes.append(.{
        .tag = .constant,
        .token_idx = token_idx,
        .children = .{ .l = data_idx },
    });
    return node_idx;
}

pub fn parseSymbol(
    parser: *Parser,
    tok: Token,
) ParseError!NodeIdx {
    // TODO: what data does a symbol have
    // const data_idx = try parser.pushData(Value, .{ .float = f });

    // push the token
    const token_idx = @intCast(u32, parser.tokens.items.len);
    try parser.tokens.append(tok);

    const node_idx = @intCast(u32, parser.nodes.items.len);
    // parse the number into a value
    // push the node
    try parser.nodes.append(.{
        .tag = .symbol,
        .children = .{},
        .token_idx = token_idx,
    });
    return node_idx;
}

pub fn parseApply(
    parser: *Parser,
    paren_tok: Token,
    scanner: *Scanner,
) ParseError!NodeIdx {
    // push the lparen token
    const paren_tok_id = @intCast(NodeIdx, parser.tokens.items.len);
    try parser.tokens.append(paren_tok);

    // create apply node
    const apply_id = @intCast(NodeIdx, parser.nodes.items.len);
    // get the symbol to apply
    var apply_node = try parser.nodes.addOne();
    apply_node.* = .{
        .tag = .apply,
        .token_idx = paren_tok_id,
        .children = .{},
    };

    // get the thing we are calling
    const caller_tok = scanner.next();
    const caller = try parser.parseExpr(caller_tok, scanner);
    apply_node.children.l = caller;

    // we start making a list by creating the first pair
    // var last: ?NodeIdx = null;
    var pair_idx = @intCast(NodeIdx, parser.nodes.items.len);
    apply_node.children.r = pair_idx;
    try parser.nodes.append(.{
        .tag = .pair,
        // TODO: patch token idx
        .token_idx = parser.nodes.items[caller].token_idx,
        .children = .{},
    });

    var n_args: u8 = 0;

    while (true) {
        const tok = scanner.next();

        // TODO: handle if the symbol makes this a special form
        switch (tok.tag) {
            // we at the end of the list
            .rparen => {
                // finish this pair
                break;
            },
            // we expect at least a right paren, this means that we have bad input
            .eof => return ParseError.UnexpectedEof,
            // otherwise, this must be an expresion
            else => {
                // if the pair's left is empty then replace it with our idx
                {
                    var pair = &parser.nodes.items[pair_idx];
                    if (pair.children.l != 0) {
                        // old pair right points to new one
                        pair_idx = @intCast(NodeIdx, parser.nodes.items.len);
                        pair.children.r = pair_idx;

                        // create the new on
                        try parser.nodes.append(.{
                            .tag = .pair,
                            .children = .{},
                            .token_idx = 0,
                        });
                    }
                }

                const n_idx = try parser.parseExpr(tok, scanner);
                parser.nodes.items[pair_idx].children.l = n_idx;
                parser.nodes.items[pair_idx].token_idx = parser.nodes.items[n_idx].token_idx;
                n_args += 1;
            },
        }
    }

    return apply_id;
}

test "parse const" {
    const code =
        \\32
    ;
    var parser = Parser.init(std.testing.allocator);
    defer parser.deinit();
    var ast = try parser.parse(code);
    defer ast.deinit(std.testing.allocator);

    // root -> const
    const expected = [_]Node{
        .{
            .tag = .root,
            .token_idx = 0,
            .children = .{ .l = 1 },
        },
        .{
            .tag = .constant,
            .token_idx = 0,
            .children = .{},
        },
    };
    try ast.testAst(&expected);
    // assert that we stored this bad boy correctly
    try std.testing.expect(ast.getData(Value, ast.nodes[1].children.l).float == 32.0);
}

test "apply no args" {
    const code =
        \\(+)
    ;
    var parser = Parser.init(std.testing.allocator);
    defer parser.deinit();
    var ast = try parser.parse(code);
    defer ast.deinit(std.testing.allocator);

    const expected = [_]Node{
        // 0
        .{
            .tag = .root,
            .token_idx = 0,
            .children = .{ .l = 1 },
        },
        // 1
        .{
            .tag = .apply,
            .token_idx = 0,
            .children = .{
                .l = 2,
                .r = 3,
            },
        },
        // 2
        .{
            .tag = .symbol,
            .token_idx = 1,
            .children = .{},
        },
        // 3
        .{
            .tag = .pair,
            .token_idx = 1,
            .children = .{},
        },
    };
    try ast.testAst(&expected);
}

test "apply no close" {
    const code =
        \\(+ 32 44
    ;
    var parser = Parser.init(std.testing.allocator);
    defer parser.deinit();

    try std.testing.expectError(ParseError.UnexpectedEof, parser.parse(code));
}

test "apply one arg" {
    const code =
        \\(+ 32)
    ;
    var parser = Parser.init(std.testing.allocator);
    defer parser.deinit();
    var ast = try parser.parse(code);
    defer ast.deinit(std.testing.allocator);

    const expected = [_]Node{
        // 0
        .{
            .tag = .root,
            .token_idx = 0,
            .children = .{ .l = 1 },
        },
        // 1
        .{
            .tag = .apply,
            .token_idx = 0,
            .children = .{
                .l = 2,
                .r = 3,
            },
        },
        // 2
        .{
            .tag = .symbol,
            .children = .{},
            .token_idx = 1,
            // .children = .{ .l = 0 },
        },
        // 3
        .{
            .tag = .pair,
            .token_idx = 2,
            .children = .{ .l = 4 },
        },
        // 4
        .{
            .tag = .constant,
            .token_idx = 2,
            .children = .{ .l = 0 * @sizeOf(Value) },
        },
    };
    try ast.testAst(&expected);
}

test "apply two args" {
    const code =
        \\(+ 32 55)
    ;
    var parser = Parser.init(std.testing.allocator);
    defer parser.deinit();
    var ast = try parser.parse(code);
    defer ast.deinit(std.testing.allocator);
    const expected = [_]Node{
        // 0
        .{
            .tag = .root,
            .token_idx = 0,
            .children = .{ .l = 1 },
        },
        // 1
        .{
            .tag = .apply,
            .token_idx = 0,
            .children = .{
                .l = 2,
                .r = 3,
            },
        },
        // 2
        .{
            .tag = .symbol,
            .children = .{},
            .token_idx = 1,
        },
        // 3
        .{
            .tag = .pair,
            .token_idx = 2,
            .children = .{ .l = 4, .r = 5 },
        },
        // 4
        .{
            .tag = .constant,
            .token_idx = 2,
            .children = .{ .l = 0 * @sizeOf(Value) },
        },
        // 5
        .{
            .tag = .pair,
            .token_idx = 3,
            .children = .{ .l = 6 },
        },
        // 6
        .{
            .tag = .constant,
            .token_idx = 3,
            .children = .{ .l = 1 * @sizeOf(Value) },
        },
    };
    try ast.testAst(&expected);
}

test "apply many" {
    const code =
        \\(+ 32 25 44)
    ;
    var parser = Parser.init(std.testing.allocator);
    defer parser.deinit();
    var ast = try parser.parse(code);
    defer ast.deinit(std.testing.allocator);
    // root -> apply -> +
    // l = null p = [ | ] p' = null
    //          |-> pair 32  l = *32 p = [32| ] p' = null
    //l = *32 p = [32| ] p' = null
    //              |-> pair 25
    //                   |-> 44
    const expected = [_]Node{
        // 0
        .{
            .tag = .root,
            .token_idx = 0,
            .children = .{ .l = 1 },
        },
        // 1
        .{
            .tag = .apply,
            .token_idx = 0,
            .children = .{
                .l = 2,
                .r = 3,
            },
        },
        // 2
        .{
            .tag = .symbol,
            .children = .{},
            .token_idx = 1,
            // .children = .{ .l = 0 },
        },
        // 3
        .{
            .tag = .pair,
            .token_idx = 2,
            .children = .{ .l = 4, .r = 5 },
        },
        // 4
        .{
            .tag = .constant,
            .token_idx = 2,
            .children = .{ .l = 0 * @sizeOf(Value) },
        },
        // 5
        .{
            .tag = .pair,
            .token_idx = 3,
            .children = .{ .l = 6, .r = 7 },
        },

        // 6
        .{
            .tag = .constant,
            .token_idx = 3,
            .children = .{ .l = 1 * @sizeOf(Value) },
        },

        // 7
        .{
            .tag = .pair,
            .token_idx = 4,
            .children = .{ .l = 8, .r = 0 },
        },

        // 8
        .{
            .tag = .constant,
            .token_idx = 4,
            .children = .{ .l = 2 * @sizeOf(Value) },
        },
    };
    try ast.testAst(&expected);
    // assert that we stored this bad boy correctly
    try std.testing.expect(ast.getData(Value, ast.nodes[4].children.l).float == 32.0);
    try std.testing.expect(ast.getData(Value, ast.nodes[6].children.l).float == 25.0);
    try std.testing.expect(ast.getData(Value, ast.nodes[8].children.l).float == 44.0);
}

test "apply nested" {
    const code =
        //01 2 34 5 6
        \\(* 3 (+ 4 5))
    ;
    _ = code;
    // 0       1        2
    // root -> apply -> *
    //         |    3 4
    //         |--> [ 3 | . ]
    //                    |     5       6
    //                    |--> apply -> +
    //                          |    7 8
    //                          |--> [ 4 | . ]
    //                                     |    9 10
    //                                     |--> [ 5 |   ]
    const expected = [_]Node{
        .{
            .tag = .root,
            .token_idx = 0,
            .children = .{ .l = 1 },
        },
        // 1
        .{
            .tag = .apply,
            .token_idx = 0,
            .children = .{
                .l = 2,
                .r = 3,
            },
        },
        // 2
        .{
            .tag = .symbol,
            .children = .{},
            .token_idx = 1,
        },
        // 3
        .{
            .tag = .pair,
            .token_idx = 2,
            .children = .{ .l = 4, .r = 5 },
        },
        // 4
        .{
            .tag = .constant,
            .token_idx = 2,
            .children = .{ .l = 0 * @sizeOf(Value) },
        },
        // 5
        .{
            .tag = .apply,
            .token_idx = 3,
            .children = .{
                .l = 2,
                .r = 3,
            },
        },
        // 6
        .{
            .tag = .symbol,
            .children = .{},
            .token_idx = 4,
        },
        // 7
        .{
            .tag = .pair,
            .token_idx = 5,
            .children = .{ .l = 8, .r = 9 },
        },
        // 8
        .{
            .tag = .constant,
            .token_idx = 5,
            .children = .{ .l = 1 * @sizeOf(Value) },
        },
        // 9
        .{
            .tag = .pair,
            .token_idx = 6,
            .children = .{ .l = 10, .r = 0 },
        },
        // 10
        .{
            .tag = .constant,
            .token_idx = 6,
            .children = .{ .l = 2 * @sizeOf(Value) },
        },
    };
    _ = expected;
}
