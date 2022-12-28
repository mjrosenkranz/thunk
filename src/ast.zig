const std = @import("std");
const Value = @import("value.zig").Value;
const Allocator = std.mem.Allocator;
const Scanner = @import("scanner.zig").Scanner;
const Token = @import("token.zig");
const SrcLoc = @import("file.zig").SrcLoc;

pub const Ast = @This();

const NodeList = std.ArrayList(Node);
const TokenList = std.ArrayList(Token);
const DATA_ALIGN = 8;
const NodeDataList = std.ArrayListAligned(u8, DATA_ALIGN);

const ParseError = error{
    UndexpectedRightParen,
    UnexpectedTag,
    UnexpectedEof,

    SyntaxError,
} || Allocator.Error;

/// all the nodes in this tree
nodes: NodeList,

/// all the tokens from which these nodes were created
tokens: TokenList,

/// extra data for our nodes
data: NodeDataList,

pub fn init(allocator: Allocator) Ast {
    var ast = Ast{
        .tokens = TokenList.init(allocator),
        .nodes = NodeList.init(allocator),
        .data = NodeDataList.init(allocator),
    };
    return ast;
}

pub fn pushData(
    ast: *Ast,
    comptime T: type,
    t: T,
) !NodeIdx {

    // allocate the required number of bytes
    const idx = @intCast(NodeIdx, ast.data.items.len);
    var size: u8 = @sizeOf(T);
    const align_t = @alignOf(T);

    comptime std.debug.assert(align_t % DATA_ALIGN == 0);

    while (size > 0) : (size -= 1) {
        try ast.data.append(0xaa);
    }

    const ptr = @ptrCast([*]T, @alignCast(align_t, &ast.data.items[idx]));
    ptr[0] = t;

    return idx;
}

/// gets typed data from the tree
pub fn getData(ast: *Ast, comptime T: type, idx: NodeIdx) T {
    const align_t = @alignOf(T);
    const ptr = @ptrCast([*]T, @alignCast(align_t, &ast.data.items[idx]));
    var t: T = undefined;
    t = ptr[0];
    return t;
}

/// Parse the whole source into a tree
pub fn parse(ast: *Ast, src: []const u8) ParseError!void {
    try ast.nodes.append(.{
        .tag = .root,
        .token_idx = 0,
        .children = .{},
    });

    // TODO: ensure capacity so that we dont need try everywere
    // create a scanner
    var scanner = Scanner.init(src);
    const first_tok = scanner.next();

    if (first_tok.tag == .eof) return;

    const id = try ast.parseExpr(first_tok, &scanner);
    // modify the root to use the new found child
    ast.nodes.items[0].children = .{
        .l = id,
    };
}

/// this is the first stop in our parsing journey.
/// here we parse the top level statements.
/// This should be just one when we are just inside a single sexpr
pub fn parseExpr(ast: *Ast, tok: Token, scanner: *Scanner) ParseError!NodeIdx {
    // we done!
    if (tok.tag == .eof) return 0;
    return switch (tok.tag) {
        .number => try ast.parseNum(tok),
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
        => try ast.parseSymbol(tok),
        .lparen => try ast.parseApply(tok, scanner),
        .rparen => ParseError.UndexpectedRightParen,
        else => {
            std.debug.print("UnexpectedTag: {}\n", .{tok.tag});
            return ParseError.UnexpectedTag;
        },
    };
}

// TODO: parse other kinds of numbers
pub fn parseNum(
    ast: *Ast,
    tok: Token,
) ParseError!NodeIdx {
    // try to parse the number
    const f = std.fmt.parseFloat(f32, tok.loc.slice) catch {
        return ParseError.SyntaxError;
    };
    const data_idx = try ast.pushData(Value, .{ .float = f });

    // push the token
    const token_idx = @intCast(u32, ast.tokens.items.len);
    try ast.tokens.append(tok);

    const node_idx = @intCast(u32, ast.nodes.items.len);
    // parse the number into a value
    // push the node
    try ast.nodes.append(.{
        .tag = .constant,
        .token_idx = token_idx,
        .children = .{ .l = data_idx },
    });
    return node_idx;
}

pub fn parseSymbol(
    ast: *Ast,
    tok: Token,
) ParseError!NodeIdx {
    // TODO: what data does a symbol have
    // const data_idx = try ast.pushData(Value, .{ .float = f });

    // push the token
    const token_idx = @intCast(u32, ast.tokens.items.len);
    try ast.tokens.append(tok);

    const node_idx = @intCast(u32, ast.nodes.items.len);
    // parse the number into a value
    // push the node
    try ast.nodes.append(.{
        .tag = .symbol,
        .children = .{},
        .token_idx = token_idx,
    });
    return node_idx;
}

pub fn parseApply(
    ast: *Ast,
    paren_tok: Token,
    scanner: *Scanner,
) ParseError!NodeIdx {
    // push the lparen token
    const paren_tok_id = @intCast(NodeIdx, ast.tokens.items.len);
    try ast.tokens.append(paren_tok);

    // create apply node
    const apply_id = @intCast(NodeIdx, ast.nodes.items.len);
    // get the symbol to apply
    var apply_node = try ast.nodes.addOne();
    apply_node.* = .{
        .tag = .apply,
        .token_idx = paren_tok_id,
        .children = .{},
    };

    // TODO: handle left is rparen
    // get the thing we are calling
    {
        const tok = scanner.next();
        const left = try ast.parseExpr(tok, scanner);
        apply_node.children.l = left;
    }

    // we start making a list by creating the first pair
    // var last: ?NodeIdx = null;
    var pair_idx = @intCast(NodeIdx, ast.nodes.items.len);
    apply_node.children.r = pair_idx;
    try ast.nodes.append(.{
        .tag = .pair,
        // TODO: patch token idx
        .token_idx = 0,
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
                    var pair = &ast.nodes.items[pair_idx];
                    if (pair.children.l != 0) {
                        // old pair right points to new one
                        pair_idx = @intCast(NodeIdx, ast.nodes.items.len);
                        pair.children.r = pair_idx;

                        // create the new on
                        try ast.nodes.append(.{
                            .tag = .pair,
                            .children = .{},
                            .token_idx = 0,
                        });
                    }
                }

                const n_idx = try ast.parseExpr(tok, scanner);
                ast.nodes.items[pair_idx].children.l = n_idx;
                n_args += 1;
            },
        }
    }

    return apply_id;
}

pub fn deinit(ast: *Ast) void {
    ast.nodes.deinit();
    ast.tokens.deinit();
    ast.data.deinit();
}

/// index for looking up nodes in the tree
pub const NodeIdx = u32;
/// index for looking up tokens in the tree
pub const TokenIdx = u32;

/// A node in the ast
pub const Node = struct {
    /// the type of this node
    tag: Tag,
    /// the token from which this node came
    token_idx: TokenIdx = 0,
    /// children of this node
    children: Children,

    pub const Children = packed struct {
        // 0 implies that there are no children, as 0 can only be the root
        l: NodeIdx = 0,
        r: NodeIdx = 0,
    };

    pub const Tag = enum {
        /// the root of the tree
        root,
        /// a constant value like a number or interned string
        /// the left child is the index of the start of the constant's
        /// value in the data list
        constant,
        /// a symbol which may be bound to a value
        /// the left child is the index of the start of the symbols's
        /// name in the data list
        symbol,
        /// the application of a procedure with params
        /// l points to the symbol we are applying
        /// r points to the first argument
        /// if r is 0 then there are no arguments
        apply,
        /// basically a linked list
        /// l is the value at this point in the list
        /// r is the next item in the list
        /// if r is zero wer're at the end of the list
        pair,
    };
};

fn testAst(ast: Ast, expected: []const Node) !void {
    if (ast.nodes.items.len != expected.len) {
        std.debug.print("expected {} nodes, got {}\n", .{ expected.len, ast.nodes.items.len });
    }

    // start with the first node since 0 is always root
    for (ast.nodes.items) |n, i| {
        const e = expected[i];
        std.testing.expect(e.tag == n.tag) catch |err| {
            std.debug.print("tag missmatch: node[{}] expected {}, got {}\n", .{
                i,
                e.tag,
                n.tag,
            });

            return err;
        };
        std.testing.expectEqual(e.children, n.children) catch |err| {
            std.debug.print("child missmatch: node[{}] expected {}, got {}\n", .{
                i,
                e.children,
                n.children,
            });
            return err;
        };
    }
}

test "parse const" {
    const code =
        \\32
    ;
    var ast = Ast.init(std.testing.allocator);
    defer ast.deinit();

    try ast.parse(code);
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
    try std.testing.expect(ast.getData(Value, ast.nodes.items[1].children.l).float == 32.0);
}

test "apply no args" {
    const code =
        \\(+)
    ;
    var ast = Ast.init(std.testing.allocator);
    defer ast.deinit();

    try ast.parse(code);
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
            .token_idx = 1,
            .children = .{},
        },
    };
    try ast.testAst(&expected);
}

test "apply one arg" {
    const code =
        \\(+ 32)
    ;
    var ast = Ast.init(std.testing.allocator);
    defer ast.deinit();

    try ast.parse(code);
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
            .token_idx = 1,
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
    var ast = Ast.init(std.testing.allocator);
    defer ast.deinit();

    try ast.parse(code);
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
            .token_idx = 1,
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
    var ast = Ast.init(std.testing.allocator);
    defer ast.deinit();

    try ast.parse(code);
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
            .token_idx = 3,
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
    try std.testing.expect(ast.getData(Value, ast.nodes.items[4].children.l).float == 32.0);
    try std.testing.expect(ast.getData(Value, ast.nodes.items[6].children.l).float == 25.0);
    try std.testing.expect(ast.getData(Value, ast.nodes.items[8].children.l).float == 44.0);
}
//
//
// test "parse nested" {
//     const code =
//         \\(* 3 (+ 4 5))
//     ;
//     _ = code;
//     // root -> * -> 3
//     //         |--> + -> 4
//     //              |--> 5
//     const expected = [_]Node{
//         .{
//             .tok = .{ .tag = .asterisk },
//             .children = .{
//                 .l = 1,
//                 .r = 2,
//             },
//         },
//         .{ .tok = .{ .tag = .number } },
//         .{
//             .tok = .{ .tag = .plus },
//             .children = .{
//                 .l = 3,
//                 .r = 4,
//             },
//         },
//         .{ .tok = .{ .tag = .number } },
//         .{ .tok = .{ .tag = .number } },
//     };
//     _ = expected;
// }
