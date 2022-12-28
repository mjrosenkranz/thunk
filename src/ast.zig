const std = @import("std");
const Value = @import("value.zig").PackedValue;
const Allocator = std.mem.Allocator;
const Scanner = @import("scanner.zig").Scanner;
const Token = @import("token.zig");
const SrcLoc = @import("file.zig").SrcLoc;

pub const Ast = @This();

const NodeList = std.ArrayList(Node);
const TokenList = std.ArrayList(Token);
/// extra data can be of any type but by making it NodeIdx
/// we know that it is word aligned
const NodeDataList = std.ArrayList(NodeIdx);

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

/// Parse the whole source into a tree
pub fn parse(ast: *Ast, src: []const u8) !void {
    try ast.nodes.append(.{
        .tag = .root,
        .token_idx = 0,
        .children = .{},
    });

    // TODO: ensure capacity so that we dont need try everywere
    // create a scanner
    var scanner = Scanner.init(src);
    const id = try ast.parseExpr(&scanner);
    // modify the root to use the new found child
    ast.nodes.items[0].children = .{
        .l = id,
    };
}

/// this is the first stop in our parsing journey.
/// here we parse the top level statements.
/// This should be just one when we are just inside a single sexpr
pub fn parseExpr(ast: *Ast, scanner: *Scanner) !NodeIdx {
    const tok = scanner.next();

    // we done!
    // TODO: assert that everything balanced and shit
    if (tok.tag == .eof) return 0;

    switch (tok.tag) {
        .number => {
            // push the token
            const token_idx = @intCast(u32, ast.tokens.items.len);
            try ast.tokens.append(tok);

            const node_idx = @intCast(u32, ast.nodes.items.len);

            try ast.data.append(@bitCast(NodeIdx, @as(f32, 32)));
            // parse the number into a value
            // push the node
            try ast.nodes.append(.{
                .tag = .constant,
                .token_idx = token_idx,
                .children = .{ .l = 0 },
            });
            return node_idx;
        },
        else => return error.UnexpectedTag,
    }
}

pub fn deinit(ast: *Ast) void {
    ast.nodes.deinit();
    ast.tokens.deinit();
    ast.data.deinit();
}

/// gets typed data from the tree
pub fn getData(ast: *Ast, comptime T: type, idx: NodeIdx) T {
    const ptr = @ptrCast([*]T, &ast.data.items[idx]);
    var t: T = undefined;
    t = ptr[0];
    return t;
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
    children: Data,

    pub const Data = packed struct {
        // 0 implies that there are no children, as 0 can only be the root
        l: NodeIdx = 0,
        r: NodeIdx = 0,
    };

    pub const Tag = enum {
        /// the root of the tree
        root,
        /// a constant value like a number or interned string
        constant,
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
            std.debug.print("node[{}] expected {}, got {}\n", .{
                i,
                e.tag,
                n.tag,
            });

            return err;
        };
        std.testing.expectEqual(e.children, n.children) catch |err| {
            std.debug.print("node[{}] expected {}, got {}\n", .{
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
    const expected = [_]Node{ .{
        .tag = .root,
        .token_idx = 0,
        .children = .{ .l = 1 },
    }, .{
        .tag = .constant,
        .token_idx = 0,
        .children = .{},
    } };
    try ast.testAst(&expected);
    // assert that we stored this bad boy correctly
    try std.testing.expect(ast.getData(f32, ast.nodes.items[1].children.l) == 32.0);
}

// test "parse plus" {}
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
