const std = @import("std");
const Value = @import("value.zig").Value;
const Allocator = std.mem.Allocator;
const Scanner = @import("scanner.zig").Scanner;
const Token = @import("token.zig");
const SrcLoc = @import("file.zig").SrcLoc;

pub const Ast = @This();

pub const DATA_ALIGN = 8;

/// all the nodes in this tree
nodes: []Node,

/// all the tokens from which these nodes were created
tokens: []Token,

/// extra data for our nodes
data: []align(DATA_ALIGN) u8,

pub fn deinit(ast: *Ast, allocator: Allocator) void {
    allocator.free(ast.nodes);
    allocator.free(ast.tokens);
    allocator.free(ast.data);
}

/// gets typed data from the tree
pub fn getData(ast: Ast, comptime T: type, idx: NodeIdx) T {
    const align_t = @alignOf(T);
    const ptr = @ptrCast([*]T, @alignCast(align_t, &ast.data[idx]));
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
        /// the left child is the byte offset of the constant's
        /// value in the data list
        constant,
        /// a symbol which may be bound to a value
        /// the left child is the index of the start of the symbols's
        /// name in the data list
        symbol,
        /// the application of a procedure with params
        /// l points to the symbol we are calling
        /// r points to the first argument
        /// if r is 0 then there are no arguments
        call,
        /// basically a linked list
        /// l is the value at this point in the list
        /// r is the next item in the list
        /// if r is zero wer're at the end of the list
        pair,
        /// an if statement
        /// ASSUMTION: the condition is the next node in the tree list
        /// l is the then branch
        /// r is the else branch
        @"if",
    };

    pub fn pprint(n: Node, i: NodeIdx, ast: *const Ast) void {
        switch (n.tag) {
            .root => {
                std.debug.print("<ast: \n", .{});
                ast.nodes[n.children.l].pprint(n.children.l, ast);
                std.debug.print("\n>\n", .{});
            },
            .constant => {
                // TODO: the alignment of the value may be wrong,
                // so get the offset first
                const off = n.children.l;
                std.debug.print("(const ", .{});
                ast.getData(Value, off).print();
                std.debug.print(")", .{});
            },
            .symbol => {
                std.debug.print("(symbol {s})", .{ast.tokens[n.token_idx].loc.slice});
            },
            .@"if" => {
                std.debug.print("(if ", .{});
                // cond
                ast.nodes[i + 1].pprint(i + 1, ast);
                std.debug.print("\n    ", .{});
                // then
                ast.nodes[n.children.l].pprint(n.children.l, ast);
                std.debug.print("\n    ", .{});
                // else
                ast.nodes[n.children.r].pprint(n.children.r, ast);
                // TODO: cond and stuff
                std.debug.print(")", .{});
            },
            .call => {
                std.debug.print("(call ", .{});
                ast.nodes[n.children.l].pprint(n.children.l, ast);
                std.debug.print(" ", .{});
                ast.nodes[n.children.r].pprint(n.children.r, ast);
                std.debug.print(")", .{});
            },
            .pair => {
                if (n.children.l != 0) {
                    ast.nodes[n.children.l].pprint(n.children.l, ast);
                }
                if (n.children.r != 0) {
                    std.debug.print(" ", .{});
                    ast.nodes[n.children.r].pprint(n.children.r, ast);
                }
            },
        }
    }
};

pub fn testAst(ast: Ast, expected: []const Node) !void {
    if (ast.nodes.len != expected.len) {
        std.debug.print("expected {} nodes, got {}\n", .{ expected.len, ast.nodes.len });
        try std.testing.expect(false);
    }

    // start with the first node since 0 is always root
    for (ast.nodes) |n, i| {
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
        std.testing.expectEqual(e.token_idx, n.token_idx) catch |err| {
            // std.debug.print("token_idx missmatch: node[{}] expected [{}]{}, got [{}]{}\n", .{
            std.debug.print("token_idx missmatch: node[{}] expected {}, got {}\n", .{
                i,
                e.token_idx,
                // ast.tokens[e.token_idx].tag,
                n.token_idx,
                // ast.tokens[n.token_idx].tag,
            });
            return err;
        };
    }
}

pub fn print(ast: Ast) void {
    std.debug.print("tree:\n", .{});
    for (ast.nodes) |n, i| {
        std.debug.print("{} {}[l: {} r: {}]\n", .{
            i,
            n.tag,
            n.children.l,
            n.children.r,
        });
    }
}

pub fn pprint(ast: Ast) void {
    std.debug.print("tree:\n", .{});
    ast.nodes[0].pprint(@intCast(NodeIdx, 0), &ast);
}

pub fn printTokens(ast: Ast) void {
    std.debug.print("tokens:\n", .{});
    for (ast.tokens) |t, i| {
        std.debug.print("[{}]: ", .{i});
        t.print();
    }
}
