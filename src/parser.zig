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

/// for allocating
allocator: Allocator,

/// scanner for getting source code
scanner: Scanner,

pub fn init(allocator: Allocator) Parser {
    return Parser{
        .allocator = allocator,
        .tokens = TokenList.init(allocator),
        .nodes = NodeList.init(allocator),
        .data = NodeDataList.init(allocator),
        .scanner = undefined,
    };
}

pub fn deinit(self: *Parser) void {
    self.nodes.deinit();
    self.tokens.deinit();
    self.data.deinit();
}

pub fn pushData(
    self: *Parser,
    comptime T: type,
    t: T,
) !NodeIdx {

    // allocate the required number of bytes
    const idx = @intCast(NodeIdx, self.data.items.len);
    var size: u8 = @sizeOf(T);
    const align_t = @alignOf(T);

    comptime std.debug.assert(align_t % DATA_ALIGN == 0);

    while (size > 0) : (size -= 1) {
        try self.data.append(0xaa);
    }

    const ptr = @ptrCast([*]T, @alignCast(align_t, &self.data.items[idx]));
    ptr[0] = t;

    return idx;
}

pub const ParseError = error{
    UndexpectedRightParen,
    UnexpectedTag,
    UnexpectedEof,

    SyntaxError,
} || Allocator.Error;

/// Parse the whole source into a tree
pub fn parse(self: *Parser, src: []const u8) ParseError!Ast {
    try self.nodes.append(.{
        .tag = .root,
        .token_idx = 0,
        .children = .{},
    });

    // TODO: ensure capacity so that we dont need try everywhere
    // create a scanner
    self.scanner = Scanner{
        .buf = src,
    };
    const first_tok = self.scanner.next();

    const id = try self.parseExpr(first_tok);
    // modify the root to use the new found child
    self.nodes.items[0].children = .{
        .l = id,
    };

    return Ast{
        .tokens = self.tokens.toOwnedSlice(),
        .nodes = self.nodes.toOwnedSlice(),
        .data = self.data.toOwnedSlice(),
    };
}

/// this is the first stop in our parsing journey.
/// here we parse the top level statements.
/// This should be just one when we are just inside a single sexpr
pub fn parseExpr(self: *Parser, tok: Token) ParseError!NodeIdx {
    // we done!
    if (tok.tag == .eof) return 0;
    return switch (tok.tag) {
        .number => try self.parseNum(tok),

        .@"false",
        .@"true",
        => try self.parseBool(tok),
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
        => try self.parseSymbol(tok),
        .lparen => try self.parseForm(tok),
        .rparen => ParseError.UndexpectedRightParen,
        else => {
            std.debug.print("UnexpectedTag: {}\n", .{tok.tag});
            return ParseError.UnexpectedTag;
        },
    };
}

// TODO: parse other kinds of numbers
pub fn parseNum(
    self: *Parser,
    tok: Token,
) ParseError!NodeIdx {
    // try to parse the number
    const f = std.fmt.parseFloat(f32, tok.loc.slice) catch {
        return ParseError.SyntaxError;
    };
    const data_idx = try self.pushData(Value, .{ .float = f });

    // push the token
    const token_idx = @intCast(u32, self.tokens.items.len);
    try self.tokens.append(tok);

    const node_idx = @intCast(u32, self.nodes.items.len);
    // parse the number into a value
    // push the node
    try self.nodes.append(.{
        .tag = .constant,
        .token_idx = token_idx,
        .children = .{ .l = data_idx },
    });
    return node_idx;
}

pub fn parseBool(
    self: *Parser,
    tok: Token,
) ParseError!NodeIdx {
    var b: bool = tok.tag == .@"true";
    const data_idx = try self.pushData(Value, .{ .boolean = b });

    // push the token
    const token_idx = @intCast(u32, self.tokens.items.len);
    try self.tokens.append(tok);

    const node_idx = @intCast(u32, self.nodes.items.len);
    // parse the number into a value
    // push the node
    try self.nodes.append(.{
        .tag = .constant,
        .token_idx = token_idx,
        .children = .{ .l = data_idx },
    });
    return node_idx;
}

pub fn parseSymbol(
    self: *Parser,
    tok: Token,
) ParseError!NodeIdx {
    // TODO: what data does a symbol have
    // const data_idx = try self.pushData(Value, .{ .float = f });

    // push the token
    const token_idx = @intCast(u32, self.tokens.items.len);
    try self.tokens.append(tok);

    const node_idx = @intCast(u32, self.nodes.items.len);
    // parse the number into a value
    // push the node
    try self.nodes.append(.{
        .tag = .symbol,
        .children = .{},
        .token_idx = token_idx,
    });
    return node_idx;
}

pub fn parseForm(
    self: *Parser,
    paren_tok: Token,
) ParseError!NodeIdx {
    // push the lparen token
    try self.tokens.append(paren_tok);

    const tok = self.scanner.next();
    return switch (tok.tag) {
        .@"if" => try self.parseCond(tok),
        // otherwise, it's safe to assume that this is
        // a not a special form and thus a call
        else => try self.parseCall(tok),
    };
}

/// parse a conditional statement
/// for now, this is just an if statement
pub fn parseCond(
    self: *Parser,
    if_tok: Token,
) ParseError!NodeIdx {

    // save this for later when we have to patch the then and else branches
    const if_idx = @intCast(NodeIdx, self.nodes.items.len);
    // push the if statement to the tree
    try self.nodes.append(.{
        .tag = .@"if",
        .children = .{},
        // INVARIANT: the latest token should be the lparen
        .token_idx = @intCast(NodeIdx, self.tokens.items.len),
    });
    try self.tokens.append(if_tok);

    // parse the cond expression
    _ = try self.parseExpr(self.scanner.next());

    const then_idx = try self.parseExpr(self.scanner.next());
    const else_idx = blk: {
        const else_tok = self.scanner.next();
        if (else_tok.tag == .rparen) {
            break :blk 0;
        } else {
            break :blk try self.parseExpr(else_tok);
        }
    };

    self.nodes.items[if_idx].children.l = then_idx;
    self.nodes.items[if_idx].children.r = else_idx;

    return if_idx;
}

pub fn parseCall(
    self: *Parser,
    caller_tok: Token,
) ParseError!NodeIdx {
    // create call node
    const call_idx = @intCast(NodeIdx, self.nodes.items.len);
    // get the symbol to call
    try self.nodes.append(.{
        .tag = .call,
        .children = .{},
        // INVARIANT: the latest token should be the lparen
        .token_idx = @intCast(NodeIdx, self.tokens.items.len - 1),
    });

    // get the thing we are calling
    const caller = try self.parseExpr(caller_tok);
    self.nodes.items[call_idx].children.l = caller;
    // self.nodes.items[call_idx].token_idx = self.nodes.items[caller].token_idx;

    // we start making a list by creating the first pair
    // var last: ?NodeIdx = null;
    var pair_idx = @intCast(NodeIdx, self.nodes.items.len);
    self.nodes.items[call_idx].children.r = pair_idx;
    try self.nodes.append(.{
        .tag = .pair,
        .token_idx = self.nodes.items[caller].token_idx,
        .children = .{},
    });

    var n_args: u8 = 0;

    while (true) {
        const tok = self.scanner.next();

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
                    var pair = &self.nodes.items[pair_idx];
                    if (pair.children.l != 0) {
                        // old pair right points to new one
                        pair_idx = @intCast(NodeIdx, self.nodes.items.len);
                        pair.children.r = pair_idx;

                        // create the new on
                        try self.nodes.append(.{
                            .tag = .pair,
                            .children = .{},
                            .token_idx = 0,
                        });
                    }
                }

                const n_idx = try self.parseExpr(tok);
                self.nodes.items[pair_idx].children.l = n_idx;
                self.nodes.items[pair_idx].token_idx = self.nodes.items[n_idx].token_idx;
                n_args += 1;
            },
        }
    }

    return call_idx;
}

test {
    std.testing.refAllDecls(@import("test_parser.zig"));
}
