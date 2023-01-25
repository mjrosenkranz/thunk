const std = @import("std");
const Value = @import("value.zig").Value;
const Allocator = std.mem.Allocator;
const Scanner = @import("scanner.zig").Scanner;
const Token = @import("token.zig");
const Ast = @import("ast.zig");
const FnCall = Ast.FnCall;
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

/// previously visited token
prev: Token,

/// token we are currently checkin out
curr: Token,

pub fn init(allocator: Allocator) Parser {
    return Parser{
        .curr = undefined,
        .prev = undefined,
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

/// gets us the next token in current and saves the previous
pub inline fn advance(self: *Parser) void {
    self.prev = self.curr;
    self.curr = self.scanner.next();
}

/// shorthand for if the current token tag is expected
pub inline fn match(self: Parser, tag: Token.Tag) bool {
    return self.curr.tag == tag;
}

/// we expect the current token to have tag, if not we throw err
pub inline fn consume(self: *Parser, tag: Token.Tag, err: ParseError) ParseError!void {
    if (self.match(tag)) {
        try self.tokens.append(self.curr);
        self.advance();
        return;
    }

    std.debug.print("expected: {} got {}\n", .{ tag, self.curr.tag });
    return err;
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
    ExpectedCloseParen,
    ExpectedNumber,
    FormFailed,

    SyntaxError,
} || Allocator.Error;

/// Parse the whole source into a tree
pub fn parse(self: *Parser, src: []const u8) ParseError!Ast {
    // TODO: ensure capacity so that we dont need try everywhere
    // create a scanner
    self.scanner = Scanner{
        .buf = src,
    };
    self.advance();
    var last_root_idx: NodeIdx = 0;
    var root_idx: NodeIdx = 0;
    while (!self.match(.eof)) : (self.advance()) {
        root_idx = @intCast(NodeIdx, self.nodes.items.len);
        if (root_idx != 0) {
            self.nodes.items[last_root_idx].children.r = root_idx;
        }
        try self.nodes.append(.{
            .tag = .seq,
            .token_idx = @intCast(NodeIdx, self.tokens.items.len),
            .children = .{},
        });
        const id = try self.parseExpr();
        // modify the root to use the new found child
        self.nodes.items[root_idx].children = .{ .l = id };
    }

    return Ast{
        .tokens = self.tokens.toOwnedSlice(),
        .nodes = self.nodes.toOwnedSlice(),
        .data = self.data.toOwnedSlice(),
    };
}

/// this is the first stop in our parsing journey.
/// here we parse the top level statements.
/// This should be just one when we are just inside a single sexpr
pub fn parseExpr(self: *Parser) ParseError!NodeIdx {
    // we done!
    if (self.curr.tag == .eof) return 0;
    return switch (self.curr.tag) {
        .number => try self.parseNum(),
        .@"false",
        .@"true",
        => try self.parseBool(),
        // these are all builtin symbols
        .plus,
        .minus,
        .asterisk,
        .slash,
        .modulo,
        .gt,
        .lt,
        .gte,
        .lte,
        .symbol,
        .@"and",
        .@"or",
        .@"not",
        => try self.parseSymbol(),
        .lparen => try self.parseForm(),
        .rparen => {
            return ParseError.UndexpectedRightParen;
        },
        else => {
            std.debug.print("UnexpectedTag: {}\n", .{self.curr.tag});
            return ParseError.UnexpectedTag;
        },
    };
}

// TODO: parse other kinds of numbers
pub fn parseNum(
    self: *Parser,
) ParseError!NodeIdx {
    // try to parse the number
    const f = std.fmt.parseFloat(f32, self.curr.loc.slice) catch {
        return ParseError.SyntaxError;
    };
    const data_idx = try self.pushData(Value, .{ .float = f });

    // push the token
    const token_idx = @intCast(u32, self.tokens.items.len);
    try self.tokens.append(self.curr);

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
) ParseError!NodeIdx {
    var b: bool = self.curr.tag == .@"true";
    const data_idx = try self.pushData(Value, .{ .boolean = b });

    // push the token
    const token_idx = @intCast(u32, self.tokens.items.len);
    try self.tokens.append(self.curr);

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
) ParseError!NodeIdx {
    // TODO: what data does a symbol have
    // const data_idx = try self.pushData(Value, .{ .float = f });

    // push the token
    const token_idx = @intCast(u32, self.tokens.items.len);
    try self.tokens.append(self.curr);

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
) ParseError!NodeIdx {
    // push the lparen token
    try self.tokens.append(self.curr);

    self.advance();
    const idx = switch (self.curr.tag) {
        .@"if" => try self.parseCond(),
        .begin => try self.parseSeq(),
        .define => try self.parseDefine(),
        // otherwise, it's safe to assume that this is
        // a not a special form and thus a call
        else => blk: {
            break :blk try self.parseCall();
        },
    };

    return idx;
}

pub fn parseDefine(
    self: *Parser,
) ParseError!NodeIdx {
    // we know curr is define so we can advance
    self.advance();
    // add define node
    const idx = @intCast(NodeIdx, self.nodes.items.len);
    try self.nodes.append(.{
        .tag = .define,
        .children = .{},
        .token_idx = @intCast(NodeIdx, self.tokens.items.len),
    });
    try self.tokens.append(self.curr);

    // this should be a symbol
    self.nodes.items[idx].children.l = try self.parseSymbol();
    self.advance();

    // evaluate the body
    self.nodes.items[idx].children.r = try self.parseExpr();
    self.advance();

    try self.consume(.rparen, ParseError.ExpectedCloseParen);

    return idx;
}

/// parse a conditional statement
/// for now, this is just an if statement
pub fn parseCond(
    self: *Parser,
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
    try self.tokens.append(self.curr);

    // parse the cond expression
    self.advance();
    _ = try self.parseExpr();

    self.advance();
    const then_idx = try self.parseExpr();
    const else_idx = blk: {
        self.advance();
        const else_tok = self.curr;
        if (else_tok.tag == .rparen) {
            break :blk 0;
        } else {
            const idx = try self.parseExpr();
            // make sure that the next token is rparen
            self.advance();
            if (!self.match(.rparen)) {
                return ParseError.ExpectedCloseParen;
            }

            break :blk idx;
        }
    };

    self.nodes.items[if_idx].children.l = then_idx;
    self.nodes.items[if_idx].children.r = else_idx;

    return if_idx;
}

pub fn parseSeq(
    self: *Parser,
) ParseError!NodeIdx {
    const begin_seq_idx = @intCast(NodeIdx, self.nodes.items.len);
    const begin_tok_idx = @intCast(NodeIdx, self.tokens.items.len);
    try self.tokens.append(self.curr);
    // try self.nodes.append(.{
    //     .tag = .seq,
    //     .token_idx = ,
    //     .children = .{},
    // });

    var last_root_idx: NodeIdx = begin_seq_idx;
    var root_idx: NodeIdx = begin_seq_idx;
    while (true) {
        self.advance();
        switch (self.curr.tag) {
            // we at the end of the sequence
            .rparen => {
                break;
            },
            // we expect at least a right paren, this means that we have bad input
            .eof => return ParseError.UnexpectedEof,
            else => {
                root_idx = @intCast(NodeIdx, self.nodes.items.len);
                if (root_idx != begin_seq_idx) {
                    self.nodes.items[last_root_idx].children.r = root_idx;
                }
                try self.nodes.append(.{
                    .tag = .seq,
                    .token_idx = @intCast(NodeIdx, self.tokens.items.len),
                    .children = .{},
                });
                const id = try self.parseExpr();
                // modify the root to use the new found child
                self.nodes.items[root_idx].children = .{ .l = id };
            },
        }
    }

    self.nodes.items[begin_seq_idx].token_idx = begin_tok_idx;

    return begin_seq_idx;
}

pub fn parseCall(
    self: *Parser,
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
    const caller = try self.parseExpr();

    // we start making a list by creating the first pair
    var pair_idx = @intCast(NodeIdx, self.nodes.items.len);
    try self.nodes.append(.{
        .tag = .pair,
        .token_idx = self.nodes.items[caller].token_idx,
        .children = .{},
    });
    // add the pair to the call
    self.nodes.items[call_idx].children.r = pair_idx;

    var n_args: u8 = 0;

    while (true) {
        self.advance();

        switch (self.curr.tag) {
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

                const n_idx = try self.parseExpr();
                self.nodes.items[pair_idx].children.l = n_idx;
                self.nodes.items[pair_idx].token_idx = self.nodes.items[n_idx].token_idx;
                n_args += 1;
            },
        }
    }

    // now that we have the arguments list setup, we can create function data
    const data_idx = try self.pushData(FnCall, .{
        .n_args = n_args,
        .caller_idx = caller,
    });
    // and set the right child to the index
    self.nodes.items[call_idx].children.l = data_idx;

    return call_idx;
}

test {
    std.testing.refAllDecls(@import("test_parser.zig"));
}
