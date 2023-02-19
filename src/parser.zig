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

const MAX_ARGS = 256;

pub const ParseError = error{
    UnexpectedCloseParen,
    UnexpectedTag,
    UnexpectedEof,
    ExpectedOpenParen,
    ExpectedCloseParen,
    ExpectedNumber,
    FormFailed,
    ExceededMaxArgs,
    ExpectedBegin,
    ExpectedIf,

    SyntaxError,
} || Allocator.Error || error{NotYetImplemented};

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
    while (!self.match(.eof)) {
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
        .tokens = try self.tokens.toOwnedSlice(),
        .nodes = try self.nodes.toOwnedSlice(),
        .data = try self.data.toOwnedSlice(),
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
        .false,
        .true,
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
        .not,
        => try self.parseSymbol(),
        .lparen => try self.parseForm(),
        .rparen => {
            std.debug.print("UnexpectedCloseParen: \n", .{});
            std.debug.print("prev:", .{});
            self.prev.print();
            std.debug.print(" curr:", .{});
            self.curr.print();
            std.debug.print("\n", .{});
            return ParseError.UnexpectedCloseParen;
        },
        else => {
            std.debug.print("UnexpectedTag: ", .{});
            self.curr.print();
            std.debug.print("\n", .{});
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
    try self.consume(.number, ParseError.ExpectedNumber);

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
    var b: bool = self.curr.tag == .true;
    const data_idx = try self.pushData(Value, .{ .boolean = b });

    // push the token
    const token_idx = @intCast(u32, self.tokens.items.len);
    try self.tokens.append(self.curr);
    self.advance();

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
    // need this advance because symbol could be something other
    // than a symbol
    self.advance();

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
    try self.consume(.lparen, ParseError.ExpectedOpenParen);

    const idx = switch (self.curr.tag) {
        .@"if" => try self.parseCond(),
        .begin => blk: {
            try self.consume(.begin, ParseError.ExpectedBegin);
            break :blk try self.parseSeq();
        },
        // TODO: should not be allowed inside an expression:
        // (let ((y (define x 5))) y)
        .define => try self.parseDefine(),
        .set => try self.parseSet(),
        .let => try self.parseLet(),
        // otherwise, it's safe to assume that this is
        // a not a special form and thus a call
        else => blk: {
            break :blk try self.parseApply();
        },
    };

    return idx;
}

pub fn parseDefine(
    self: *Parser,
) ParseError!NodeIdx {
    // add define node
    const idx = @intCast(NodeIdx, self.nodes.items.len);
    try self.nodes.append(.{
        .tag = .define,
        .children = .{},
        .token_idx = @intCast(NodeIdx, self.tokens.items.len),
    });
    try self.consume(.define, error.FormFailed);

    // this should be a symbol
    self.nodes.items[idx].children.l = try self.parseSymbol();

    // evaluate the body
    self.nodes.items[idx].children.r = try self.parseExpr();

    try self.consume(.rparen, ParseError.ExpectedCloseParen);

    return idx;
}

pub fn parseSet(
    self: *Parser,
) ParseError!NodeIdx {
    // add define node
    const idx = @intCast(NodeIdx, self.nodes.items.len);
    try self.nodes.append(.{
        .tag = .set,
        .children = .{},
        .token_idx = @intCast(NodeIdx, self.tokens.items.len),
    });
    try self.consume(.set, error.FormFailed);

    // this should be a symbol
    self.nodes.items[idx].children.l = try self.parseSymbol();

    // evaluate the body
    self.nodes.items[idx].children.r = try self.parseExpr();

    try self.consume(.rparen, ParseError.ExpectedCloseParen);

    return idx;
}

pub fn parseLet(
    self: *Parser,
) ParseError!NodeIdx {
    // save this for later when we have to patch the then and else branches
    const let_idx = @intCast(NodeIdx, self.nodes.items.len);
    // push the if statement to the tree
    try self.nodes.append(.{
        .tag = .let,
        .children = .{},
        .token_idx = @intCast(NodeIdx, self.tokens.items.len),
    });
    // should not failt to consume let
    try self.consume(.let, ParseError.SyntaxError);

    const bindings_list_idx = @intCast(NodeIdx, self.nodes.items.len);
    try self.nodes.append(.{
        .tag = .pair,
        .children = .{
            .l = 0,
            .r = 0,
        },
        .token_idx = @intCast(NodeIdx, self.tokens.items.len),
    });
    // beginning of assoc list
    try self.consume(.lparen, ParseError.ExpectedOpenParen);

    var tail_idx = bindings_list_idx;

    // first binding:
    while (!(self.match(.rparen) or self.match(.eof))) {
        var pair = &self.nodes.items[tail_idx];
        if (pair.children.l != 0) {
            // old pair right points to new one
            tail_idx = @intCast(NodeIdx, self.nodes.items.len);
            pair.children.r = tail_idx;

            // create the new on
            try self.nodes.append(.{
                .tag = .pair,
                .children = .{},
                .token_idx = @intCast(NodeIdx, self.tokens.items.len),
            });
        }
        const binding_idx = try self.parseBinding();
        self.nodes.items[tail_idx].children.l = binding_idx;
    }
    // self.nodes.items[tail_idx].token_idx = self.nodes.items[binding_idx].token_idx;

    // end of the list
    try self.consume(.rparen, ParseError.ExpectedCloseParen);

    const body_idx = try self.parseSeq();

    // set the left to the list
    self.nodes.items[let_idx].children.l = bindings_list_idx;
    self.nodes.items[let_idx].children.r = body_idx;

    return let_idx;
}

pub fn parseBinding(
    self: *Parser,
) ParseError!NodeIdx {
    // add the binding
    const binding_idx = @intCast(NodeIdx, self.nodes.items.len);
    try self.nodes.append(.{
        .tag = .pair,
        .children = .{
            .l = 0,
            .r = 0,
        },
        .token_idx = @intCast(NodeIdx, self.tokens.items.len),
    });
    try self.consume(.lparen, ParseError.ExpectedOpenParen);

    // variable
    const variable = try self.parseExpr();
    self.nodes.items[binding_idx].children.l = variable;

    // value
    const val = try self.parseExpr();
    self.nodes.items[binding_idx].children.r = val;

    // end of binding
    try self.consume(.rparen, ParseError.ExpectedCloseParen);

    return binding_idx;
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
    try self.consume(.@"if", ParseError.ExpectedIf);

    // cond
    _ = try self.parseExpr();

    const then_idx = try self.parseExpr();

    const else_idx = blk: {
        const else_tok = self.curr;
        if (else_tok.tag == .rparen) {
            break :blk 0;
        } else {
            const idx = try self.parseExpr();
            break :blk idx;
        }
    };

    self.nodes.items[if_idx].children.l = then_idx;
    self.nodes.items[if_idx].children.r = else_idx;
    try self.consume(.rparen, ParseError.ExpectedCloseParen);

    return if_idx;
}

pub fn parseSeq(
    self: *Parser,
) ParseError!NodeIdx {
    // const begin_tok_idx = @intCast(NodeIdx, self.tokens.items.len) - 1;
    const begin_seq_idx = @intCast(NodeIdx, self.nodes.items.len);
    var last_root_idx: NodeIdx = begin_seq_idx;
    var root_idx: NodeIdx = begin_seq_idx;
    while (true) {
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
                const tok_idx = @intCast(NodeIdx, self.tokens.items.len);

                try self.nodes.append(.{
                    .tag = .seq,
                    .token_idx = tok_idx,
                    .children = .{},
                });
                const id = try self.parseExpr();
                // modify the root to use the new found child
                self.nodes.items[root_idx].children = .{ .l = id };
            },
        }

        // self.nodes.items[begin_seq_idx].token_idx = begin_tok_idx;
    }

    try self.consume(.rparen, ParseError.ExpectedCloseParen);

    return begin_seq_idx;
}

pub fn parseApply(
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
        if (n_args >= MAX_ARGS) {
            return ParseError.ExceededMaxArgs;
        }
    }

    // now that we have the arguments list setup, we can create function data
    const data_idx = try self.pushData(FnCall, .{
        .n_args = n_args,
        .caller_idx = caller,
    });
    // and set the right child to the index
    self.nodes.items[call_idx].children.l = data_idx;

    try self.consume(.rparen, ParseError.ExpectedCloseParen);

    return call_idx;
}

test {
    std.testing.refAllDecls(@import("test_parser.zig"));
}
