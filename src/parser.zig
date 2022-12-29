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

pub fn init(allocator: Allocator) Parser {
    return Parser{
        .allocator = allocator,
        .tokens = TokenList.init(allocator),
        .nodes = NodeList.init(allocator),
        .data = NodeDataList.init(allocator),
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

const ParseError = error{
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
    var scanner = Scanner.init(src);
    const first_tok = scanner.next();

    const id = try self.parseExpr(first_tok, &scanner);
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
pub fn parseExpr(self: *Parser, tok: Token, scanner: *Scanner) ParseError!NodeIdx {
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
        .lparen => try self.parseForm(tok, scanner),
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
    scanner: *Scanner,
) ParseError!NodeIdx {
    // push the lparen token
    try self.tokens.append(paren_tok);

    const tok = scanner.next();
    return switch (tok.tag) {
        // otherwise, it's safe to assume that this is
        // a not a special form and thus a call
        else => try self.parseCall(tok, scanner),
    };
}

pub fn parseCall(
    self: *Parser,
    caller_tok: Token,
    scanner: *Scanner,
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
    const caller = try self.parseExpr(caller_tok, scanner);
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

                const n_idx = try self.parseExpr(tok, scanner);
                self.nodes.items[pair_idx].children.l = n_idx;
                self.nodes.items[pair_idx].token_idx = self.nodes.items[n_idx].token_idx;
                n_args += 1;
            },
        }
    }

    return call_idx;
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

test "call no args" {
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
            .tag = .call,
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

test "call no close" {
    const code =
        \\(+ 32 44
    ;
    var parser = Parser.init(std.testing.allocator);
    defer parser.deinit();

    try std.testing.expectError(ParseError.UnexpectedEof, parser.parse(code));
}

test "call one arg" {
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
            .tag = .call,
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

test "call two args" {
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
            .tag = .call,
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

test "call many" {
    const code =
        \\(+ 32 25 44)
    ;
    var parser = Parser.init(std.testing.allocator);
    defer parser.deinit();
    var ast = try parser.parse(code);
    defer ast.deinit(std.testing.allocator);
    // root -> call -> +
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
            .tag = .call,
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

test "call nested" {
    const code =
        //01 2 34 5 6
        \\(* 3 (+ 4 5))
    ;
    var parser = Parser.init(std.testing.allocator);
    defer parser.deinit();
    var ast = try parser.parse(code);
    defer ast.deinit(std.testing.allocator);
    // 0       1        2
    // root -> call -> *
    //         |    3 4
    //         |--> [ 3 | . ]
    //                    |    5
    //                    |--> [ . |   ]
    //                           |     6       7
    //                           |--> call -> +
    //                                 |    8 9
    //                                 |--> [ 4 | . ]
    //                                            |   10 11
    //                                            |--> [ 5 |   ]
    const expected = [_]Node{
        .{
            .tag = .root,
            .token_idx = 0,
            .children = .{ .l = 1 },
        },
        // 1
        .{
            .tag = .call,
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
            .children = .{ .l = 6, .r = 0 },
        },

        // 6
        .{
            .tag = .call,
            .token_idx = 3,
            .children = .{
                .l = 7,
                .r = 8,
            },
        },
        // 7
        .{
            .tag = .symbol,
            .children = .{},
            .token_idx = 4,
        },
        // 8
        .{
            .tag = .pair,
            .token_idx = 5,
            .children = .{ .l = 9, .r = 10 },
        },
        // 9
        .{
            .tag = .constant,
            .token_idx = 5,
            .children = .{ .l = 1 * @sizeOf(Value) },
        },
        // 10
        .{
            .tag = .pair,
            .token_idx = 6,
            .children = .{ .l = 11, .r = 0 },
        },
        // 11
        .{
            .tag = .constant,
            .token_idx = 6,
            .children = .{ .l = 2 * @sizeOf(Value) },
        },
    };
    try ast.testAst(&expected);
    // assert that we stored this bad boy correctly
    try std.testing.expect(ast.getData(Value, ast.nodes[4].children.l).float == 3.0);
    try std.testing.expect(ast.getData(Value, ast.nodes[9].children.l).float == 4.0);
    try std.testing.expect(ast.getData(Value, ast.nodes[11].children.l).float == 5.0);
}

// TODO: change this when we have lamda syntax
test "call lambda" {
    const code =
        //01 2 3  4 5
        \\((fn #t) #f)
    ;
    var parser = Parser.init(std.testing.allocator);
    defer parser.deinit();
    var ast = try parser.parse(code);
    defer ast.deinit(std.testing.allocator);
    // 0       1        2      3
    // root -> call -> call -> fn
    //         |        |    4 5
    //         |        |--> [ x |  ]
    //         |    6 7
    //         |--> [ 32 |   ]

    const expected = [_]Node{
        // 0
        .{
            .tag = .root,
            .token_idx = 0,
            .children = .{ .l = 1 },
        },
        // 1
        .{
            .tag = .call,
            .token_idx = 0,
            .children = .{
                .l = 2,
                .r = 6,
            },
        },
        // 2
        .{
            .tag = .call,
            .token_idx = 1,
            .children = .{
                .l = 3,
                .r = 4,
            },
        },
        // 3
        .{
            .tag = .symbol,
            .children = .{},
            .token_idx = 2,
        },
        // 4
        .{
            .tag = .pair,
            .token_idx = 3,
            .children = .{ .l = 5 },
        },
        // 5
        .{
            .tag = .constant,
            .token_idx = 3,
            .children = .{ .l = 0 },
        },
        // 6
        .{
            .tag = .pair,
            .token_idx = 4,
            .children = .{ .l = 7 },
        },
        // 7
        .{
            .tag = .constant,
            .token_idx = 4,
            .children = .{ .l = 16 },
        },
    };

    try ast.testAst(&expected);
    try std.testing.expect(ast.getData(Value, ast.nodes[5].children.l).boolean == true);
    try std.testing.expect(ast.getData(Value, ast.nodes[7].children.l).boolean == false);
}
