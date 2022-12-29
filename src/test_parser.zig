const std = @import("std");
const Value = @import("value.zig").Value;
const Parser = @import("parser.zig");
const Ast = @import("ast.zig");
const NodeIdx = Ast.NodeIdx;
const Node = Ast.Node;

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

    try std.testing.expectError(Parser.ParseError.UnexpectedEof, parser.parse(code));
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