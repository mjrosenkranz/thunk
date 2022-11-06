const std = @import("std");

pub const ValueType = enum(u8) {
    float,
    empty,
    boolean,
};

pub const Pair = struct {
    car: Value,
    cdr: Value,
};

pub const String = struct {
    len: usize,
    char: [:0]u8,
};

pub const ObjType = enum(u4) {
    pair,
    string,

    pub fn Type(comptime self: @This()) type {
        return switch (self) {
            .pair => Pair,
            .string => String,
        };
    }
};

pub const Obj = packed struct(usize) {
    ot: ObjType,
    ptr_int: u60,

    pub fn ptr(self: @This(), comptime T: type) *T {
        var ret_ptr: usize = @as(usize, self.ptr_int) << @bitSizeOf(ObjType);
        return @intToPtr(*T, ret_ptr);
    }

    pub fn fromPtr(comptime ot: ObjType, o_ptr: *align(4) ot.Type()) @This() {
        return .{
            .ot = ot,
            .ptr_int = @truncate(u60, @ptrToInt(o_ptr) >> @bitSizeOf(ObjType)),
        };
    }
};

pub const Value = union(ValueType) {
    /// float/number type
    float: f32,
    /// nothing, nada, void, null
    empty: void,
    /// this value is true or false
    boolean: bool,
    /// heap obj
    /// stores a pointer to a pair on the heap
    // pair: *Pair,
    /// string
    // string: []const u8,

    pub inline fn sameType(a: Value, b: Value) bool {
        return std.meta.activeTag(a) == std.meta.activeTag(b);
    }

    pub inline fn assertSameType(a: Value, b: Value) !void {
        if (!sameType(a, b)) return error.NotSameType;
    }

    pub inline fn isType(t: ValueType, v: Value) bool {
        return std.meta.activeTag(v) == t;
    }

    pub inline fn assertType(t: ValueType, v: Value) !void {
        if (!isType(t, v)) return error.NotExpectedType;
    }

    pub inline fn add(a: Value, b: Value) !Value {
        try assertType(.float, a);
        try assertType(.float, b);
        return Value{ .float = a.float + b.float };
    }

    pub inline fn sub(a: Value, b: Value) !Value {
        try assertType(.float, a);
        try assertType(.float, b);
        return Value{ .float = a.float - b.float };
    }

    pub inline fn mul(a: Value, b: Value) !Value {
        try assertType(.float, a);
        try assertType(.float, b);
        return Value{ .float = a.float * b.float };
    }

    pub inline fn div(a: Value, b: Value) !Value {
        try assertType(.float, a);
        try assertType(.float, b);
        return Value{ .float = a.float / b.float };
    }

    pub inline fn eql(a: Value, b: Value) !Value {
        return Value{
            .boolean = switch (a) {
                .float => a.float == b.float,
                .boolean => a.bool == b.bool,
                else => return error.InvalidType,
            },
        };
    }
};

test "assert" {
    // const v = Value{ .float = 123 };
    // try Value.assertType(.float, v);
    std.debug.print("{}\n", .{@sizeOf(Value)});
    try std.testing.expect(@sizeOf(Value) == 8);
}

test "ptr" {
    const alloc = std.testing.allocator;

    var pairs = try alloc.alignedAlloc(Pair, 4, 1);
    defer alloc.free(pairs);
    var pair = &pairs[0];
    pair.* = Pair{
        .car = Value.empty,
        .cdr = Value.empty,
    };
    var obj = Obj.fromPtr(.pair, pair);
    try std.testing.expect(@ptrToInt(obj.ptr(Pair)) == @ptrToInt(pair));
}
