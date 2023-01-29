const std = @import("std");

pub const ValueType = enum(u8) {
    float,
    empty,
    boolean,
    string,
};

pub const Pair = struct {
    car: Value,
    cdr: Value,
};

pub const String = struct {
    len: usize,
    bytes: [*]u8,

    pub fn init(allocator: std.mem.Allocator, str: []const u8) !*String {
        var ptr = try allocator.alignedAlloc(u8, @alignOf(String), @sizeOf(String) + str.len);

        var str_ptr = @ptrCast(*String, ptr);
        str_ptr.len = str.len;

        var bytes = @ptrCast([*]u8, &str_ptr.bytes);

        for (str) |b, i| {
            bytes[i] = b;
        }

        return str_ptr;
    }

    pub fn deinit(self: *String, allocator: std.mem.Allocator) void {
        const buf = @ptrCast([*]u8, self)[0 .. @sizeOf(String) + self.len];
        allocator.free(buf);
    }

    pub fn slice(self: *String) []const u8 {
        return @ptrCast([*]const u8, &self.bytes)[0..self.len];
    }
};

pub const Value = union(ValueType) {
    /// float/number type
    float: f32,
    /// nothing, nada, void, null
    empty: void,
    /// this value is true or false
    boolean: bool,
    /// string of characters
    string: *String,

    pub fn print(v: Value) void {
        switch (v) {
            .float => |f| std.debug.print("{d:.2}", .{f}),
            inline else => |inner| std.debug.print("{}", .{inner}),
        }
    }

    pub inline fn truthy(v: Value) bool {
        return switch (v) {
            .boolean => |b| b,
            else => true,
        };
    }

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

    pub inline fn lt(a: Value, b: Value) !Value {
        try assertType(.float, a);
        try assertType(.float, b);
        return Value{ .boolean = a.float < b.float };
    }

    pub inline fn lte(a: Value, b: Value) !Value {
        try assertType(.float, a);
        try assertType(.float, b);
        return Value{ .boolean = a.float <= b.float };
    }

    pub inline fn gt(a: Value, b: Value) !Value {
        try assertType(.float, a);
        try assertType(.float, b);
        return Value{ .boolean = a.float > b.float };
    }

    pub inline fn gte(a: Value, b: Value) !Value {
        try assertType(.float, a);
        try assertType(.float, b);
        return Value{ .boolean = a.float >= b.float };
    }
};

test "assert" {
    // const v = Value{ .float = 123 };
    // try Value.assertType(.float, v);
    std.debug.print("{}\n", .{@sizeOf(Value)});
    try std.testing.expect(@sizeOf(Value) == 16);
}

test "truthy" {
    const t = Value{ .boolean = true };
    const f = Value{ .boolean = false };
    const v = Value{ .float = 32 };

    try std.testing.expect(t.truthy() == true);
    try std.testing.expect(v.truthy() == true);
    try std.testing.expect(f.truthy() == false);
}
