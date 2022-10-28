const std = @import("std");

pub const ValueType = enum {
    float,
    nil,
    boolean,
};

pub const Value = union(ValueType) {
    /// float/number type
    float: f32,
    /// nothing, nada, void, null
    nil: void,
    /// this value is true or false
    boolean: bool,

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
        Value{ .float = a.float / b.float };
    }

    pub inline fn eql(a: Value, b: Value) !Value {
        return Value{
            .boolean = switch (a) {
                .float => a.float == b.float,
                else => return error.InvalidType,
            },
        };
    }
};

test "assert" {
    const v = Value{ .float = 123 };
    try Value.assertType(.float, v);
}
