const std = @import("std");

pub const Value = union {
    // TODO: make this more kinds of values
    // for now we will only work with immediate f32
    float: f32,

    pub inline fn add(a: Value, b: Value) !Value {
        return Value{ .float = a.float + b.float };
    }

    pub inline fn sub(a: Value, b: Value) !Value {
        return Value{ .float = a.float - b.float };
    }

    pub inline fn mul(a: Value, b: Value) !Value {
        return Value{ .float = a.float * b.float };
    }

    pub inline fn div(a: Value, b: Value) !Value {
        return Value{ .float = a.float / b.float };
    }

    pub inline fn eql(a: Value, b: Value) !Value {
        // return std.math.approxEqAbs(f32, a, b, std.math.epsilon(f32));
        return a.float == b.float;
    }
};
