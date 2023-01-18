//! experiments for api and such
const std = @import("std");

// pub const Fn = struct {
//     calling_convention: CallingConvention,
//     alignment: comptime_int,
//     is_generic: bool,
//     is_var_args: bool,
//     /// TODO change the language spec to make this not optional.
//     return_type: ?type,
//     args: []const Param,

//     /// This data structure is used by the Zig language code generation and
//     /// therefore must be kept in sync with the compiler implementation.
//     pub const Param = struct {
//         is_generic: bool,
//         is_noalias: bool,
//         arg_type: ?type,
//     };
// };

pub const ObjType = enum {
    num,
};

/// change?
pub const MAX_ARGS = 32;

pub const NativeFnDef = struct {
    args: [MAX_ARGS]ObjType = undefined,
    n_args: u8 = 0,
    ptr: *const anyopaque = undefined,

    var F: type = undefined;

    pub fn call(f: @This(), args: []Obj) void {
        std.debug.assert(f.n_args == args.len);
        @call(.{}, F, .{inline for (f.args) |arg_t, i| {
            args[i].untag(arg_t);
        }});
    }
};

fn defn(comptime f: anytype) void {
    // make sure this is a function
    // throw compile error if not?
    // add to a list of provided functions
    const F = @TypeOf(f);
    const info = @typeInfo(F);

    var def = NativeFnDef{};
    comptime for (info.Fn.args) |p, i| {
        if (p.arg_type) |T| {
            def.args[i] = switch (@typeInfo(T)) {
                .Float => ObjType.num,
                else => @compileError("unsupported native type"),
            };
            def.n_args += 1;
        }
    };

    return def;
}

const Obj = f32;

fn add(a: Obj, b: Obj) Obj {
    return a + b;
}

test "defn" {
    std.debug.print("{}\n", .{defn(add)});
}
