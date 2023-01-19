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

/// change?
pub const MAX_ARGS = 32;

const CallFn = *const fn (self: NativeFnDef, args: []const Obj) ?Obj;

pub const NativeFnDef = struct {
    args: [MAX_ARGS]ObjType = undefined,
    n_args: u8 = 0,
    ptr: CallFn = undefined,

    pub fn call(self: @This(), args: []const Obj) ?Obj {
        return self.ptr(self, args);
    }
};

const nums = [_][]const u8{
    "0",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "10",
    "11",
    "12",
    "13",
    "14",
    "15",
    "16",
    "17",
    "18",
    "19",
    "20",
    "21",
    "22",
    "23",
    "24",
    "25",
    "26",
    "27",
    "28",
    "29",
    "30",
    "31",
    "32",
};

fn defn(comptime f: anytype) NativeFnDef {
    // make sure this is a function
    // throw compile error if not?
    // add to a list of provided functions
    const F = @TypeOf(f);
    const info = @typeInfo(F).Fn;

    var def = NativeFnDef{};
    comptime for (info.args) |p, i| {
        if (p.arg_type) |T| {
            def.args[i] = switch (@typeInfo(T)) {
                .Float => .float,
                .Bool => .boolean,
                else => @compileError("unsupported native type"),
            };
            def.n_args += 1;
        }
    };

    const closure = struct {
        fn func(d: NativeFnDef, args: []const Obj) ?Obj {
            std.debug.assert(d.n_args == args.len);

            comptime var fields: [info.args.len]std.builtin.Type.StructField = undefined;
            inline for (info.args) |arg, i| {
                fields[i] = std.builtin.Type.StructField{
                    .name = nums[i],
                    .field_type = arg.arg_type.?,
                    .is_comptime = true,
                    .default_value = null,
                    .alignment = @alignOf(arg.arg_type.?),
                };
            }
            const st = std.builtin.Type{
                .Struct = .{
                    .layout = .Auto,
                    .fields = &fields,
                    .decls = &[_]std.builtin.Type.Declaration{},
                    .is_tuple = true,
                },
            };

            const T = @Type(st);
            var t: T = undefined;
            inline for (std.meta.fields(T)) |fi, i| {
                @field(t, fi.name) = args[i].to(fi.field_type).?;
            }

            const ret = Obj.from(@call(.{}, f, t));
            if (info.return_type == void) {
                return null;
            } else {
                return ret;
            }
        }
    };
    def.ptr = closure.func;

    return def;
}

const HeapObjType = enum {};

const HeapObj = struct {
    tag: HeapObjType,
};

pub const ObjType = enum(u8) {
    empty,
    float,
    boolean,
    heap,
};

pub const Obj = packed struct(u64) {
    /// opaque data of the object
    data: u56 = 0,
    /// the type of the object
    tag: ObjType,

    pub fn from(o: anytype) Obj {
        const T = @TypeOf(o);
        return switch (T) {
            comptime_float,
            f32,
            => .{
                .tag = .float,
                .data = @as(u56, @bitCast(u32, @as(f32, o))),
            },
            bool => .{
                .tag = .boolean,
                .data = @as(u56, @boolToInt(o)),
            },
            void => {
                return .{ .tag = .empty };
            },
            else => {
                std.debug.print("\ninvalid type {}!\n", .{T});
                unreachable;
            },
        };
    }

    pub fn to(o: @This(), comptime T: type) ?T {
        switch (T) {
            f32 => {
                if (o.tag == .float) {
                    return @bitCast(f32, @truncate(u32, o.data));
                }
            },
            bool => {
                if (o.tag == .boolean) {
                    return o.data == 1;
                }
            },
            else => {
                std.debug.print("\ninvalid type {}!\n", .{T});
                unreachable;
            },
        }

        return null;
    }
};

fn add(a: f32, b: f32) f32 {
    return a + b;
}

fn effect(a: bool) void {
    std.debug.print("effect: {}\n", .{a});
}

test "defn" {
    const f = defn(add);
    const ret = f.call(&.{ Obj.from(32.0), Obj.from(55.0) }).?;
    try std.testing.expect(ret.to(f32) == @as(f32, 87.0));

    const f2 = defn(effect);
    try std.testing.expect(f2.call(&.{Obj.from(false)}) == null);

    // const info = @typeInfo(@TypeOf(.{ 32, 44 })).Struct;
    // std.debug.print("{}\n", .{info.is_tuple});
    // inline for (info.fields) |f| {
    //     std.debug.print("{s}: {}\n", .{ f.name, f.field_type });
    // }
}
