const std = @import("std");
/// virtual machine for interpreting our op-codes
pub const VM = struct {
    pub fn init() VM {
        return .{};
    }

    pub fn deinit(self: VM) void {
        _ = self;
    }
};

const Instr = packed struct {
    // make sure our instructions are the right size
    comptime {
        std.debug.assert(@bitSizeOf(Data) == 24);
        std.debug.assert(@bitSizeOf(Instr) == 32);
    }

    /// OpCode for each instruction we support
    pub const Op = enum(u8) {
        /// return from a function
        ret,
    };

    pub const Data = extern union {
        /// data for instruction with three arguments
        Instr3: packed struct {
            a: u8,
            b: u8,
            c: u8,
        },

        /// data for instruction with signed argument
        InstrS: packed struct {
            a: u8,
            s: i16,
        },

        /// data for instruction with unsigned argument
        InstrU: packed struct {
            a: u8,
            u: u16,
        },
    };

    op: Op,
    data: Data,
};

test "init" {
    var vm = VM.init();
    defer vm.deinit();

    var i: Instr = undefined;
    _ = i;
}
