const std = @import("std");
/// Insructions that are our bytecode
/// all instructions are the same size (32 bits)
/// to maintain alignment
pub const Inst = union(Op) {
    // make sure our instructions are the right size
    comptime {
        std.debug.assert(@bitSizeOf(Arg3) == 24);
        std.debug.assert(@bitSizeOf(ArgS) == 24);
        std.debug.assert(@bitSizeOf(ArgU) == 24);
        std.debug.assert(@bitSizeOf(Inst) == 32);
        std.debug.assert(@sizeOf(Inst) == 4);
    }

    /// OpCode for each instruction we support
    pub const Op = enum {
        ret,
        load,
        add,
    };

    /// arguments for instruction with three arguments
    pub const Arg3 = packed struct {
        a: u8 = 0,
        b: u8 = 0,
        c: u8 = 0,
    };

    /// arguments for instruction with one signed argument
    pub const ArgS = packed struct {
        a: u8 = 0,
        s: i16 = 0,
    };

    /// arguments for instruction with an unsigned argument
    pub const ArgU = packed struct {
        a: u8 = 0,
        u: u16 = 0,
    };

    const Self = @This();

    /// returns values in the registers a to b
    ret: Arg3,
    /// loads immediate value stored at u into register a
    load: ArgU,
    /// adds the values in the last two registers together and stores
    /// the result in the first
    add: Arg3,
};
