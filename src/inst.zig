const std = @import("std");
/// Instructions that are our bytecode
/// all instructions are the same size (32 bits)
/// to maintain alignment
pub const Inst = union(enum) {
    // make sure our instructions are the right size
    comptime {
        std.debug.assert(@bitSizeOf(Arg3) == 24);
        std.debug.assert(@bitSizeOf(ArgS) == 24);
        std.debug.assert(@bitSizeOf(ArgU) == 24);
        std.debug.assert(@bitSizeOf(Inst) == 32);
        std.debug.assert(@sizeOf(Inst) == 4);
    }

    /// arguments for instruction with three arguments
    pub const Arg3 = packed struct {
        r: u8 = 0,
        r1: u8 = 0,
        r2: u8 = 0,
    };

    /// arguments for instruction with one signed argument
    pub const ArgS = packed struct {
        r: u8 = 0,
        s: i16 = 0,
    };

    /// arguments for instruction with an unsigned argument
    pub const ArgU = packed struct {
        r: u8 = 0,
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
    /// adds the immediate in s to a register and stores it in there
    addimm: ArgU,
    /// subtracts the values in the last two registers together and stores
    /// the result in the first
    sub: Arg3,
    /// subtracts the immediate in s to a register and stores it in there
    subimm: ArgU,
};
