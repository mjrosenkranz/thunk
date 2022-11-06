const std = @import("std");

/// backing int of an opcode
pub const OpSize = u8;
/// backing int of an argument struct
pub const ArgSize = u24;
/// backing int of a full instruction
pub const InstSize = u32;

/// instructions enum
pub const Op = enum(OpSize) {
    ret,
    load,
    add,
    addimm,
    mul,
    mulimm,
    sub,
    subimm,
    div,
    divimm,
    define_global,
};

/// Typesafe register arguments
pub const Reg = u8;

/// arguments for instruction with three arguments
pub const Arg3 = packed struct(ArgSize) {
    r2: Reg = 0,
    r1: Reg = 0,
    r: Reg = 0,
};

/// arguments for instruction with one signed argument
pub const ArgS = packed struct(ArgSize) {
    s: i16 = 0,
    r: Reg = 0,
};

/// arguments for instruction with an unsigned argument
pub const ArgU = packed struct(ArgSize) {
    u: u16 = 0,
    r: Reg = 0,
};

pub const Inst = packed struct(InstSize) {
    data: ArgSize,
    op: Op,

    const Self = @This();

    pub inline fn init(comptime op: Op, arg: instType(op)) Self {
        return .{
            .op = op,
            .data = @bitCast(ArgSize, arg),
        };
    }

    pub inline fn argu(self: Self) ArgU {
        return @bitCast(ArgU, self.data);
    }

    pub inline fn args(self: Self) ArgS {
        return @bitCast(ArgS, self.data);
    }

    pub inline fn arg3(self: Self) Arg3 {
        return @bitCast(Arg3, self.data);
    }

    pub inline fn instType(comptime op: Op) type {
        return switch (op) {
            // returns values in the registers a to b
            .ret => Arg3,
            // loads immediate value stored at u into register a
            .load => ArgU,
            // adds the values in the last two registers together and stores
            // the result in the first
            .add => Arg3,
            // adds the immediate in s to a register and stores it in there
            .addimm => ArgU,
            // multiplies the values in the last two registers together and stores
            // the result in the first
            .mul => Arg3,
            // multiplies the immediate in s to a register and stores it in there
            .mulimm => ArgU,
            // subtracts the values in the last two registers together and stores
            // the result in the first
            .sub => Arg3,
            // subtracts the immediate in s to a register and stores it in there
            .subimm => ArgU,
            // divides the values in the last two registers together and stores
            // the result in the first
            .div => Arg3,
            // divides the immediate in s to a register and stores it in there
            .divimm => ArgU,

            // define a global variable where the symbol name is in u and value in r
            .define_global => ArgU,
        };
    }
};
