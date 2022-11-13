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
    move,
    jmp,
    eq_true,
    eq_false,
    add,
    addimm,
    mul,
    mulimm,
    sub,
    subimm,
    div,
    divimm,
    define_global,
    set_global,
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

/// arguments for instruction with an unsigned argument
/// TODO: should this be signed?
pub const ArgI = ArgSize;

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

    pub inline fn argi(self: Self) ArgI {
        return @bitCast(ArgI, self.data);
    }

    pub inline fn disassemble(self: Self, offset: usize) void {
        const offset_fmt = "{d:0>4}: ";
        switch (self.op) {
            .ret => std.debug.print(offset_fmt ++ "ret: a: {} b: {} c: {}\n", .{
                offset,
                self.arg3().r,
                self.arg3().r1,
                self.arg3().r2,
            }),
            .load => {
                std.debug.print(offset_fmt ++ "load: imm[{}] into reg{}\n", .{
                    offset,
                    self.argu().u,
                    self.argu().r,
                });
            },
            .move => {
                std.debug.print(offset_fmt ++ "move: reg{} = reg{}\n", .{
                    offset,
                    self.arg3().r,
                    self.arg3().r1,
                });
            },
            .add => {
                std.debug.print(offset_fmt ++ "add: reg[{}] = reg[{}] + reg[{}]\n", .{
                    offset,
                    self.arg3().r,
                    self.arg3().r1,
                    self.arg3().r2,
                });
            },
            .addimm => {
                std.debug.print(offset_fmt ++ "addimm: reg[{}] = reg[{}] + imm[{}]\n", .{
                    offset,
                    self.argu().r,
                    self.argu().r,
                    self.argu().u,
                });
            },
            .sub => {
                std.debug.print(offset_fmt ++ "sub: reg[{}] = reg[{}] + reg[{}]\n", .{
                    offset,
                    self.arg3().r,
                    self.arg3().r1,
                    self.arg3().r2,
                });
            },
            .subimm => {
                std.debug.print(offset_fmt ++ "subimm: reg[{}] = reg[{}] + imm[{}]\n", .{
                    offset,
                    self.argu().r,
                    self.argu().r,
                    self.argu().u,
                });
            },
            .mul => {
                std.debug.print(offset_fmt ++ "mul: reg[{}] = reg[{}] + reg[{}]\n", .{
                    offset,
                    self.argu().r,
                    self.argu().r,
                    self.argu().u,
                });
            },
            .mulimm => {
                std.debug.print(offset_fmt ++ "mulimm: reg[{}] = reg[{}] + imm[{}]\n", .{
                    offset,
                    self.argu().r,
                    self.argu().r,
                    self.argu().u,
                });
            },

            .div => {
                std.debug.print(offset_fmt ++ "div: reg[{}] = reg[{}] + reg[{}]\n", .{
                    offset,
                    self.argu().r,
                    self.argu().r,
                    self.argu().u,
                });
            },
            .divimm => {
                std.debug.print(offset_fmt ++ "divimm: reg[{}] = reg[{}] + imm[{}]\n", .{
                    offset,
                    self.argu().r,
                    self.argu().r,
                    self.argu().u,
                });
            },
            .define_global => {
                std.debug.print(offset_fmt ++ "define: <global imm[{}]> = reg[{}]\n", .{
                    offset,
                    self.argu().u,
                    self.argu().r,
                });
            },
            .set_global => {
                std.debug.print(offset_fmt ++ "set: <global imm[{}]> = reg[{}]\n", .{
                    offset,
                    self.argu().u,
                    self.argu().r,
                });
            },
            .jmp => {
                std.debug.print(offset_fmt ++ "jmp: ip += {}\n", .{
                    offset,
                    self.data,
                });
            },
            .eq_false => {
                std.debug.print(offset_fmt ++ "eq_false: if !reg[{}]: ip += 1\n", .{
                    offset,
                    self.arg3().r,
                });
            },
            .eq_true => {
                std.debug.print(offset_fmt ++ "eq_true: if reg[{}]: ip += 1\n", .{
                    offset,
                    self.arg3().r,
                });
            },
        }
    }

    pub inline fn instType(comptime op: Op) type {
        return switch (op) {
            // returns values in the registers a to b
            .ret => Arg3,
            // loads immediate value stored at u into register a
            .load => ArgU,
            // moves the value from r1 into r
            .move => Arg3,
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
            //set value of global variable where symbol name is in u and value in r
            .set_global => ArgU,
            // change ip by i
            .jmp => ArgI,
            // if the value in reg r is truthy then skip next inst
            .eq_true => Arg3,
            // if the value in reg r is false then skip next inst
            .eq_false => Arg3,
        };
    }
};
