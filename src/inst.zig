const std = @import("std");
const TokenTag = @import("token.zig").Token.Tag;

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
    @"test",
    not,
    add,
    addconst,
    mul,
    mulconst,
    sub,
    subconst,
    div,
    divconst,
    define_global,
    set_global,
    get_global,
    lt,
    lte,
    gt,
    gte,

    /// helper function for converting a primitive operation to
    /// a supported opcode
    pub fn fromPrimitiveTokenTag(tag: TokenTag, is_const: bool) Op {
        return switch (tag) {
            .plus => if (is_const) .addconst else .add,
            .minus => if (is_const) .subconst else .sub,
            .asterisk => if (is_const) .mulconst else .mul,
            .slash => if (is_const) .divconst else .div,
            .gt => .gt,
            .lt => .lt,
            .gte => .gte,
            .lte => .lte,
            else => unreachable,
        };
    }
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
                std.debug.print(offset_fmt ++ "load: const[{}] into reg{}\n", .{
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
            .addconst => {
                std.debug.print(offset_fmt ++ "addconst: reg[{}] = reg[{}] + const[{}]\n", .{
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
            .subconst => {
                std.debug.print(offset_fmt ++ "subconst: reg[{}] = reg[{}] + const[{}]\n", .{
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
            .mulconst => {
                std.debug.print(offset_fmt ++ "mulconst: reg[{}] = reg[{}] + const[{}]\n", .{
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
            .divconst => {
                std.debug.print(offset_fmt ++ "divconst: reg[{}] = reg[{}] + const[{}]\n", .{
                    offset,
                    self.argu().r,
                    self.argu().r,
                    self.argu().u,
                });
            },
            .define_global => {
                std.debug.print(offset_fmt ++ "define: <global const[{}]> = reg[{}]\n", .{
                    offset,
                    self.argu().u,
                    self.argu().r,
                });
            },
            .get_global => {
                std.debug.print(offset_fmt ++ "get: reg[{}] = <global const[{}]>\n", .{
                    offset,
                    self.argu().r,
                    self.argu().u,
                });
            },
            .set_global => {
                std.debug.print(offset_fmt ++ "set: <global const[{}]> = reg[{}]\n", .{
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
            .eq_true => {
                std.debug.print(offset_fmt ++ "eq_true: if reg[{}]: ip += 1\n", .{
                    offset,
                    self.arg3().r,
                });
            },
            .@"test" => {
                std.debug.print(offset_fmt ++ "test: reg[{}] = reg[{}] == {}\n", .{
                    offset,
                    self.arg3().r,
                    self.arg3().r1,
                    if (self.arg3().r2 > 0) true else false,
                });
            },
            .not => {
                std.debug.print(offset_fmt ++ "not: reg[{}] = !reg[{}]\n", .{
                    offset,
                    self.arg3().r,
                    self.arg3().r1,
                });
            },
            .lt => {
                std.debug.print(offset_fmt ++ "lt: reg[{}] = reg[{}] < reg[{}]\n", .{
                    offset,
                    self.arg3().r,
                    self.arg3().r1,
                    self.arg3().r2,
                });
            },
            .lte => {
                std.debug.print(offset_fmt ++ "lte: reg[{}] = reg[{}] <= reg[{}]\n", .{
                    offset,
                    self.arg3().r,
                    self.arg3().r1,
                    self.arg3().r2,
                });
            },
            .gt => {
                std.debug.print(offset_fmt ++ "gt: reg[{}] = reg[{}] > reg[{}]\n", .{
                    offset,
                    self.arg3().r,
                    self.arg3().r1,
                    self.arg3().r2,
                });
            },
            .gte => {
                std.debug.print(offset_fmt ++ "gte: reg[{}] = reg[{}] >= reg[{}]\n", .{
                    offset,
                    self.arg3().r,
                    self.arg3().r1,
                    self.arg3().r2,
                });
            },
        }
    }

    pub inline fn instType(comptime op: Op) type {
        return switch (op) {
            // returns values in register a
            // TODO: figure out returning multiple values
            .ret => Arg3,
            // loads constant value stored at u into register a
            .load => ArgU,
            // moves the value from r1 into r
            .move => Arg3,
            // adds the values in the last two registers together and stores
            // the result in the first
            .add => Arg3,
            // adds the constant in s to a register and stores it in there
            .addconst => ArgU,
            // multiplies the values in the last two registers together and stores
            // the result in the first
            .mul => Arg3,
            // multiplies the constant in s to a register and stores it in there
            .mulconst => ArgU,
            // subtracts the values in the last two registers together and stores
            // the result in the first
            .sub => Arg3,
            // subtracts the constant in s to a register and stores it in there
            .subconst => ArgU,
            // divides the values in the last two registers together and stores
            // the result in the first
            .div => Arg3,
            // divides the constant in s to a register and stores it in there
            .divconst => ArgU,

            // define a global variable where the symbol name is in u and value in r
            .define_global => ArgU,
            // set value of global variable where symbol name is in u and value in r
            .set_global => ArgU,
            // get value of global variable where symbol name is in u destination is r
            .get_global => ArgU,
            // change ip by i
            .jmp => ArgI,
            // if the value in reg r is truthy then skip next inst
            .eq_true => Arg3,
            // stores if r1 == bool[r2] into r, and skips next instruction if false
            // if r2 == 0 then we test if false
            // if r2 >= 1 then we test if true
            .@"test" => Arg3,
            // store !r1 in r
            .not => Arg3,
            // stores true into r if r1 < r2, otherwise stores false
            .lt => Arg3,
            // stores true into r if r1 <= r2, otherwise stores false
            .lte => Arg3,
            // stores true into r if r1 > r2, otherwise stores false
            .gt => Arg3,
            // stores true into r if r1 >= r2, otherwise stores false
            .gte => Arg3,
        };
    }
};
