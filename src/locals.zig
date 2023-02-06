const std = @import("std");
const Token = @import("token.zig");
const inst = @import("inst.zig");
const Reg = inst.Reg;
const Assoc = @import("assoc.zig").Assoc;

pub const LocalList = Assoc(
    32,
    Token,
    Local,
    &compareToken,
);

pub inline fn compareToken(a: Token, b: Token) bool {
    return std.mem.eql(u8, a.loc.slice, b.loc.slice);
}

const Local = struct {
    /// the register
    reg: Reg,
    /// the depth of the local in our nesting
    depth: u8,
};
