const std = @import("std");
const Inst = @import("inst.zig").Inst;
const Value = @import("value.zig").Value;

/// A collection of instructions and acompanying data
pub const Chunk = struct {
    const Self = @This();

    const MAX_INST = 1024;
    const MAX_IMMS = 1024;

    /// instructions that make up this chunk
    code: [MAX_INST]Inst = undefined,
    /// offset in the inst array
    n_inst: usize = 0,
    /// immediate values that might be refered to
    imms: [MAX_IMMS]Value = undefined,
    /// offset in the imms array
    n_imms: usize = 0,

    pub fn clear(self: *Self) void {
        self.* = Self{};
    }

    /// Add an instruction to this chunk
    /// returns an error if there are too many instructions
    /// or imms in this chunk
    pub fn pushInst(self: *Self, inst: Inst) !usize {
        if (self.n_inst >= MAX_INST) {
            return error.TooManyInstructions;
        }

        const offset = self.n_inst;
        self.n_inst += 1;

        self.code[offset] = inst;

        return offset;
    }

    /// Adds a slice of instructions to this chunk.
    /// if the slice is too big then an error is returned imediately
    pub fn pushInstSlice(self: *Self, insts: []const Inst) !usize {
        const total_offset = self.n_inst + insts.len;
        if (total_offset >= MAX_INST) {
            return error.TooManyInstructions;
        }
        for (insts) |inst| {
            _ = try self.pushInst(inst);
        }

        return total_offset;
    }

    pub fn pushImm(self: *Self, imm: Value) !u16 {
        if (self.n_imms >= MAX_IMMS) {
            return error.TooManyImmediates;
        }

        const offset = self.n_imms;
        self.n_imms += 1;

        self.imms[offset] = imm;

        return @intCast(u16, offset);
    }

    pub fn fromSlice(insts: []const Inst) !Self {
        var self = Chunk{};
        _ = try self.pushInstSlice(insts);
        return self;
    }

    /// disassemble an instruction, used for debug only
    pub fn disassemble(self: Self) void {
        std.debug.print("chunk code_len: {} imms_len: {}\n", .{
            self.n_inst,
            self.n_imms,
        });
        var i: usize = 0;
        while (i < self.n_inst) : (i += 1) {
            self.disassembleInst(i, self.code[i]);
        }
    }

    pub inline fn disassembleInst(self: Self, offset: usize, inst: Inst) void {
        const offset_fmt = "{d:0>4}: ";
        switch (inst) {
            .ret => |args| std.debug.print(offset_fmt ++ "ret: a: {} b: {} c: {}\n", .{
                offset,
                args.r,
                args.r1,
                args.r2,
            }),
            .load => |args| {
                std.debug.print(offset_fmt ++ "load: imm[{}] ({d:.1}) into reg{}\n", .{
                    offset,
                    args.u,
                    self.imms[args.u],
                    args.r,
                });
            },
            .add => |args| {
                std.debug.print(offset_fmt ++ "add: reg[{}] = reg[{}] + reg[{}]\n", .{
                    offset,
                    args.r,
                    args.r1,
                    args.r2,
                });
            },
            .addimm => |args| {
                std.debug.print(offset_fmt ++ "addimm: reg[{}] = reg[{}] + imm[{}] ({d:.1})\n", .{
                    offset,
                    args.r,
                    args.r,
                    args.u,
                    self.imms[args.u],
                });
            },
            .sub => |args| {
                std.debug.print(offset_fmt ++ "sub: reg[{}] = reg[{}] + reg[{}]\n", .{
                    offset,
                    args.r,
                    args.r1,
                    args.r2,
                });
            },
            .subimm => |args| {
                std.debug.print(offset_fmt ++ "subimm: reg[{}] = reg[{}] + imm[{}] ({d:.1})\n", .{
                    offset,
                    args.r,
                    args.r,
                    args.u,
                    self.imms[args.u],
                });
            },
            .mul => |args| {
                std.debug.print(offset_fmt ++ "mul: reg[{}] = reg[{}] + reg[{}]\n", .{
                    offset,
                    args.r,
                    args.r1,
                    args.r2,
                });
            },
            .mulimm => |args| {
                std.debug.print(offset_fmt ++ "mulimm: reg[{}] = reg[{}] + imm[{}] ({d:.1})\n", .{
                    offset,
                    args.r,
                    args.r,
                    args.u,
                    self.imms[args.u],
                });
            },

            .div => |args| {
                std.debug.print(offset_fmt ++ "div: reg[{}] = reg[{}] + reg[{}]\n", .{
                    offset,
                    args.r,
                    args.r1,
                    args.r2,
                });
            },
            .divimm => |args| {
                std.debug.print(offset_fmt ++ "divimm: reg[{}] = reg[{}] + imm[{}] ({d:.1})\n", .{
                    offset,
                    args.r,
                    args.r,
                    args.u,
                    self.imms[args.u],
                });
            },
        }
    }
};

test "chunk" {
    var chunk = Chunk{};
    _ = try chunk.pushInst(.{ .ret = .{} });
    const off = try chunk.pushInstSlice(&.{
        .{ .load = .{ .u = 0 } },
        .{ .ret = .{} },
    });
    try std.testing.expect(chunk.n_inst == 3);
    try std.testing.expect(chunk.n_inst == off);

    _ = try chunk.pushImm(23.3);

    std.debug.print("\n", .{});
    chunk.disassemble();

    chunk.clear();
    try std.testing.expect(chunk.n_inst == 0);
}
