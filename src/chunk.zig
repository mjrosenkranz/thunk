const std = @import("std");
const Inst = @import("inst.zig").Inst;
// start with only one kind of value
const Value = f32;

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
    pub fn addInst(self: *Self, inst: Inst) !usize {
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
    pub fn addInstSlice(self: *Self, insts: []const Inst) !usize {
        const total_offset = self.n_inst + insts.len;
        if (total_offset >= MAX_INST) {
            return error.TooManyInstructions;
        }
        for (insts) |inst| {
            _ = try self.addInst(inst);
        }

        return total_offset;
    }

    pub fn addImm(self: *Self, imm: Value) !usize {
        if (self.n_imms >= MAX_IMMS) {
            return error.TooManyImmediates;
        }

        const offset = self.n_imms;
        self.n_imms += 1;

        self.imms[offset] = imm;

        return offset;
    }

    pub fn fromSlice(insts: []const Inst) !Self {
        var self = Chunk{};
        _ = try self.addInstSlice(insts);
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
                args.a,
                args.b,
                args.c,
            }),
            .load => |args| {
                std.debug.print(offset_fmt ++ "load: imm[{}] ({d:.1}) into reg{}\n", .{
                    offset,
                    args.u,
                    self.imms[args.u],
                    args.a,
                });
            },
        }
    }
};

test "chunk" {
    var chunk = Chunk{};
    _ = try chunk.addInst(.{ .ret = .{} });
    const off = try chunk.addInstSlice(&.{
        .{ .load = .{ .u = 0 } },
        .{ .ret = .{} },
    });
    try std.testing.expect(chunk.n_inst == 3);
    try std.testing.expect(chunk.n_inst == off);

    _ = try chunk.addImm(23.3);

    std.debug.print("\n", .{});
    chunk.disassemble();

    chunk.clear();
    try std.testing.expect(chunk.n_inst == 0);
}
