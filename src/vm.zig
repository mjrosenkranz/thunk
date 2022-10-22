const std = @import("std");
/// virtual machine for interpreting our op-codes
pub const VM = struct {
    const Self = @This();
    pub fn init() Self {
        return .{};
    }

    pub fn deinit(self: Self) void {
        _ = self;
    }
};

/// Insructions that are our bytecode
/// all instructions are the same size (32 bits)
/// to maintain alignment
const Inst = union(Op) {
    // make sure our instructions are the right size
    comptime {
        std.debug.assert(@bitSizeOf(Arg3) == 24);
        std.debug.assert(@bitSizeOf(ArgS) == 24);
        std.debug.assert(@bitSizeOf(ArgU) == 24);
        std.debug.assert(@bitSizeOf(Inst) == 32);
        std.debug.assert(@sizeOf(Inst) == 4);
    }

    /// OpCode for each instruction we support
    pub const Op = enum(u8) {
        /// return from a function
        ret,
        /// loads a constant into a target register
        load,
    };

    /// argumentts for instruction with three arguments
    pub const Arg3 = packed struct {
        a: u8 = 0,
        b: u8 = 0,
        c: u8 = 0,
    };

    /// argumentts for instruction with one signed argument
    pub const ArgS = packed struct {
        a: u8 = 0,
        s: i16 = 0,
    };

    /// argumentts for instruction with an unsigned argument
    pub const ArgU = packed struct {
        a: u8 = 0,
        u: u16 = 0,
    };

    const Self = @This();

    ret: void,
    load: ArgU,

    /// print's an instruction, used for debug only
    pub fn print(self: Self) void {
        switch (self) {
            .ret => std.debug.print("return\n", .{}),
            .load => |args| {
                std.debug.print("load: {}\n", .{args.u});
            },
        }
    }
};

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
    code_offset: usize = 0,
    /// immediate values that might be refered to
    imms: [MAX_IMMS]Value = undefined,
    /// offset in the imms array
    imms_offset: usize = 0,

    /// Add an instruction to this chunk
    /// returns an error if there are too many instructions
    /// or imms in this chunk
    pub fn addInst(self: *Self, inst: Inst) !usize {
        if (self.code_offset >= MAX_INST) {
            return error.TooManyInstructions;
        }

        const offset = self.code_offset;
        self.code_offset += 1;

        self.code[offset] = inst;

        return offset;
    }

    /// Adds a slice of instructions to this chunk.
    /// if the slice is too big then an error is returned imediately
    pub fn addInstSlice(self: *Self, insts: []const Inst) !usize {
        const total_offset = self.code_offset + insts.len;
        if (total_offset >= MAX_INST) {
            return error.TooManyInstructions;
        }
        for (insts) |inst| {
            _ = try self.addInst(inst);
        }

        return total_offset;
    }

    pub fn addImm(self: *Self, imm: Value) !usize {
        if (self.imms_offset >= MAX_IMMS) {
            return error.TooManyImmediates;
        }

        const offset = self.imms_offset;
        self.imms_offset += 1;

        self.imms[offset] = imm;

        return offset;
    }

    pub fn clear(self: *Self) void {
        self.* = Self{};
    }
};

test "chunk" {
    var chunk = Chunk{};
    _ = try chunk.addInst(.{ .ret = {} });
    const off = try chunk.addInstSlice(&.{
        .{ .load = .{ .u = 13 } },
        .{ .ret = {} },
    });
    try std.testing.expect(chunk.code_offset == 3);
    try std.testing.expect(chunk.code_offset == off);
    var i: usize = 0;
    while (i < chunk.code_offset) : (i += 1) {
        chunk.code[i].print();
    }

    chunk.clear();
    try std.testing.expect(chunk.code_offset == 0);

    _ = try chunk.addImm(23);
}

test "init vm" {
    var vm = VM.init();
    defer vm.deinit();
}
