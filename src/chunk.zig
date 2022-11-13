const std = @import("std");
const Inst = @import("inst.zig").Inst;
const Value = @import("value.zig").Value;
const String = @import("value.zig").String;

/// A collection of instructions and acompanying data
pub const Chunk = struct {
    const Self = @This();

    const MAX_INST = 1024;
    const MAX_CONSTS = 1024;

    pub const ChunkError = error{
        TooManyConsts,
        TooManyInstructions,
    };

    /// instructions that make up this chunk
    code: [MAX_INST]Inst = undefined,
    /// offset in the inst array
    n_inst: usize = 0,
    /// constediate values that might be refered to
    consts: [MAX_CONSTS]Value = [_]Value{Value.empty} ** MAX_CONSTS,
    /// offset in the consts array
    n_consts: usize = 0,

    // TOOD: remove
    allocator: std.mem.Allocator = undefined,

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.consts) |val| {
            switch (val) {
                .string => |str| {
                    str.deinit(self.allocator);
                },
                else => {},
            }
        }
    }

    pub fn clear(self: *Self) void {
        self.* = Self{};
    }

    /// Add an instruction to this chunk
    /// returns an error if there are too many instructions
    /// or consts in this chunk
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

    pub fn pushConst(self: *Self, val: Value) !u16 {
        if (self.n_consts >= MAX_CONSTS) {
            return error.TooManyConsts;
        }

        const offset = self.n_consts;
        self.n_consts += 1;

        self.consts[offset] = val;

        return @intCast(u16, offset);
    }

    pub fn pushConstStr(self: *Self, str: []const u8) !u16 {
        // check if we can even create the string
        if (self.n_consts >= MAX_CONSTS) {
            return error.TooManyConsts;
        }
        // allocate it
        const str_ptr = try String.init(self.allocator, str);
        // push it
        return try self.pushConst(.{ .string = str_ptr });
    }

    pub fn fromSlice(insts: []const Inst) !Self {
        var self = Chunk{};
        _ = try self.pushInstSlice(insts);
        return self;
    }

    /// disassemble an instruction, used for debug only
    pub fn disassemble(self: Self) void {
        std.debug.print("constediates:\n", .{});
        {
            var i: usize = 0;
            while (i < self.n_consts) : (i += 1) {
                std.debug.print("consts[{}]: {}\n", .{ i, self.consts[i] });
            }
        }

        std.debug.print("chunk code_len: {}\n", .{self.n_inst});
        {
            var i: usize = 0;
            while (i < self.n_inst) : (i += 1) {
                self.code[i].disassemble(i);
            }
        }
        std.debug.print("-------------------------------\n", .{});
    }
};

test "chunk" {
    var chunk = Chunk{};
    _ = try chunk.pushInst(Inst.init(.ret, .{}));
    const off = try chunk.pushInstSlice(&.{
        Inst.init(.load, .{ .u = 0 }),
        Inst.init(.ret, .{}),
    });
    try std.testing.expect(chunk.n_inst == 3);
    try std.testing.expect(chunk.n_inst == off);

    _ = try chunk.pushConst(.{ .float = 23.3 });

    std.debug.print("\n", .{});
    chunk.disassemble();

    chunk.clear();
    try std.testing.expect(chunk.n_inst == 0);

    _ = Inst.init(.load, .{}).argu().u;
}
