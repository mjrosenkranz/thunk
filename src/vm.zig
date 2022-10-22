const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const Inst = @import("inst.zig").Inst;
/// virtual machine for interpreting our op-codes
pub const VM = struct {
    const Self = @This();

    /// instruction pointer, index of the current inst
    ip: usize = 0,

    pub fn init() Self {
        return .{};
    }

    /// executes all the instructions in a chunk
    pub fn exec(self: *Self, chunk: *Chunk) !void {
        while (self.ip < chunk.n_inst) {
            switch (chunk.code[self.ip]) {
                .ret => return,
                // nothing to do with a load for now so we can just print it
                .load => |args| {
                    std.debug.print("load: imm[{}] ({d:.1}) into reg{}\n", .{
                        args.u,
                        chunk.imms[args.u],
                        args.a,
                    });
                },
            }

            self.ip += 1;
        }

        // if we get here we ran out of instructions in the chunk
        // this means that the chunk did not end with a return and we
        // should throw an error
        // TODO: this should maybe be handled differently?
        // is there a case where a chunk should just end?
        return error.UnexpectedEnd;
    }

    pub fn deinit(self: Self) void {
        _ = self;
    }
};

test "init vm" {
    var vm = VM.init();
    defer vm.deinit();
}

test "just returns" {
    var vm = VM.init();
    defer vm.deinit();

    var chunk = try Chunk.fromSlice(&.{
        .{ .ret = .{} },
    });

    try vm.exec(&chunk);
}

test "UnexpectedEnd" {
    var vm = VM.init();
    defer vm.deinit();

    var chunk = try Chunk.fromSlice(&.{
        .{ .load = .{} },
    });

    try std.testing.expectError(error.UnexpectedEnd, vm.exec(&chunk));
}
