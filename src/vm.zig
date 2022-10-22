const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const Inst = @import("inst.zig").Inst;
/// virtual machine for interpreting our op-codes
pub const Vm = struct {
    const Self = @This();

    /// config for the vm to use in execution
    const Config = struct {
        debug_trace: bool = false,
    };

    /// instruction pointer, index of the current inst
    ip: usize = 0,
    config: Config = .{},

    pub fn init() Self {
        return .{};
    }

    pub fn initConfig(config: Config) Self {
        return .{
            .config = config,
        };
    }

    /// executes all the instructions in a chunk
    pub fn exec(self: *Self, chunk: *Chunk) !void {
        while (self.ip < chunk.n_inst) {
            const inst = chunk.code[self.ip];

            if (self.config.debug_trace) {
                chunk.disassembleInst(self.ip, inst);
            }

            switch (inst) {
                .ret => return,
                // nothing to do with a load for now so we can just print it
                .load => |_| {},
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

const TestConfig = Vm.Config{
    .debug_trace = true,
};

test "init vm" {
    var vm = Vm.initConfig(TestConfig);
    defer vm.deinit();
}

test "just returns" {
    var vm = Vm.initConfig(TestConfig);
    defer vm.deinit();

    var chunk = try Chunk.fromSlice(&.{
        .{ .ret = .{} },
    });

    try vm.exec(&chunk);
}

test "UnexpectedEnd" {
    var vm = Vm.initConfig(TestConfig);
    defer vm.deinit();

    var chunk = try Chunk.fromSlice(&.{
        .{ .load = .{} },
    });

    try std.testing.expectError(error.UnexpectedEnd, vm.exec(&chunk));
}
