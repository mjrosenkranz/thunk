const std = @import("std");
const testing = std.testing;
const Chunk = @import("chunk.zig").Chunk;
const Inst = @import("inst.zig").Inst;
const Value = @import("value.zig").Value;
/// virtual machine for interpreting our op-codes
pub const Vm = struct {
    const Self = @This();

    /// config for the vm to use in execution
    pub const Config = struct {
        debug_trace: bool = false,
    };

    pub const MAX_REGS = 256;

    /// vm configuration
    config: Config = .{},
    /// instruction pointer, index of the current inst
    ip: usize = 0,
    /// registers used in the vm
    /// TODO: should this just be a u32? or maybe a union
    regs: [MAX_REGS]Value = undefined,

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
                .load => |args| {
                    self.regs[args.a] = chunk.imms[args.u];
                },
                .add => |args| {
                    self.regs[args.a] = self.regs[args.b] + self.regs[args.c];
                },
                .addimm => |args| {
                    self.regs[args.a] = self.regs[args.a] + chunk.imms[args.u];
                },
                .sub => |args| {
                    self.regs[args.a] = self.regs[args.b] - self.regs[args.c];
                },
                .subimm => |args| {
                    self.regs[args.a] = self.regs[args.a] - chunk.imms[args.u];
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

    try testing.expectError(error.UnexpectedEnd, vm.exec(&chunk));
}

test "load a value" {
    var vm = Vm.initConfig(TestConfig);
    defer vm.deinit();

    var chunk = Chunk{};
    // load first constant into register 2
    _ = try chunk.pushInst(.{ .load = .{ .a = 2, .u = try chunk.pushImm(3) } });
    // return nada
    _ = try chunk.pushInst(.{ .ret = .{} });

    try vm.exec(&chunk);

    try testing.expect(vm.regs[2] == 3);
}

test "load some values and add them" {
    var vm = Vm.initConfig(TestConfig);
    defer vm.deinit();

    // the registers we are loading into
    const a = 1;
    const b = 2;
    const c = 3;

    var chunk = Chunk{};
    // load a constant into b
    _ = try chunk.pushInst(.{ .load = .{ .a = b, .u = try chunk.pushImm(3) } });
    // load a constant into c
    _ = try chunk.pushInst(.{ .load = .{ .a = c, .u = try chunk.pushImm(2) } });
    // a = b + c
    _ = try chunk.pushInst(.{ .add = .{ .a = a, .b = b, .c = c } });
    // return nada
    _ = try chunk.pushInst(.{ .ret = .{} });

    try vm.exec(&chunk);

    try testing.expect(vm.regs[a] == 5);
    try testing.expect(vm.regs[b] == 3);
    try testing.expect(vm.regs[c] == 2);
}

test "add and store in same register" {
    var vm = Vm.initConfig(TestConfig);
    defer vm.deinit();

    // the registers we are loading into
    const a = 1;
    const b = 2;

    const eps = std.math.epsilon(f32);

    var chunk = Chunk{};
    // load a constant into a
    _ = try chunk.pushInst(.{ .load = .{ .a = a, .u = try chunk.pushImm(7) } });
    // load a constant into b
    _ = try chunk.pushInst(.{ .load = .{ .a = b, .u = try chunk.pushImm(3) } });
    // a = b + a
    _ = try chunk.pushInst(.{ .add = .{ .a = a, .b = a, .c = b } });
    // return nada
    _ = try chunk.pushInst(.{ .ret = .{} });

    try vm.exec(&chunk);

    try testing.expectApproxEqAbs(@as(f32, 10.0), vm.regs[a], eps);
    try testing.expectApproxEqAbs(@as(f32, 3.0), vm.regs[b], eps);
}

test "add immediate" {
    var vm = Vm.initConfig(TestConfig);
    defer vm.deinit();

    // the registers we are loading into
    const a = 1;

    const eps = std.math.epsilon(f32);

    var chunk = Chunk{};
    // load a constant into a
    _ = try chunk.pushInst(.{ .load = .{ .a = a, .u = try chunk.pushImm(7) } });
    // load a constant into b
    _ = try chunk.pushInst(.{ .addimm = .{ .a = a, .u = try chunk.pushImm(3) } });
    // return nada
    _ = try chunk.pushInst(.{ .ret = .{} });

    try vm.exec(&chunk);

    try testing.expectApproxEqAbs(@as(f32, 10.0), vm.regs[a], eps);
}

test "subtract" {
    var vm = Vm.initConfig(TestConfig);
    defer vm.deinit();

    // the registers we are loading into
    const a = 1;
    const b = 2;
    const c = 3;
    const d = 4;

    const eps = std.math.epsilon(f32);

    var chunk = Chunk{};
    // load a constant into a
    _ = try chunk.pushInst(.{ .load = .{ .a = a, .u = try chunk.pushImm(7) } });
    // load a constant into b
    _ = try chunk.pushInst(.{ .subimm = .{ .a = a, .u = try chunk.pushImm(3) } });

    _ = try chunk.pushInst(.{ .load = .{ .a = b, .u = try chunk.pushImm(4) } });
    _ = try chunk.pushInst(.{ .load = .{ .a = c, .u = try chunk.pushImm(6) } });
    _ = try chunk.pushInst(.{ .sub = .{ .a = d, .b = b, .c = c } });

    // return nada
    _ = try chunk.pushInst(.{ .ret = .{} });

    try vm.exec(&chunk);

    try testing.expectApproxEqAbs(@as(f32, 4), vm.regs[a], eps);
    try testing.expectApproxEqAbs(@as(f32, 4), vm.regs[b], eps);
    try testing.expectApproxEqAbs(@as(f32, 6), vm.regs[c], eps);
    try testing.expectApproxEqAbs(@as(f32, -2), vm.regs[d], eps);
}
