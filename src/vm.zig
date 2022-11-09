const std = @import("std");
const testing = std.testing;
const Chunk = @import("chunk.zig").Chunk;
const Compiler = @import("compiler.zig").Compiler;
const Inst = @import("inst.zig").Inst;
const Value = @import("value.zig").Value;
const String = @import("value.zig").String;
const Env = @import("env.zig").Env;
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

    /// the global environment
    env: Env,

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .env = Env.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.env.deinit();
    }

    pub fn initConfig(config: Config, allocator: std.mem.Allocator) Self {
        return .{
            .config = config,
            .env = Env.init(allocator),
        };
    }

    /// executes all the instructions in a chunk
    pub fn exec(self: *Self, chunk: *Chunk) !void {
        while (self.ip < chunk.n_inst) {
            const inst = chunk.code[self.ip];

            if (self.config.debug_trace) {
                // chunk.disassembleInst(self.ip, inst);
            }

            switch (inst.op) {
                .ret => return,
                // nothing to do with a load for now so we can just print it
                .load => {
                    const args = inst.argu();
                    self.regs[args.r] = chunk.imms[args.u];
                },
                .add => {
                    const args = inst.arg3();
                    self.regs[args.r] = try self.regs[args.r1].add(self.regs[args.r2]);
                },
                .addimm => {
                    const args = inst.argu();
                    self.regs[args.r] = try self.regs[args.r].add(chunk.imms[args.u]);
                },
                .sub => {
                    const args = inst.arg3();
                    self.regs[args.r] = try self.regs[args.r1].sub(self.regs[args.r2]);
                },
                .subimm => {
                    const args = inst.argu();
                    self.regs[args.r] = try self.regs[args.r].sub(chunk.imms[args.u]);
                },
                .mul => {
                    const args = inst.arg3();
                    self.regs[args.r] = try self.regs[args.r1].mul(self.regs[args.r2]);
                },
                .mulimm => {
                    const args = inst.argu();
                    self.regs[args.r] = try self.regs[args.r].mul(chunk.imms[args.u]);
                },
                .div => {
                    const args = inst.arg3();
                    self.regs[args.r] = try self.regs[args.r1].div(self.regs[args.r2]);
                },
                .divimm => {
                    const args = inst.argu();
                    self.regs[args.r] = try self.regs[args.r].div(chunk.imms[args.u]);
                },
                .define_global => {
                    const args = inst.argu();
                    const imm = chunk.imms[args.u];
                    try Value.assertType(.string, imm);
                    const str = imm.string;
                    try self.env.map.put(str.slice(), self.regs[args.r]);
                },
                .set_global => {
                    const args = inst.argu();
                    const imm = chunk.imms[args.u];
                    try Value.assertType(.string, imm);
                    try self.env.map.put(imm.string.slice(), self.regs[args.r]);
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
};

const TestConfig = Vm.Config{
    .debug_trace = true,
};

const eps = std.math.epsilon(f32);

test "init vm" {
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();
}

test "just returns" {
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    var chunk = try Chunk.fromSlice(&.{
        Inst.init(.ret, .{}),
    });

    try vm.exec(&chunk);
}

test "UnexpectedEnd" {
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    var chunk = try Chunk.fromSlice(&.{
        Inst.init(.load, .{}),
    });

    try testing.expectError(error.UnexpectedEnd, vm.exec(&chunk));
}

test "load a value" {
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    var chunk = Chunk{};
    // load first constant into register 2
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = 2, .u = try chunk.pushImm(.{ .float = 3 }) }));
    // return nada
    _ = try chunk.pushInst(Inst.init(.ret, .{}));

    try vm.exec(&chunk);

    try testing.expectApproxEqAbs(@as(f32, 3), vm.regs[2].float, eps);
}

test "load some values and add them" {
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    // the registers we are loading into
    const a = 1;
    const b = 2;
    const c = 3;

    var chunk = Chunk{};
    // load a constant into b
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = b, .u = try chunk.pushImm(.{ .float = 3 }) }));
    // load a constant into c
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = c, .u = try chunk.pushImm(.{ .float = 2 }) }));
    // a = b + c
    _ = try chunk.pushInst(Inst.init(.add, .{ .r = a, .r1 = b, .r2 = c }));
    // return nada
    _ = try chunk.pushInst(Inst.init(.ret, .{}));

    try vm.exec(&chunk);

    try testing.expectApproxEqAbs(@as(f32, 5), vm.regs[a].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 3), vm.regs[b].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 2), vm.regs[c].float, eps);
}

test "add and store in same register" {
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    // the registers we are loading into
    const a = 1;
    const b = 2;

    var chunk = Chunk{};
    // load a constant into a
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = a, .u = try chunk.pushImm(.{ .float = 7 }) }));
    // load a constant into b
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = b, .u = try chunk.pushImm(.{ .float = 3 }) }));
    // a = b + a
    _ = try chunk.pushInst(Inst.init(.add, .{ .r = a, .r1 = a, .r2 = b }));
    // return nada
    _ = try chunk.pushInst(Inst.init(.ret, .{}));

    try vm.exec(&chunk);

    try testing.expectApproxEqAbs(@as(f32, 10.0), vm.regs[a].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 3.0), vm.regs[b].float, eps);
}

test "add immediate" {
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    // the registers we are loading into
    const a = 1;

    var chunk = Chunk{};
    // load a constant into a
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = a, .u = try chunk.pushImm(.{ .float = 7 }) }));
    // load a constant into b
    _ = try chunk.pushInst(Inst.init(.addimm, .{ .r = a, .u = try chunk.pushImm(.{ .float = 3 }) }));
    // return nada
    _ = try chunk.pushInst(Inst.init(.ret, .{}));

    try vm.exec(&chunk);

    try testing.expectApproxEqAbs(@as(f32, 10.0), vm.regs[a].float, eps);
}

test "subtract" {
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    // the registers we are loading into
    const a = 1;
    const b = 2;
    const c = 3;
    const d = 4;

    var chunk = Chunk{};
    // load a constant into a
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = a, .u = try chunk.pushImm(.{ .float = 7 }) }));
    // load a constant into b
    _ = try chunk.pushInst(Inst.init(.subimm, .{ .r = a, .u = try chunk.pushImm(.{ .float = 3 }) }));

    _ = try chunk.pushInst(Inst.init(.load, .{ .r = b, .u = try chunk.pushImm(.{ .float = 4 }) }));
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = c, .u = try chunk.pushImm(.{ .float = 6 }) }));
    _ = try chunk.pushInst(Inst.init(.sub, .{ .r = d, .r1 = b, .r2 = c }));

    // return nada
    _ = try chunk.pushInst(Inst.init(.ret, .{}));

    try vm.exec(&chunk);

    try testing.expectApproxEqAbs(@as(f32, 4), vm.regs[a].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 4), vm.regs[b].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 6), vm.regs[c].float, eps);
    try testing.expectApproxEqAbs(@as(f32, -2), vm.regs[d].float, eps);
}

test "multiply" {
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    // the registers we are loading into
    const a = 1;
    const b = 2;
    const c = 3;
    const d = 4;

    var chunk = Chunk{};
    // load a constant into a
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = a, .u = try chunk.pushImm(.{ .float = 7 }) }));
    // load a constant into b
    _ = try chunk.pushInst(Inst.init(.mulimm, .{ .r = a, .u = try chunk.pushImm(.{ .float = 3 }) }));

    _ = try chunk.pushInst(Inst.init(.load, .{ .r = b, .u = try chunk.pushImm(.{ .float = -4 }) }));
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = c, .u = try chunk.pushImm(.{ .float = 6 }) }));
    _ = try chunk.pushInst(Inst.init(.mul, .{ .r = d, .r1 = b, .r2 = c }));

    // return nada
    _ = try chunk.pushInst(Inst.init(.ret, .{}));

    try vm.exec(&chunk);

    try testing.expectApproxEqAbs(@as(f32, 21), vm.regs[a].float, eps);
    try testing.expectApproxEqAbs(@as(f32, -4), vm.regs[b].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 6), vm.regs[c].float, eps);
    try testing.expectApproxEqAbs(@as(f32, -24), vm.regs[d].float, eps);
}

test "divide" {
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    // the registers we are loading into
    const a = 1;
    const b = 2;
    const c = 3;
    const d = 4;

    var chunk = Chunk{};
    // load a constant into a
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = a, .u = try chunk.pushImm(.{ .float = 8 }) }));
    // load a constant into b
    _ = try chunk.pushInst(Inst.init(.divimm, .{ .r = a, .u = try chunk.pushImm(.{ .float = 4 }) }));

    _ = try chunk.pushInst(Inst.init(.load, .{ .r = b, .u = try chunk.pushImm(.{ .float = 6 }) }));
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = c, .u = try chunk.pushImm(.{ .float = -4 }) }));
    _ = try chunk.pushInst(Inst.init(.div, .{ .r = d, .r1 = b, .r2 = c }));

    // return nada
    _ = try chunk.pushInst(Inst.init(.ret, .{}));

    try vm.exec(&chunk);

    try testing.expectApproxEqAbs(@as(f32, 2.0), vm.regs[a].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 6), vm.regs[b].float, eps);
    try testing.expectApproxEqAbs(@as(f32, -4), vm.regs[c].float, eps);
    try testing.expectApproxEqAbs(@as(f32, -1.5), vm.regs[d].float, eps);
}

test "addition full pipeline" {
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    const code =
        \\(+ 125 13)
    ;
    var chunk = Chunk{};
    var compiler = Compiler{};
    _ = try compiler.compile(code, &chunk, &vm.env);
    try testing.expectApproxEqAbs(@as(f32, 125), chunk.imms[0].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 13), chunk.imms[1].float, eps);

    try testing.expectEqualSlices(Inst, &.{
        Inst.init(.load, .{ .r = 1, .u = 0 }),
        Inst.init(.load, .{ .r = 2, .u = 1 }),
        Inst.init(.add, .{ .r = 3, .r1 = 1, .r2 = 2 }),
        Inst.init(.ret, .{}),
    }, chunk.code[0..chunk.n_inst]);

    try vm.exec(&chunk);
    try testing.expectApproxEqAbs(@as(f32, 125), vm.regs[1].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 13), vm.regs[2].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 138), vm.regs[3].float, eps);
}

test "nested math full pipeline" {
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    const code =
        \\(* 3 (+ 4 5))
    ;
    var chunk = Chunk{};
    var compiler = Compiler{};
    _ = try compiler.compile(code, &chunk, &vm.env);
    try testing.expectApproxEqAbs(@as(f32, 3), chunk.imms[0].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 4), chunk.imms[1].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 5), chunk.imms[2].float, eps);

    try testing.expectEqualSlices(Inst, &.{
        Inst.init(.load, .{ .r = 1, .u = 0 }),
        Inst.init(.load, .{ .r = 2, .u = 1 }),
        Inst.init(.load, .{ .r = 3, .u = 2 }),
        Inst.init(.add, .{ .r = 4, .r1 = 2, .r2 = 3 }),
        Inst.init(.mul, .{ .r = 5, .r1 = 1, .r2 = 4 }),
        Inst.init(.ret, .{}),
    }, chunk.code[0..chunk.n_inst]);

    try vm.exec(&chunk);
    try testing.expectApproxEqAbs(@as(f32, 3), vm.regs[1].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 4), vm.regs[2].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 5), vm.regs[3].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 9), vm.regs[4].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 27), vm.regs[5].float, eps);
}

// TODO: redefined builtin primitives so this passes
test "just a proceedure" {
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    const code =
        \\+
    ;
    var chunk = Chunk{};
    var compiler = Compiler{};
    _ = try compiler.compile(code, &chunk, &vm.env);
    try vm.exec(&chunk);
}

test "define var" {
    const code =
        \\(define foo 13)
    ;

    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();
    var chunk = Chunk.init(testing.allocator);
    defer chunk.deinit();
    var compiler = Compiler{};
    _ = try compiler.compile(code, &chunk, &vm.env);

    try vm.exec(&chunk);

    try testing.expect(vm.env.map.get("foo").?.float == 13);
}

test "set var" {
    const code =
        \\(define foo 13)
        \\(set! foo 32)
    ;

    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    var chunk = Chunk.init(testing.allocator);
    defer chunk.deinit();
    var compiler = Compiler{};
    _ = try compiler.compile(code, &chunk, &vm.env);

    try vm.exec(&chunk);

    try testing.expect(vm.env.map.get("foo").?.float == 32);
}
