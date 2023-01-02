const std = @import("std");
const testing = std.testing;
const Chunk = @import("chunk.zig").Chunk;
const compiler = @import("compiler.zig");
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

    pub fn step(self: *Self, chunk: *Chunk) !void {
        if (self.ip >= chunk.n_inst) return error.PastChunkEnd;

        const inst = chunk.code[self.ip];

        if (self.config.debug_trace) {
            inst.disassemble(self.ip);
        }

        switch (inst.op) {
            .jmp => {
                self.ip += inst.argi();
            },
            .ret => {},
            // nothing to do with a load for now so we can just print it
            .load => {
                const args = inst.argu();
                self.regs[args.r] = chunk.consts[args.u];
            },
            .move => {
                const args = inst.arg3();
                self.regs[args.r] = self.regs[args.r1];
            },
            .add => {
                const args = inst.arg3();
                self.regs[args.r] = try self.regs[args.r1].add(self.regs[args.r2]);
            },
            .addconst => {
                const args = inst.argu();
                self.regs[args.r] = try self.regs[args.r].add(chunk.consts[args.u]);
            },
            .sub => {
                const args = inst.arg3();
                self.regs[args.r] = try self.regs[args.r1].sub(self.regs[args.r2]);
            },
            .subconst => {
                const args = inst.argu();
                self.regs[args.r] = try self.regs[args.r].sub(chunk.consts[args.u]);
            },
            .mul => {
                const args = inst.arg3();
                self.regs[args.r] = try self.regs[args.r1].mul(self.regs[args.r2]);
            },
            .mulconst => {
                const args = inst.argu();
                self.regs[args.r] = try self.regs[args.r].mul(chunk.consts[args.u]);
            },
            .div => {
                const args = inst.arg3();
                self.regs[args.r] = try self.regs[args.r1].div(self.regs[args.r2]);
            },
            .divconst => {
                const args = inst.argu();
                self.regs[args.r] = try self.regs[args.r].div(chunk.consts[args.u]);
            },
            .define_global => {
                const args = inst.argu();
                const val = chunk.consts[args.u];
                try Value.assertType(.string, val);
                const str = val.string;
                try self.env.map.put(str.slice(), self.regs[args.r]);
            },
            .set_global => {
                const args = inst.argu();
                const val = chunk.consts[args.u];
                try Value.assertType(.string, val);
                try self.env.map.put(val.string.slice(), self.regs[args.r]);
            },
            .eq_true => {
                const args = inst.arg3();
                if (self.regs[args.r].truthy()) {
                    self.ip += 1;
                }
            },
            .@"test" => {
                const args = inst.arg3();
                const compare = if (args.r2 > 0) true else false;
                const r1 = self.regs[args.r1].truthy();
                const res = r1 == compare;
                self.regs[args.r] = Value{ .boolean = r1 };

                if (!res) {
                    self.ip += 1;
                }
            },
            .@"not" => {
                const args = inst.arg3();
                const r1 = self.regs[args.r1].truthy();
                self.regs[args.r] = Value{ .boolean = !r1 };
            },
            .lt => {
                const args = inst.arg3();
                self.regs[args.r] = try self.regs[args.r1].lt(self.regs[args.r2]);
            },
            .lte => {
                const args = inst.arg3();
                self.regs[args.r] = try self.regs[args.r1].lte(self.regs[args.r2]);
            },
            .gt => {
                const args = inst.arg3();
                self.regs[args.r] = try self.regs[args.r1].gt(self.regs[args.r2]);
            },
            .gte => {
                const args = inst.arg3();
                self.regs[args.r] = try self.regs[args.r1].gte(self.regs[args.r2]);
            },
        }

        self.ip += 1;
    }

    /// executes all the instructions in a chunk
    pub fn exec(self: *Self, chunk: *Chunk) !void {
        while (self.ip < chunk.n_inst) {
            try self.step(chunk);
        }

        if (chunk.code[self.ip - 1].op != .ret) {

            // if we get here we ran out of instructions in the chunk
            // this means that the chunk did not end with a return and we
            // should throw an error
            // TODO: this should maybe be handled differently?
            // is there a case where a chunk should just end?
            return error.UnexpectedEnd;
        }
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
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = 2, .u = try chunk.pushConst(.{ .float = 3 }) }));
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
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = b, .u = try chunk.pushConst(.{ .float = 3 }) }));
    // load a constant into c
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = c, .u = try chunk.pushConst(.{ .float = 2 }) }));
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
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = a, .u = try chunk.pushConst(.{ .float = 7 }) }));
    // load a constant into b
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = b, .u = try chunk.pushConst(.{ .float = 3 }) }));
    // a = b + a
    _ = try chunk.pushInst(Inst.init(.add, .{ .r = a, .r1 = a, .r2 = b }));
    // return nada
    _ = try chunk.pushInst(Inst.init(.ret, .{}));

    try vm.exec(&chunk);

    try testing.expectApproxEqAbs(@as(f32, 10.0), vm.regs[a].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 3.0), vm.regs[b].float, eps);
}

test "add constediate" {
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    // the registers we are loading into
    const a = 1;

    var chunk = Chunk{};
    // load a constant into a
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = a, .u = try chunk.pushConst(.{ .float = 7 }) }));
    // load a constant into b
    _ = try chunk.pushInst(Inst.init(.addconst, .{ .r = a, .u = try chunk.pushConst(.{ .float = 3 }) }));
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
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = a, .u = try chunk.pushConst(.{ .float = 7 }) }));
    // load a constant into b
    _ = try chunk.pushInst(Inst.init(.subconst, .{ .r = a, .u = try chunk.pushConst(.{ .float = 3 }) }));

    _ = try chunk.pushInst(Inst.init(.load, .{ .r = b, .u = try chunk.pushConst(.{ .float = 4 }) }));
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = c, .u = try chunk.pushConst(.{ .float = 6 }) }));
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
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = a, .u = try chunk.pushConst(.{ .float = 7 }) }));
    // load a constant into b
    _ = try chunk.pushInst(Inst.init(.mulconst, .{ .r = a, .u = try chunk.pushConst(.{ .float = 3 }) }));

    _ = try chunk.pushInst(Inst.init(.load, .{ .r = b, .u = try chunk.pushConst(.{ .float = -4 }) }));
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = c, .u = try chunk.pushConst(.{ .float = 6 }) }));
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
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = a, .u = try chunk.pushConst(.{ .float = 8 }) }));
    // load a constant into b
    _ = try chunk.pushInst(Inst.init(.divconst, .{ .r = a, .u = try chunk.pushConst(.{ .float = 4 }) }));

    _ = try chunk.pushInst(Inst.init(.load, .{ .r = b, .u = try chunk.pushConst(.{ .float = 6 }) }));
    _ = try chunk.pushInst(Inst.init(.load, .{ .r = c, .u = try chunk.pushConst(.{ .float = -4 }) }));
    _ = try chunk.pushInst(Inst.init(.div, .{ .r = d, .r1 = b, .r2 = c }));

    // return nada
    _ = try chunk.pushInst(Inst.init(.ret, .{}));

    try vm.exec(&chunk);

    try testing.expectApproxEqAbs(@as(f32, 2.0), vm.regs[a].float, eps);
    try testing.expectApproxEqAbs(@as(f32, 6), vm.regs[b].float, eps);
    try testing.expectApproxEqAbs(@as(f32, -4), vm.regs[c].float, eps);
    try testing.expectApproxEqAbs(@as(f32, -1.5), vm.regs[d].float, eps);
}

test "if statement" {
    const code =
        \\(if #t 12 -3)
        \\(if #f 12 -3)
    ;
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    var chunk = try compiler.compile(code, &vm.env, testing.allocator);
    var env = Env.init(testing.allocator);
    defer env.deinit();
    try testing.expect(true == chunk.consts[0].boolean);
    try testing.expect(12 == chunk.consts[1].float);
    try testing.expect(-3 == chunk.consts[2].float);

    try testing.expect(false == chunk.consts[3].boolean);
    try testing.expect(12 == chunk.consts[4].float);
    try testing.expect(-3 == chunk.consts[5].float);

    try vm.exec(&chunk);

    try testing.expect(vm.regs[1].float == 12);
    try testing.expect(vm.regs[5].float == -3);
}

test "addition none" {
    const code =
        \\(+)
    ;
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    try testing.expectError(
        compiler.CompileError.WrongNumberArguments,
        compiler.compile(code, &vm.env, testing.allocator),
    );
}

test "addition one" {
    const code =
        \\(+ 2)
    ;
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    var chunk = try compiler.compile(code, &vm.env, testing.allocator);
    var env = Env.init(testing.allocator);
    defer env.deinit();
    try testing.expect(2 == chunk.consts[0].float);

    try vm.exec(&chunk);

    try testing.expect(vm.regs[1].float == 2);
}

test "addition two" {
    const code =
        \\(+ 1 2)
    ;
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    var chunk = try compiler.compile(code, &vm.env, testing.allocator);
    var env = Env.init(testing.allocator);
    defer env.deinit();
    try testing.expect(1 == chunk.consts[0].float);
    try testing.expect(2 == chunk.consts[1].float);

    try vm.exec(&chunk);

    try testing.expect(vm.regs[1].float == 3);
}

test "addition many" {
    const code =
        \\(+ 1 2 3)
    ;
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    var chunk = try compiler.compile(code, &vm.env, testing.allocator);
    var env = Env.init(testing.allocator);
    defer env.deinit();
    try testing.expect(1 == chunk.consts[0].float);
    try testing.expect(2 == chunk.consts[1].float);
    try testing.expect(3 == chunk.consts[2].float);

    try vm.exec(&chunk);

    try testing.expect(vm.regs[1].float == 6);
}

test "nested arithmetic" {
    const code =
        \\(+ (/ 12 3) (- 2 3) (* 3 2))
    ;
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    var chunk = try compiler.compile(code, &vm.env, testing.allocator);
    var env = Env.init(testing.allocator);
    defer env.deinit();
    try testing.expect(chunk.consts[0].float == 12);
    try testing.expect(chunk.consts[1].float == 3);
    try testing.expect(chunk.consts[2].float == 2);
    try testing.expect(chunk.consts[3].float == 3);
    try testing.expect(chunk.consts[4].float == 3);
    try testing.expect(chunk.consts[5].float == 2);

    try vm.exec(&chunk);

    try testing.expect(vm.regs[1].float == 9);
}

// TODO: make these one test once you have multiple expressions
test "ineq" {
    const code =
        \\(< 3 4)
    ;
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    var chunk = try compiler.compile(code, &vm.env, testing.allocator);
    var env = Env.init(testing.allocator);
    defer env.deinit();
    try testing.expect(3 == chunk.consts[0].float);
    try testing.expect(4 == chunk.consts[1].float);

    try vm.exec(&chunk);

    try testing.expect(vm.regs[1].boolean);
}

test "ineq false" {
    const code =
        \\(> 3 4)
    ;
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    var chunk = try compiler.compile(code, &vm.env, testing.allocator);
    var env = Env.init(testing.allocator);
    defer env.deinit();
    try testing.expect(3 == chunk.consts[0].float);
    try testing.expect(4 == chunk.consts[1].float);

    try vm.exec(&chunk);

    try testing.expect(vm.regs[1].boolean == false);
}

test "ineq eql" {
    const code =
        \\(>= 4 4)
    ;
    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    var chunk = try compiler.compile(code, &vm.env, testing.allocator);
    var env = Env.init(testing.allocator);
    defer env.deinit();
    try testing.expect(4 == chunk.consts[0].float);
    try testing.expect(4 == chunk.consts[1].float);

    try vm.exec(&chunk);

    try testing.expect(vm.regs[1].boolean);
}

test "exec and" {
    const code =
        \\(and #t 33)
    ;

    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    var chunk = try compiler.compile(code, &vm.env, testing.allocator);
    var env = Env.init(testing.allocator);
    defer env.deinit();
    try testing.expect(true == chunk.consts[0].boolean);
    try testing.expect(33 == chunk.consts[1].float);

    try vm.exec(&chunk);

    try testing.expect(vm.regs[1].float == 33);
}

test "exec and false" {
    const code =
        \\(and #f 33 44)
    ;

    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    var chunk = try compiler.compile(code, &vm.env, testing.allocator);
    var env = Env.init(testing.allocator);
    defer env.deinit();
    try testing.expect(false == chunk.consts[0].boolean);
    try testing.expect(33 == chunk.consts[1].float);

    try vm.exec(&chunk);

    try testing.expect(vm.regs[1].boolean == false);
}

test "exec or" {
    const code =
        \\(or #f 33)
    ;

    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    var chunk = try compiler.compile(code, &vm.env, testing.allocator);
    var env = Env.init(testing.allocator);
    defer env.deinit();
    try testing.expect(false == chunk.consts[0].boolean);
    try testing.expect(33 == chunk.consts[1].float);

    try vm.exec(&chunk);

    try testing.expect(vm.regs[1].float == 33);
}

test "exec or true" {
    const code =
        \\(or #t 33 44)
    ;

    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    var chunk = try compiler.compile(code, &vm.env, testing.allocator);
    var env = Env.init(testing.allocator);
    defer env.deinit();
    try testing.expect(true == chunk.consts[0].boolean);
    try testing.expect(33 == chunk.consts[1].float);

    try vm.exec(&chunk);

    try testing.expect(vm.regs[1].boolean == true);
}

test "exec not" {
    const code =
        \\(not #t)
    ;

    var vm = Vm.initConfig(TestConfig, testing.allocator);
    defer vm.deinit();

    var chunk = try compiler.compile(code, &vm.env, testing.allocator);
    var env = Env.init(testing.allocator);
    defer env.deinit();
    try testing.expect(true == chunk.consts[0].boolean);

    try vm.exec(&chunk);

    try testing.expect(vm.regs[1].boolean == false);
}
