const std = @import("std");
const chunk = @import("chunk.zig");
const inst = @import("inst.zig");
const scanner = @import("scanner.zig");
const value = @import("value.zig");
const vm = @import("vm.zig");

test "basic test" {
    std.testing.refAllDecls(@This());
}
