const std = @import("std");
const vm = @import("vm.zig");
const chunk = @import("chunk.zig");
const scanner = @import("scanner.zig");

test "basic test" {
    std.testing.refAllDecls(@This());
}
