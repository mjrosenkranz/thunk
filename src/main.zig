const std = @import("std");
const thunk = @import("lib.zig");

fn runRepl() !void {
    var repl = try thunk.Repl.init(std.testing.allocator);
    defer repl.deinit();
    try repl.loop();
}

fn runScript(path: []const u8) !void {
    std.log.info("running script: {s}", .{path});
}

pub fn main() anyerror!void {
    const allocator = std.testing.allocator;
    var iter = std.process.ArgIterator.init();
    // skip over executable
    _ = iter.skip();

    // with an argument we run the file given
    if (iter.next(allocator)) |arg| {
        defer allocator.free(arg catch unreachable);
        return runScript(try arg);
    }

    // otherwise we just enter the repl
    return runRepl();
}
