const std = @import("std");
const thunk = @import("lib.zig");

fn runRepl(allocator: std.mem.Allocator) !void {
    var repl = try thunk.repl.Repl.init(allocator);
    defer repl.deinit();
    try repl.loop();
}

fn runScript(path: []const u8) !void {
    // read the file into code
    std.log.info("running script: {s}", .{path});
}

pub fn main() anyerror!void {
    var iter = std.process.ArgIterator.init();
    // skip over executable
    _ = iter.skip();

    // with an argument we run the file given
    if (iter.next()) |arg| {
        return runScript(arg);
    }

    // otherwise we just enter the repl
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    return runRepl(alloc);
}
