const std = @import("std");
const thunk = @import("lib.zig");
const Vm = thunk.Vm;
const Compiler = thunk.Compiler;
const os = std.os;
const sys = std.os.system;
const STDIN = 0;

const Repl = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    vm: Vm,
    compiler: Compiler,
    og: os.termios,

    pub fn init(allocator: std.mem.Allocator) !Repl {
        // TODO: setup interrupts

        var self = Self{
            .allocator = allocator,
            .vm = Vm{},
            .compiler = Compiler{},
            .og = try os.tcgetattr(STDIN),
        };

        // enable raw mode
        // first get the original termios
        // set raw mode
        var raw = self.og;
        // local flags
        // turn off echo, get bytes one at a time, turn off signals (int and stp)
        // turn off ^V and ^O

        raw.lflag &= ~(@as(u16, sys.ECHO | sys.ICANON | sys.ISIG | sys.IEXTEN));
        // turn off software flow control (diables ^S and ^Q), and carriage returns
        raw.iflag &= ~(@as(u16, sys.IXON | sys.ICRNL | sys.BRKINT | sys.INPCK | sys.ISTRIP));
        // set 8 bits per byte
        raw.cflag |= sys.CS8;
        // turn off output processing
        raw.oflag &= ~(@as(u16, sys.OPOST));

        // add a timout
        // raw.cc[os.O.VMIN] = 0;
        // raw.cc[os.VTIME] = 1;

        // flush changes
        try os.tcsetattr(STDIN, sys.TCSA.FLUSH, raw);

        return self;
    }

    pub fn deinit(self: Self) void {
        self.vm.deinit();
        // restore original settings
        os.tcsetattr(STDIN, sys.TCSA.FLUSH, self.og) catch unreachable;
    }

    pub fn loop(self: *Self) !void {
        _ = self;
        var reader = std.io.getStdIn().reader();
        var b: u8 = 0;
        while (true) {
            b = try reader.readByte();

            switch (b) {
                ('d' & 0x1f) => break,
                '\r' => std.debug.print("\r\n", .{}),
                else => {
                    if (b < 32) {
                        std.debug.print("unknown ctrl: {}\r\n", .{b});
                    } else {
                        std.debug.print("{c}", .{b});
                    }
                },
            }
        }
    }
};

pub fn main() anyerror!void {
    var repl = try Repl.init(std.testing.allocator);
    defer repl.deinit();
    try repl.loop();
}
