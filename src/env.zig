//! This module level struct will house the our
//! environment type.
//! For now that is just going to be a hashmap but will
//! need to be a more efficient table of some kind.
const std = @import("std");
const Value = @import("value.zig").Value;

// TODO: replace with a hash table type
pub const Env = struct {
    const Self = @This();

    map: std.hash_map.StringHashMap(Value),

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .map = std.hash_map.StringHashMap(Value).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.map.deinit();
    }
};
