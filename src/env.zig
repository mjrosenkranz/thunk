//! This module level struct will house the our
//! environment type.
//! For now that is just going to be a hashmap but will
//! need to be a more efficient table of some kind.
const std = @import("std");
const Value = @import("value.zig").Value;

pub const Env = std.hash_map.StringHashMap(Value);

pub var global = Env.init(std.testing.allocator);
