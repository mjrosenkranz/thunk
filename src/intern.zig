const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

/// Indexes into the bytes by offset
/// Hashes by the contents of the slice
const OffsetCtx = struct {
    bytes: *std.ArrayListUnmanaged(u8),

    /// checks if offsets are equal
    pub fn eql(self: OffsetCtx, a: u32, b: u32) bool {
        _ = self;
        return a == b;
    }

    /// gets the string by offset and hashes it
    pub fn hash(self: OffsetCtx, x: u32) u64 {
        const x_slice = mem.spanZ(@ptrCast([*:0]const u8, self.string_bytes.items.ptr) + x);
        return std.hash_map.hashString(x_slice);
    }
};

const SliceAdapter = struct {
    string_bytes: *std.ArrayListUnmanaged(u8),

    pub fn eql(self: SliceAdapter, a_slice: []const u8, b: u32) bool {
        const b_slice = mem.spanZ(@ptrCast([*:0]const u8, self.string_bytes.items.ptr) + b);
        return mem.eql(u8, a_slice, b_slice);
    }

    pub fn hash(self: SliceAdapter, adapted_key: []const u8) u64 {
        _ = self;
        return std.hash_map.hashString(adapted_key);
    }
};

test "hash contexts" {
    const gpa = std.testing.allocator;

    var foo: Intern = .{
        .string_bytes = .{},
        .string_table = .{},
    };
    defer foo.string_bytes.deinit(gpa);
    defer foo.string_table.deinit(gpa);

    const index_context: OffsetCtx = .{ .string_bytes = &foo.string_bytes };

    const hello_index = @intCast(u32, foo.string_bytes.items.len);
    try foo.string_bytes.appendSlice(gpa, "hello\x00");
    try foo.string_table.putContext(gpa, hello_index, {}, index_context);

    const world_index = @intCast(u32, foo.string_bytes.items.len);
    try foo.string_bytes.appendSlice(gpa, "world\x00");
    try foo.string_table.putContext(gpa, world_index, {}, index_context);

    // now we want to check if a string exists based on a string literal
    const slice_context: SliceAdapter = .{ .string_bytes = &foo.string_bytes };
    const found_entry = foo.string_table.getEntryAdapted(@as([]const u8, "world"), slice_context).?;
    try std.testing.expectEqual(found_entry.key_ptr.*, world_index);
}

/// Intern strings in this store!
const Intern = struct {
    bytes: std.ArrayListUnmanaged(u8),
    /// Key is string_bytes index.
    table: std.HashMapUnmanaged(u32, void, OffsetCtx, std.hash_map.default_max_load_percentage),

    allocator: Allocator,

    const Hash = u32;

    pub fn init(allocator: Allocator) void {
        return .{
            .bytes = .{},
            .table = .{},
            .allocator = allocator,
        };
    }

    /// pushs a string into the intern table
    pub fn push(self: *Intern, str: []const u8) !Hash {
        const offset = @intCast(u32, self.bytes.itesm.len);
        self.bytes.appendSlice(self.allocator, str);

        const ctx: OffsetCtx = .{ .string_bytes = &self.bytes };
        // put void into the table
        try self.table.putContext(self.allocator, offset, {}, ctx);
    }

    /// (maybe) gets contents of entry if hash exists
    pub fn get(self: *Intern, hash: Hash) ?[]const u8 {
        _ = self;
        _ = hash;
    }
};

test "intern" {
    const allocator = std.testing.allocator;

    var intern = Intern.init(allocator);
    intern.push();
}
