const std = @import("std");

pub const AssocError = error{
    ExceedsCapacity,
};

pub fn Assoc(
    comptime N: usize,
    comptime K: type,
    comptime V: type,
    comptime Eq: *const fn (a: K, b: K) bool,
) type {
    return struct {
        const Self = @This();

        idx: usize = 0,
        keys: [N]K = undefined,
        values: [N]V = undefined,

        /// creates a new association in the list
        pub fn assoc(self: *Self, k: K, v: V) !void {
            if (self.idx >= N) return AssocError.ExceedsCapacity;

            self.keys[self.idx] = k;
            self.values[self.idx] = v;

            self.idx += 1;
        }

        /// removes an association from the list
        /// if the key cannot be found, does nothing
        pub fn dissoc(self: *Self, k: K) void {
            for (self.keys[0..self.idx]) |o, i| {
                if (Eq(k, o)) {
                    // if this is the last idx then we done
                    self.idx -= 1;
                    if (i != self.idx) {
                        // otherwise, swap the last value and key with the index of this one
                        self.keys[i] = self.keys[self.idx];
                        self.values[i] = self.values[self.idx];
                    }
                    return;
                }
            }
        }

        /// returns the value associated with the key, if there is one
        pub fn get(self: Self, k: K) ?V {
            for (self.keys[0..self.idx]) |o, i| {
                if (Eq(k, o)) {
                    return self.values[i];
                }
            }

            return null;
        }
    };
}

test "assoc" {
    const IntAssoc = Assoc(32, u32, u8, struct {
        pub fn f(a: u32, b: u32) bool {
            return a == b;
        }
    }.f);

    var a = IntAssoc{};
    try a.assoc(5, 'a');
    try a.assoc(7, 'b');

    try std.testing.expect(a.get(5).? == 'a');
    try std.testing.expect(a.get(7).? == 'b');
    try std.testing.expect(a.get(13) == null);
}

test "dissoc" {
    const IntAssoc = Assoc(32, u32, u8, struct {
        pub fn f(a: u32, b: u32) bool {
            return a == b;
        }
    }.f);

    var a = IntAssoc{};
    try a.assoc(5, 'a');
    try a.assoc(7, 'b');
    try a.assoc(3, 'z');

    try std.testing.expect(a.get(5).? == 'a');
    try std.testing.expect(a.get(7).? == 'b');
    try std.testing.expect(a.get(3).? == 'z');

    try std.testing.expect(a.idx == 3);

    a.dissoc(5);
    // should be noop
    a.dissoc(13);

    try std.testing.expect(a.idx == 2);
    try std.testing.expect(a.keys[0] == 3);
    try std.testing.expect(a.keys[1] == 7);
    try std.testing.expect(a.values[0] == 'z');
    try std.testing.expect(a.values[1] == 'b');
    try std.testing.expect(a.get(7).? == 'b');
    try std.testing.expect(a.get(3).? == 'z');

    a.dissoc(3);
    try std.testing.expect(a.idx == 1);
    try std.testing.expect(a.keys[0] == 7);
    try std.testing.expect(a.get(7).? == 'b');

    a.dissoc(7);
    try std.testing.expect(a.idx == 0);
}
