const std = @import("std");

pub fn AssocList(
    comptime K: type,
    comptime V: type,
    comptime Eq: *const fn (a: K, b: K) bool,
) type {
    const N = 32;
    return struct {
        const Self = @This();

        keys: [N]K = undefined,
        values: [N]V = undefined,
        idx: usize = 0,

        /// creates a new association in the list
        pub fn assoc(self: *Self, k: K, v: V) !void {
            if (self.idx >= N) return error.ExceedsCapacity;

            self.keys[self.idx] = k;
            self.values[self.idx] = v;

            self.idx += 1;
        }

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

test {
    const IntAssoc = AssocList(u32, u8, struct {
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
