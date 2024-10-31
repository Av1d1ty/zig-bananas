const std = @import("std");

pub const NULL = &Object{ .null = .{} };
pub const TRUE = &Object{ .bool = .{ .value = true } };
pub const FALSE = &Object{ .bool = .{ .value = false } };

pub const Object = union(enum) {
    int: Integer,
    bool: Boolean,
    null: Null,

    pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        if (fmt.len != 0) std.fmt.invalidFmtError(fmt, self);
        return switch (self) {
            .null => writer.writeAll("null"),
            inline else => |obj| writer.print("{any}", .{obj.value}),
        };
    }
};

const Null = struct {};

pub const Boolean = struct {
    value: bool,
};

pub const Integer = struct {
    value: i64,
};

pub const Environment = struct {
    outer: ?*const Environment = null,
    store: std.StringHashMap(*const Object),

    pub fn init(allocator: std.mem.Allocator) Environment {
        const store = std.StringHashMap(*const Object).init(allocator);
        return Environment{ .store = store };
    }

    pub fn deinit(self: *@This()) void {
        self.store.deinit();
    }

    pub fn get(self: *const @This(), key: []const u8) ?*const Object {
        const value = self.store.get(key);
        return value orelse if (self.outer) |outer| outer.get(key) else value;
    }

    pub fn put(self: *@This(), key: []const u8, val: *const Object) void {
        self.store.put(key, val) catch unreachable;
    }
};
