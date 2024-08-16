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
            .null => writer.print("null", .{}),
            inline else => |obj| writer.print("{any}", .{obj.value}),
        };
    }
};

const Null = struct {};

const Boolean = struct {
    value: bool,
};

pub const Integer = struct {
    value: i64,
};
