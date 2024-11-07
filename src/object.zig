const std = @import("std");
const ast = @import("ast.zig");

pub const NULL = &Object{ .null = .{} };
pub const TRUE = &Object{ .bool = .{ .value = true } };
pub const FALSE = &Object{ .bool = .{ .value = false } };

pub const Object = union(enum) {
    int: Integer,
    str: String,
    bool: Boolean,
    func: Function,
    null: Null,

    pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        if (fmt.len != 0) std.fmt.invalidFmtError(fmt, self);
        return switch (self) {
            .null => writer.writeAll("null"),
            .func => |func| writer.print("{}", .{func}),
            .str => |str| writer.print("\"{s}\"", .{str.value}),
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

pub const String = struct {
    value: []const u8,
};

pub const Function = struct {
    parameters: []ast.Identifier,
    body: ast.BlockStatement,
    env: *Environment,

    pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        if (fmt.len != 0) std.fmt.invalidFmtError(fmt, self);
        try writer.writeAll("fn(");
        for (1.., self.parameters) |i, param| {
            try writer.print("{}", .{param});
            if (self.parameters.len != 1 and i != self.parameters.len) try writer.writeAll(", ");
        }
        try writer.writeAll(") ");
        try writer.print("{{ {} }}", .{self.body});
    }
};

pub const Environment = struct {
    outer: ?*const Environment,
    store: std.StringHashMap(*const Object),

    pub fn init(allocator: std.mem.Allocator, outer_env: ?*const Environment) Environment {
        const store = std.StringHashMap(*const Object).init(allocator);
        return Environment{ .store = store, .outer = outer_env };
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
