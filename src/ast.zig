const std = @import("std");
const Token = @import("token.zig").Token;

pub const Statement = union(enum) {
    let: LetStatement,
    ret: ReturnStatement,
    exp: ExpressionStatement,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        if (fmt.len != 0) std.fmt.invalidFmtError(fmt, self);
        return switch (self) {
            .let => |let| writer.print("{s} {s} = {};\n", .{ "let", let.name.value, let.value }),
            .ret => |ret| writer.print("{s} {?};\n", .{ "return", ret.value }),
            .exp => |exp| writer.print("{};\n", .{exp.expression}),
        };
    }
};

pub const Expression = union(enum) {
    ident: Identifier,
    int: Integer,
    bool: Boolean,
    pref: Prefix,
    inf: Infix,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        if (fmt.len != 0) std.fmt.invalidFmtError(fmt, self);
        return switch (self) {
            .ident => |ident| writer.print("{s}", .{ident.value}),
            .int => |int| writer.print("{d}", .{int.value}),
            .bool => |boolean| writer.print("{}", .{boolean.value}),
            .pref => |pref| writer.print("({}{})", .{ pref.token, pref.right }),
            .inf => |inf| writer.print("({} {} {})", .{ inf.left, inf.token, inf.right }),
        };
    }
};

pub const LetStatement = struct {
    token: Token,
    name: Identifier,
    value: *const Expression,
};

pub const ReturnStatement = struct {
    token: Token,
    value: ?*const Expression,
};

pub const ExpressionStatement = struct {
    // token: Token,
    expression: *const Expression,
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,
};

pub const Integer = struct {
    token: Token,
    value: i64,
};

pub const Boolean = struct {
    token: Token,
    value: bool,
};

pub const Prefix = struct {
    token: Token, // The prefix token, e.g. `!`
    right: *const Expression,
};

pub const Infix = struct {
    token: Token,
    left: *const Expression,
    right: *const Expression,
};

pub const Program = struct {
    allocator: std.mem.Allocator,
    statements: std.ArrayList(Statement),
    expression_pointers: std.ArrayList(*const Expression),

    pub fn init(allocator: std.mem.Allocator) !*Program {
        const program_ptr = try allocator.create(Program);
        program_ptr.* = Program{
            .allocator = allocator,
            .statements = std.ArrayList(Statement).init(allocator),
            .expression_pointers = std.ArrayList(*const Expression).init(allocator),
        };
        return program_ptr;
    }

    pub fn deinit(self: *@This()) void {
        for (self.expression_pointers.items) |exp| {
            self.allocator.destroy(exp);
        }
        self.expression_pointers.deinit();
        self.statements.deinit();
        self.allocator.destroy(self);
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        if (fmt.len != 0) std.fmt.invalidFmtError(fmt, self);
        // try writer.writeAll("\n");
        for (self.statements.items) |st| {
            try writer.print("{}", .{st});
        }
    }
};

test "string" {
    var program = try Program.init(std.testing.allocator);
    defer program.deinit();
    try program.statements.append(.{
        .let = LetStatement{
            .token = .let,
            .name = Identifier{
                .token = .{ .ident = "myVar" },
                .value = "myVar",
            },
            .value = &(Expression{ .int = .{
                .token = .{ .int = "42" },
                .value = 42,
            } }),
        },
    });
    try std.testing.expectFmt("let myVar = 42;\n", "{}", .{program});
}
