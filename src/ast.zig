const std = @import("std");
const Token = @import("token.zig").Token;

pub const Statement = union(enum) {
    let: LetStatement,
    ret: ReturnStatement,
    exp: ExpressionStatement,
    blk: BlockStatement,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        if (fmt.len != 0) std.fmt.invalidFmtError(fmt, self);
        return switch (self) {
            .let => |let| writer.print("{s} {s} = {};", .{ "let", let.name.value, let.value }),
            .ret => |ret| writer.print("{s} {?};", .{ "return", ret.value }),
            .exp => |exp| writer.print("{};", .{exp.expression}), // TODO: no `;` for ifs
            .blk => |blk| for (blk.statements) |st| try writer.print("{}\n", .{st}),
        };
    }
};

pub const Expression = union(enum) {
    ident: Identifier,
    int: Integer,
    bool: Boolean,
    pref: Prefix,
    inf: Infix,
    if_exp: If,

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
            .if_exp => |if_exp| blk: {
                if (if_exp.alternative) |alt| {
                    break :blk writer.print(
                        "if {} {{ {} }} else {{ {?} }}",
                        .{ if_exp.condition, if_exp.consequence, alt },
                    );
                } else {
                    break :blk writer.print(
                        "if {} {{ {} }}",
                        .{ if_exp.condition, if_exp.consequence },
                    );
                }
            },
        };
    }
};

pub const LetStatement = struct {
    token: Token, // TODO: remove, no use
    name: Identifier,
    value: *const Expression,
};

pub const ReturnStatement = struct {
    token: Token, // TODO: remove, no use
    value: ?*const Expression,
};

pub const ExpressionStatement = struct {
    // token: Token,
    expression: *const Expression,
};

pub const Identifier = struct {
    token: Token, // TODO: remove, no use
    value: []const u8,
};

pub const Integer = struct {
    token: Token, // TODO: remove, no use
    value: i64,
};

pub const Boolean = struct {
    token: Token, // TODO: remove, no use
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

pub const If = struct {
    token: Token, // TODO: remove, no use
    condition: *const Expression,
    consequence: BlockStatement,
    alternative: ?BlockStatement,
};

pub const BlockStatement = struct {
    token: Token, // TODO: remove, no use
    statements: []Statement,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        if (fmt.len != 0) std.fmt.invalidFmtError(fmt, self);
        return for (self.statements) |st| try writer.print("{}", .{st});
    }
};

pub const Program = struct {
    allocator: std.mem.Allocator,
    statements: std.ArrayList(Statement),
    // TODO: use Slice
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
            switch (exp.*) {
                .if_exp => |ex| {
                    self.allocator.free(ex.consequence.statements);
                    if (ex.alternative) |alt| {
                        self.allocator.free(alt.statements);
                    }
                },
                else => {},
            }
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
        for (self.statements.items) |st| {
            try writer.print("{}\n", .{st});
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
