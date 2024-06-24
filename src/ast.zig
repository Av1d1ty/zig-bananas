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
    func: Function,

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
            .func => |func| {
                try writer.print("{}", .{func.token});
                try writer.writeAll(" (");
                for (1.., func.parameters) |i, param| {
                    try writer.print("{s}", .{param.value});
                    if (func.parameters.len != 1 and i != func.parameters.len) try writer.writeAll(", ");
                }
                try writer.writeAll(") ");
                try writer.print("{{ {} }}", .{func.body});
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

pub const Function = struct {
    token: Token,
    parameters: []Identifier,
    body: BlockStatement,
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
    /// Used to deallocate parsed statements and expressions
    allocator: std.mem.Allocator,
    statements: []Statement,
    expression_pointers: []*const Expression,

    pub fn deinit(self: *@This()) void {
        for (self.expression_pointers) |exp| {
            switch (exp.*) {
                .if_exp => |ex| {
                    self.allocator.free(ex.consequence.statements);
                    if (ex.alternative) |alt| {
                        self.allocator.free(alt.statements);
                    }
                },
                .func => |ex| {
                    self.allocator.free(ex.parameters);
                    self.allocator.free(ex.body.statements);
                },
                else => {},
            }
            self.allocator.destroy(exp);
        }
        self.allocator.free(self.expression_pointers);
        self.allocator.free(self.statements);
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        if (fmt.len != 0) std.fmt.invalidFmtError(fmt, self);
        for (self.statements) |st| {
            try writer.print("{}\n", .{st});
        }
    }
};

test "string" {
    var statements = std.ArrayList(Statement).init(std.testing.allocator);
    var expressions = std.ArrayList(*const Expression).init(std.testing.allocator);
    try statements.append(.{
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
    var program = Program{
        .allocator = std.testing.allocator,
        .statements = try statements.toOwnedSlice(),
        .expression_pointers = try expressions.toOwnedSlice(),
    };
    defer program.deinit();
    try std.testing.expectFmt("let myVar = 42;\n", "{}", .{program});
}
