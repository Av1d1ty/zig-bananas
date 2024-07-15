const std = @import("std");
const Token = @import("token.zig").Token;

pub const Statement = union(enum) {
    let: LetStatement,
    ret: ReturnStatement,
    exp: ExpressionStatement,
    blk: BlockStatement,

    pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        if (fmt.len != 0) std.fmt.invalidFmtError(fmt, self);
        return switch (self) {
            .let => |let| writer.print("{s} {s} = {}", .{ "let", let.name.value, let.value }),
            .ret => |ret| writer.print("{s} {?}", .{ "return", ret.value }),
            .exp => |exp| writer.print("{}", .{exp.expression}),
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
    call: Call,

    pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        if (fmt.len != 0) std.fmt.invalidFmtError(fmt, self);
        return switch (self) {
            .ident => |ident| writer.print("{}", .{ident}),
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
            .func => |func| try writer.print("{}", .{func}),
            .call => |call| {
                switch (call.func) {
                    inline .func, .ident => |func| try writer.print("{}", .{func}),
                }
                try writer.writeAll("(");
                for (1.., call.arguments) |i, arg| {
                    try writer.print("{}", .{arg});
                    if (call.arguments.len != 1 and i != call.arguments.len) try writer.writeAll(", ");
                }
                try writer.writeAll(")");
            },
        };
    }
};

// TODO: reconsider Token usage in the structs below

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

    pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        if (fmt.len != 0) std.fmt.invalidFmtError(fmt, self);
        return writer.print("{s}", .{self.value});
    }
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

pub const If = struct {
    token: Token,
    condition: *const Expression,
    consequence: BlockStatement,
    alternative: ?BlockStatement,
};

pub const Function = struct {
    token: Token,
    parameters: []Identifier,
    body: BlockStatement,

    pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        if (fmt.len != 0) std.fmt.invalidFmtError(fmt, self);
        try writer.print("{}", .{self.token});
        try writer.writeAll(" (");
        for (1.., self.parameters) |i, param| {
            try writer.print("{}", .{param});
            if (self.parameters.len != 1 and i != self.parameters.len) try writer.writeAll(", ");
        }
        try writer.writeAll(") ");
        try writer.print("{{ {} }}", .{self.body});
    }
};

pub const Call = struct {
    token: Token, // The '(' token
    func: union(enum) { func: Function, ident: Identifier },
    arguments: []Expression,
};

pub const BlockStatement = struct {
    token: Token,
    statements: []Statement,

    pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        if (fmt.len != 0) std.fmt.invalidFmtError(fmt, self);
        return for (self.statements) |st| try writer.print("{};", .{st});
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
                .call => |call| {
                    self.allocator.free(call.arguments);
                },
                else => {},
            }
            self.allocator.destroy(exp);
        }
        self.allocator.free(self.expression_pointers);
        self.allocator.free(self.statements);
    }

    pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        if (fmt.len != 0) std.fmt.invalidFmtError(fmt, self);
        for (self.statements) |st| {
            try writer.print("{}\n", .{st});
            // if (st == .exp and (st.exp.expression.* == .func or st.exp.expression.* == .if_exp)) {
            //     try writer.writeAll("\n");
            //     continue;
            // }
            // try writer.writeAll("\n");
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
    try std.testing.expectFmt("let myVar = 42\n", "{}", .{program});
}
