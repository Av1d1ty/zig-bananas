const std = @import("std");
const Token = @import("token.zig").Token;

const Node = union(enum) {
    s: Statement,
    e: Expression,

    // pub fn token_literal(self: *@This()) []const u8 {
    //     switch (self) {
    //         inline else => |case| return case.token_literal(),
    //     }
    // }
};

pub const Statement = union(enum) {
    let: LetStatement,
    ret: ReturnStatement,
    exp: ExpressionStatement,
    // pub fn token_literal() void {}
};

pub const Expression = union(enum) {
    ident: Identifier,
    int: IntegerLiteral,
    pref: Prefix,
    // pub fn token_literal() void {}
};

pub const LetStatement = struct {
    token: Token,
    name: Identifier,
};

pub const ReturnStatement = struct {
    token: Token,
    value: ?Expression,
};

pub const ExpressionStatement = struct {
    token: Token,
    expression: Expression,
};

pub const Identifier = struct {
    token: Token,
};

pub const IntegerLiteral = struct {
    token: Token,
    value: i64,
};

pub const Prefix = struct {
    token: Token, // The prefix token, e.g. !
    // operator: u8,
    right: *Expression,
};

pub const Program = struct {
    statements: std.ArrayList(Statement),

    // fn token_literal(self: *@This()) []const u8 {
    //     return if (self.statements.len > 0) self.statements[0].token_literal() else "";
    // }

    pub fn deinit(self: *@This()) void {
        defer self.statements.deinit();
    }

    pub fn print(self: *@This()) void {
        std.debug.print("\n", .{});
        for (self.statements.items, 1..) |statement, i| {
            std.debug.print("{d}: {any}\n", .{ i, statement });
        }
    }
};
