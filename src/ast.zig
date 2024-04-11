const std = @import("std");
const Token = @import("token.zig").Token;

const Node = union(enum) {
    s: Statement,
    e: Expression,

    pub fn token_literal(self: *@This()) []const u8 {
        switch (self) {
            inline else => |case| return case.token_literal(),
        }
    }
};

pub const Statement = union(enum) {
    let: *LetStatement,
    pub fn token_literal() void {}
};

pub const Expression = struct {
    pub fn token_literal() void {}
};

pub const LetStatement = struct {
    token: Token,
    name: Identifier,
    // TODO: remove optional
    value: ?Expression = null,
};

pub const Identifier = struct {
    token: Token,
    // value: []const u8,
};

pub const Program = struct {
    statements: std.ArrayList(Statement),

    fn token_literal(self: *@This()) []const u8 {
        return if (self.statements.len > 0) self.statements[0].token_literal() else "";
    }
};
