const std = @import("std");
const Token = @import("token.zig").Token;

const Node = union(enum) {
    s: Statement,
    e: Expression,
};

pub const Statement = union(enum) {
    let: LetStatement,
    ret: ReturnStatement,
    exp: ExpressionStatement,
};

pub const Expression = union(enum) {
    ident: Identifier,
    int: IntegerLiteral,
    pref: Prefix,
    inf: Infix,
};

pub const LetStatement = struct {
    token: Token,
    name: Identifier,

    pub fn string(self: @This(), buf: []u8) ![]u8 {
        return std.fmt.bufPrint(buf, "{s} {s} = ;\n", .{
            @tagName(self.token),
            self.name.token.get_value().?,
        });
    }
};

pub const ReturnStatement = struct {
    token: Token,
    value: ?Expression,
};

pub const ExpressionStatement = struct {
    token: Token,
    expression: *Expression,
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
    right: *Expression,
};

pub const Infix = struct {
    token: Token,
    left: *Expression,
    right: *Expression,
};

pub const Program = struct {
    statements: std.ArrayList(Statement),
    expression_pointers: std.ArrayList(*Expression),

    pub fn deinit(self: *@This()) void {
        for (self.expression_pointers.items) |exp| {
            std.testing.allocator.destroy(exp);
        }
        self.expression_pointers.deinit();
        self.statements.deinit();
    }

    pub fn print(self: *@This()) !void {
        var out = std.ArrayList(u8).init(std.testing.allocator);
        var buf: [1024]u8 = undefined;
        for (self.statements.items) |st| {
            switch (st) {
                .let => |let| {
                    try out.appendSlice(try let.string(&buf));
                },
                else => {},
            }
        }
        // TODO: write to stdout
        const out_slice = try out.toOwnedSlice();
        defer std.testing.allocator.free(out_slice);
        std.debug.print("\n{s}", .{out_slice});
    }
};
