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

    pub fn string(self: @This(), buf: []u8) ![]u8 {
        return switch (self) {
            inline else => |st| st.string(buf),
        };
    }
};

pub const Expression = union(enum) {
    ident: Identifier,
    int: Integer,
    pref: Prefix,
    inf: Infix,

    pub fn string(self: @This(), buf: []u8) ![]u8 {
        return switch (self) {
            .ident => |ident| std.fmt.bufPrint(buf, "{s}", .{ident.token.get_value().?}),
            .int => |int| std.fmt.bufPrint(buf, "{d}", .{int.value}),
            .pref, .inf => unreachable,
        };
    }
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

    pub fn string(self: @This(), buf: []u8) ![]u8 {
        return std.fmt.bufPrint(buf, "{s} {?};\n", .{
            @tagName(self.token),
            self.value,
        });
    }
};

pub const ExpressionStatement = struct {
    token: Token,
    expression: *Expression,

    pub fn string(self: @This(), buf: []u8) ![]u8 {
        return self.expression.string(buf);
    }
};

pub const Identifier = struct {
    token: Token,
};

pub const Integer = struct {
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

    // TODO: init function

    pub fn deinit(self: *@This()) void {
        for (self.expression_pointers.items) |exp| {
            std.testing.allocator.destroy(exp);
        }
        self.expression_pointers.deinit();
        self.statements.deinit();
    }

    pub fn string(self: *@This()) ![]u8 {
        var out = std.ArrayList(u8).init(std.testing.allocator);
        var buf: [1024]u8 = undefined;
        for (self.statements.items) |st| {
            try out.appendSlice(try st.string(&buf));
        }
        return out.toOwnedSlice();
    }
    pub fn print(self: *@This()) !void {
        const out_slice = try self.string();
        defer std.testing.allocator.free(out_slice);
        // TODO: write to stdout
        std.debug.print("\n{s}\n", .{out_slice});
    }
};

const expect = std.testing.expect;
test "string" {
    var program = Program{
        .statements = std.ArrayList(Statement).init(std.testing.allocator),
        .expression_pointers = std.ArrayList(*Expression).init(std.testing.allocator),
    };
    defer program.deinit();
    try program.statements.append(.{
        .let = LetStatement{
            .token = .let,
            .name = Identifier{
                .token = .{ .ident = "myVar" },
            },
        },
    });
    const program_sting = try program.string();
    defer std.testing.allocator.free(program_sting);
    // std.debug.print("\n1: {any}\n", .{program_sting});
    try expect(std.mem.eql(u8, program_sting, "let myVar = ;\n"));
}
