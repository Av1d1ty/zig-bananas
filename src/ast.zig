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
            // TODO: print expression value
            .let => |let| writer.print("{s} {s} = {};\n", .{
                @tagName(let.token),
                let.name.value,
                let.value,
            }),
            .ret => |ret| writer.print("{s} {?};\n", .{ @tagName(ret.token), ret.value }),
            .exp => |exp| writer.print("{};\n", .{exp.expression}),
        };
    }
};

pub const Expression = union(enum) {
    ident: Identifier,
    int: Integer,
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
            .pref => |pref| writer.print("({s}{})", .{ pref.token.get_string(), pref.right }),
            .inf => |inf| writer.print("({} {s} {})", .{
                inf.left,
                inf.token.get_string(),
                inf.right,
            }),
        };
    }
};

pub const LetStatement = struct {
    token: Token,
    name: Identifier,
    value: *Expression,
};

pub const ReturnStatement = struct {
    token: Token,
    value: ?*Expression,
};

pub const ExpressionStatement = struct {
    // token: Token,
    expression: *Expression,
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,
};

pub const Integer = struct {
    token: Token,
    value: i64,
};

pub const Prefix = struct {
    token: Token, // The prefix token, e.g. `!`
    right: *Expression,
};

pub const Infix = struct {
    token: Token,
    left: *Expression,
    right: *Expression,
};

pub const Program = struct {
    allocator: std.mem.Allocator,
    statements: std.ArrayList(Statement),
    expression_pointers: std.ArrayList(*Expression),

    pub fn init(allocator: std.mem.Allocator) !*Program {
        const program_ptr = try allocator.create(Program);
        program_ptr.* = Program{
            .allocator = allocator,
            .statements = std.ArrayList(Statement).init(allocator),
            .expression_pointers = std.ArrayList(*Expression).init(allocator),
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

// test "string" {
//     var program = try Program.init(std.testing.allocator);
//     defer program.deinit();
//     try program.statements.append(.{
//         .let = LetStatement{
//             .token = .let,
//             .name = Identifier{
//                 .token = .{ .ident = "myVar" },
//                 .value = "myVar",
//             },
//             .value = Expression{ .inf = Infix{
//                 .token = .plus,
//                 .left = Expression{ .int = Integer{ .token = .{ .int = "1" }, .value = 1 } },
//                 .right = Expression{ .ident = Identifier{ .token = .{ .ident = "b" }, .value = "b" } },
//             } },
//         },
//     });
//     try std.testing.expectFmt("let myVar = 1 + b;\n", "{}", .{program});
// }
