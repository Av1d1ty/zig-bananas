const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Node = @import("ast.zig").Node;
const obj = @import("object.zig");

pub fn eval(node: Node) *const obj.Object {
    return switch (node) {
        .program => |prog| blk: {
            var result = obj.NULL;
            for (prog.statements) |stmt| result = eval(Node{ .statement = stmt });
            break :blk result;
        },
        .statement => |stmt| switch (stmt) {
            .exp => |exp| eval(Node{ .expression = exp.expression }),
            else => unreachable,
        },
        .expression => |expr| switch (expr.*) {
            .int => |int| &obj.Object{ .int = obj.Integer{ .value = int.value } },
            .bool => |bool_| if (bool_.value) obj.TRUE else obj.FALSE,
            else => unreachable,
        },
    };
}
fn test_eval(input: []const u8) !*const obj.Object {
    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer, std.testing.allocator);
    var program = try parser.parse_program();

    defer parser.deinit();
    defer program.deinit();

    // const ptr = std.testing.allocator.create(obj.Object) catch unreachable;
    // errdefer std.testing.allocator.destroy(ptr);
    // defer std.testing.allocator.destroy(ptr);

    return eval(Node{ .program = &program });
}

test "integer" {
    const cases = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "5", .expected = 5 },
        .{ .input = "10", .expected = 10 },
    };
    for (cases) |case| {
        try std.testing.expectEqual(case.expected, (try test_eval(case.input)).int.value);
        // std.debug.print("\nint: {d}", .{(try test_eval(case.input)).int.value});
    }
}

test "boolean" {
    const cases = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{ .input = "true", .expected = true },
        .{ .input = "false", .expected = false },
    };
    for (cases) |case| {
        try std.testing.expectEqual(case.expected, (try test_eval(case.input)).bool.value);
    }
}
