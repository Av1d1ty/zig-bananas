const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Node = @import("ast.zig").Node;
const obj = @import("object.zig");

pub const Evaluator = struct {
    allocator: std.mem.Allocator,

    pub fn eval(self: @This(), node: Node) !*const obj.Object {
        return switch (node) {
            .program => |prog| blk: {
                var result = obj.NULL;
                for (prog.statements) |stmt| result = try self.eval(Node{ .statement = stmt });
                // std.debug.print("\nresult: {}\n", .{result});
                break :blk result;
            },
            .statement => |stmt| switch (stmt) {
                .exp => |exp| try self.eval(Node{ .expression = exp.expression }),
                else => unreachable,
            },
            .expression => |expr| switch (expr.*) {
                .int => |int| self.alloc_obj(obj.Object{ .int = obj.Integer{ .value = int.value } }),
                .bool => |bool_| if (bool_.value) obj.TRUE else obj.FALSE,
                .pref => |pref| blk: {
                    const right = try self.eval(Node{ .expression = pref.right });
                    // TODO: store operators in a separate union, so that this `else` won't be needed
                    break :blk switch (pref.operator) {
                        .bang => switch (right.*) {
                            .bool => if (right == obj.FALSE) obj.TRUE else obj.FALSE,
                            .null => obj.TRUE,
                            .int => |int| if (int.value == 0) obj.TRUE else obj.FALSE,
                        },
                        .minus => switch (right.*) {
                            .int => |int| self.alloc_obj(
                                obj.Object{ .int = obj.Integer{ .value = -int.value } },
                            ),
                            else => error.InvalidPrefixOperand,
                        },
                    };
                },
                .inf => |inf| blk: {
                    // const left = try self.eval(Node{ .expression = inf.left });
                    // const right = try self.eval(Node{ .expression = inf.right });
                    break :blk switch (inf.operator) {
                        .plus => error.Unimplemented,
                        .minus => error.Unimplemented,
                        .asterisk => error.Unimplemented,
                        .slash => error.Unimplemented,
                        .gt => error.Unimplemented,
                        .lt => error.Unimplemented,
                        .eq => error.Unimplemented,
                        .not_eq => error.Unimplemented,
                    };
                },
                else => unreachable,
            },
        };
    }

    fn alloc_obj(self: @This(), object: obj.Object) *const obj.Object {
        const ptr = self.allocator.create(obj.Object) catch unreachable;
        ptr.* = object;
        return ptr;
    }

    // fn eval_prefix_expr(
    //     self: @This(),
    //     operator: ast.Operator,
    //     left: *const obj.Object,
    // ) !*const obj.Object {}

    // fn eval_infix_expr(
    //     self: @This(),
    //     operator: ast.Operator,
    //     left: *const obj.Object,
    //     right: *const obj.Object,
    // ) !*const obj.Object {}
};

fn test_eval(input: []const u8) !obj.Object {
    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer, std.testing.allocator);
    var program = try parser.parse_program();
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var evaluator = Evaluator{ .allocator = arena.allocator() };

    defer parser.deinit();
    defer program.deinit();
    defer arena.deinit();

    const eval_result = try evaluator.eval(Node{ .program = &program });
    return eval_result.*;
}

test "integer" {
    const cases = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "5", .expected = 5 },
        .{ .input = "10", .expected = 10 },
        .{ .input = "-5", .expected = -5 },
        .{ .input = "-10", .expected = -10 },
    };
    for (cases) |case| {
        const result = try test_eval(case.input);
        switch (result) {
            .int => |actual| try std.testing.expectEqual(case.expected, actual.value),
            inline else => |actual| {
                std.log.err("\nExpected INT, got {}\n", .{actual});
                return error.UnexpectedObjectType;
            },
        }
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
        const result = try test_eval(case.input);
        switch (result) {
            .bool => |actual| try std.testing.expectEqual(case.expected, actual.value),
            inline else => |actual| {
                std.log.err("\nExpected BOOL, got {}\n", .{actual});
                return error.UnexpectedObjectType;
            },
        }
    }
}

test "bang" {
    const cases = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{ .input = "!true", .expected = false },
        .{ .input = "!false", .expected = true },
        .{ .input = "!!true", .expected = true },
        .{ .input = "!!false", .expected = false },
    };
    for (cases) |case| {
        const result = try test_eval(case.input);
        switch (result) {
            .bool => |actual| try std.testing.expectEqual(case.expected, actual.value),
            inline else => |actual| {
                std.log.err("\nExpected BOOL, got {}\n", .{actual});
                return error.UnexpectedObjectType;
            },
        }
    }
}
