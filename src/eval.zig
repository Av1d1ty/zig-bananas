const std = @import("std");
const ast = @import("ast.zig");
const obj = @import("object.zig");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Node = ast.Node;
const assert = std.debug.assert;

pub const Evaluator = struct {
    allocator: std.mem.Allocator,
    is_returning: bool = false,

    pub fn eval(self: *@This(), node: Node) !*const obj.Object {
        return switch (node) {
            .program => |prog| self.eval_statements(prog.statements),
            .statement => |stmt| switch (stmt) {
                .blk => |blk| try self.eval_statements(blk.statements),
                .exp => |exp| try self.eval(Node{ .expression = exp.expression }),
                .ret => |ret| if (ret.value) |val| try self.eval(Node{ .expression = val }) else obj.NULL,
                else => error.Unimplemented,
            },
            .expression => |expr| switch (expr.*) {
                .int => |int| self.alloc_obj(obj.Object{ .int = obj.Integer{ .value = int.value } }),
                .bool => |boolean| if (boolean.value) obj.TRUE else obj.FALSE,
                .pref => |pref| blk: {
                    const right = try self.eval(Node{ .expression = pref.right });
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
                    const left = try self.eval(Node{ .expression = inf.left });
                    const right = try self.eval(Node{ .expression = inf.right });
                    break :blk switch (left.*) {
                        .bool => if (right.* == .bool) eval_infix_bool(inf.operator, left, right) else error.InvalidOperand,
                        .int => |l_int| if (right.* == .int) self.eval_infix_int(inf.operator, l_int, right.int) else error.InvalidOperand,
                        else => error.UnsupportedInfixOperation,
                    };
                },
                .if_exp => |if_exp| blk: {
                    const evaluated_condition = try self.eval(Node{ .expression = if_exp.condition });
                    if (is_truthy(evaluated_condition)) {
                        break :blk try self.eval(Node{ .statement = .{ .blk = if_exp.consequence } });
                    } else if (if_exp.alternative) |alternative| {
                        break :blk try self.eval(Node{ .statement = .{ .blk = alternative } });
                    } else {
                        break :blk obj.NULL;
                    }
                },
                else => error.Unimplemented,
            },
        };
    }

    fn eval_statements(self: *@This(), statements: []ast.Statement) anyerror!*const obj.Object {
        var result = obj.NULL;
        for (statements) |stmt| {
            result = try self.eval(Node{ .statement = stmt });
            if (stmt == .ret) self.is_returning = true;
            if (self.is_returning) return result;
        }
        return result;
    }

    fn eval_infix_int(self: @This(), op: ast.InfixOperator, left: obj.Integer, right: obj.Integer) !*const obj.Object {
        const value = switch (op) {
            .plus => left.value + right.value,
            .minus => left.value - right.value,
            .asterisk => left.value * right.value,
            .slash => if (right.value != 0) @divTrunc(left.value, right.value) else return error.DivisionByZero,
            .gt => return if (left.value > right.value) obj.TRUE else obj.FALSE,
            .lt => return if (left.value < right.value) obj.TRUE else obj.FALSE,
            .eq => return if (left.value == right.value) obj.TRUE else obj.FALSE,
            .not_eq => return if (left.value != right.value) obj.TRUE else obj.FALSE,
        };
        return self.alloc_obj(obj.Object{ .int = obj.Integer{ .value = value } });
    }

    fn alloc_obj(self: @This(), object: obj.Object) *const obj.Object {
        const ptr = self.allocator.create(obj.Object) catch unreachable;
        ptr.* = object;
        return ptr;
    }
};

fn eval_infix_bool(op: ast.InfixOperator, left: *const obj.Object, right: *const obj.Object) !*const obj.Object {
    assert(left == obj.TRUE or left == obj.FALSE);
    assert(right == obj.TRUE or right == obj.FALSE);
    return switch (op) {
        .eq => if (left == right) obj.TRUE else obj.FALSE,
        .not_eq => if (left != right) obj.TRUE else obj.FALSE,
        .plus,
        .minus,
        .asterisk,
        .slash,
        .lt,
        .gt,
        => error.UnsupportedOperator,
    };
}

fn is_truthy(object: *const obj.Object) bool {
    return switch (object.*) {
        .int => |int| int.value != 0,
        .bool => object == obj.TRUE,
        .null => false,
    };
}

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
        expected: union { int: i64, boolean: bool },
    }{
        .{ .input = "5", .expected = .{ .int = 5 } },
        .{ .input = "10", .expected = .{ .int = 10 } },
        .{ .input = "-5", .expected = .{ .int = -5 } },
        .{ .input = "-10", .expected = .{ .int = -10 } },
        .{ .input = "9 > 7", .expected = .{ .boolean = true } },
        .{ .input = "9 < 7", .expected = .{ .boolean = false } },
        .{ .input = "7 > 9", .expected = .{ .boolean = false } },
        .{ .input = "7 < 9", .expected = .{ .boolean = true } },
        .{ .input = "9 == 7", .expected = .{ .boolean = false } },
        .{ .input = "9 == 9", .expected = .{ .boolean = true } },
        .{ .input = "9 != 9", .expected = .{ .boolean = false } },
        .{ .input = "9 != 7", .expected = .{ .boolean = true } },
        .{ .input = "5 + 5 + 5 + 5 - 10", .expected = .{ .int = 10 } },
        .{ .input = "2 * 2 * 2 * 2 * 2", .expected = .{ .int = 32 } },
        .{ .input = "-50 + 100 + -50", .expected = .{ .int = 0 } },
        .{ .input = "5 * 2 + 10", .expected = .{ .int = 20 } },
        .{ .input = "5 + 2 * 10", .expected = .{ .int = 25 } },
        .{ .input = "20 + 2 * -10", .expected = .{ .int = 0 } },
        .{ .input = "50 / 2 * 2 + 10", .expected = .{ .int = 60 } },
        .{ .input = "2 * (5 + 10)", .expected = .{ .int = 30 } },
        .{ .input = "3 * 3 * 3 + 10", .expected = .{ .int = 37 } },
        .{ .input = "3 * (3 * 3) + 10", .expected = .{ .int = 37 } },
        .{ .input = "(5 + 10 * 2 + 15 / 3) * 2 + -10", .expected = .{ .int = 50 } },
    };
    for (cases) |case| {
        const result = try test_eval(case.input);
        switch (result) {
            .int => |actual| try std.testing.expectEqual(case.expected.int, actual.value),
            .bool => |actual| try std.testing.expectEqual(case.expected.boolean, actual.value),
            inline else => |actual| {
                std.log.err("\nExpected int or bool, got {}\n", .{actual});
                return error.UnexpectedObjectType;
            },
        }
    }
}

test "boolean" {
    const cases = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{ .input = "true", .expected = true },
        .{ .input = "false", .expected = false },
        .{ .input = "false == false", .expected = true },
        .{ .input = "true == false", .expected = false },
        .{ .input = "true != false", .expected = true },
        .{ .input = "false != true", .expected = true },
        .{ .input = "(1 < 2) == true", .expected = true },
        .{ .input = "(1 < 2) == false", .expected = false },
        .{ .input = "(1 > 2) == true", .expected = false },
        .{ .input = "(1 > 2) == false", .expected = true },
    };
    for (cases) |case| {
        const result = try test_eval(case.input);
        switch (result) {
            .bool => |actual| try std.testing.expectEqual(case.expected, actual.value),
            inline else => |actual| {
                std.log.err("\nExpected bool, got {}\n", .{actual});
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
                std.log.err("\nExpected bool, got {}\n", .{actual});
                return error.UnexpectedObjectType;
            },
        }
    }
}

test "if_else" {
    const cases = [_]struct {
        input: []const u8,
        expected: obj.Object,
    }{
        .{ .input = "if (true) { 10 }", .expected = .{ .int = .{ .value = 10 } } },
        .{ .input = "if (false) { 10 }", .expected = .{ .null = .{} } },
        .{ .input = "if (1) { 10 }", .expected = .{ .int = .{ .value = 10 } } },
        .{ .input = "if (1 < 2) { 10 }", .expected = .{ .int = .{ .value = 10 } } },
        .{ .input = "if (1 > 2) { 10 }", .expected = .{ .null = .{} } },
        .{ .input = "if (1 > 2) { 10 } else { 20 }", .expected = .{ .int = .{ .value = 20 } } },
        .{ .input = "if (1 < 2) { 10 } else { 20 }", .expected = .{ .int = .{ .value = 10 } } },
    };
    for (cases) |case| {
        const result = try test_eval(case.input);
        switch (result) {
            .int => |actual| try std.testing.expectEqual(case.expected.int.value, actual.value),
            .null => try std.testing.expect(case.expected == .null),
            inline else => |actual| {
                std.log.err("\nExpected int or null, got {}\n", .{actual});
                return error.UnexpectedObjectType;
            },
        }
    }
}

test "return" {
    const cases = [_]struct {
        input: []const u8,
        expected: obj.Object,
    }{
        .{ .input = "return 10;", .expected = .{ .int = .{ .value = 10 } } },
        .{ .input = "return 10; 9;", .expected = .{ .int = .{ .value = 10 } } },
        .{ .input = "return 2 * 5; 9", .expected = .{ .int = .{ .value = 10 } } },
        .{ .input = "9; return 2 * 5; 9;", .expected = .{ .int = .{ .value = 10 } } },
    };
    for (cases) |case| {
        const result = try test_eval(case.input);
        switch (result) {
            .int => |actual| try std.testing.expectEqual(case.expected.int.value, actual.value),
            inline else => |actual| {
                std.log.err("\nExpected int, got {}\n", .{actual});
                return error.UnexpectedObjectType;
            },
        }
    }
}
