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

    pub fn eval(self: *@This(), node: Node, env: *obj.Environment) !*const obj.Object {
        return switch (node) {
            .program => |prog| self.eval_statements(prog.statements, env),
            .statement => |stmt| switch (stmt) {
                .blk => |blk| try self.eval_statements(blk.statements, env),
                .exp => |exp| try self.eval(Node{ .expression = exp.expression }, env),
                .ret => |ret| if (ret.value) |val| try self.eval(Node{ .expression = val }, env) else obj.NULL,
                .let => |let| blk: {
                    const val = try self.eval(Node{ .expression = let.value }, env);
                    const name = self.allocator.dupe(u8, let.name.value) catch unreachable;
                    env.put(name, val);
                    break :blk obj.NULL;
                },
            },
            .expression => |expr| switch (expr.*) {
                .int => |int| self.alloc_obj(obj.Object{ .int = obj.Integer{ .value = int.value } }),
                .bool => |boolean| if (boolean.value) obj.TRUE else obj.FALSE,
                .pref => |pref| blk: {
                    const right = try self.eval(Node{ .expression = pref.right }, env);
                    break :blk switch (pref.operator) {
                        .bang => switch (right.*) {
                            .bool => if (right == obj.FALSE) obj.TRUE else obj.FALSE,
                            .null => obj.TRUE,
                            .int => |int| if (int.value == 0) obj.TRUE else obj.FALSE,
                            .func => error.InvalidOperand,
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
                    const left = try self.eval(Node{ .expression = inf.left }, env);
                    const right = try self.eval(Node{ .expression = inf.right }, env);
                    break :blk switch (left.*) {
                        .bool => if (right.* == .bool) eval_infix_bool(inf.operator, left, right) else error.InvalidOperand,
                        .int => |l_int| if (right.* == .int) self.eval_infix_int(inf.operator, l_int, right.int) else error.InvalidOperand,
                        else => error.UnsupportedInfixOperation,
                    };
                },
                .if_exp => |if_exp| blk: {
                    const evaluated_condition = try self.eval(Node{ .expression = if_exp.condition }, env);
                    if (is_truthy(evaluated_condition)) {
                        break :blk try self.eval(Node{ .statement = .{ .blk = if_exp.consequence } }, env);
                    } else if (if_exp.alternative) |alternative| {
                        break :blk try self.eval(Node{ .statement = .{ .blk = alternative } }, env);
                    } else {
                        break :blk obj.NULL;
                    }
                },
                .ident => |ident| env.get(ident.value) orelse error.UnboundIdentifier,
                .func => |func| self.alloc_obj(obj.Object{ .func = obj.Function{
                    .parameters = func.parameters,
                    .body = func.body,
                    .env = env,
                } }),
                .call => |call| blk: {
                    const func_obj = try switch (call.func) {
                        .ident => |ident| env.get(ident.value) orelse error.UnboundIdentifier,
                        .func => |func| self.alloc_obj(obj.Object{ .func = obj.Function{
                            .parameters = func.parameters,
                            .body = func.body,
                            .env = env,
                        } }),
                    };
                    // Parser ensures that this AST object resolves only to functions
                    const func = func_obj.func;
                    if (func.parameters.len != call.arguments.len) return error.ArgumentCountMismatch;
                    var args_array = std.ArrayList(*const obj.Object).init(self.allocator);
                    for (call.arguments) |arg| {
                        const arg_val = try self.eval(Node{ .expression = &arg }, env);
                        try args_array.append(arg_val);
                    }
                    const args = try args_array.toOwnedSlice();
                    defer self.allocator.free(args);
                    break :blk self.eval_function(func, args);
                },
            },
        };
    }

    fn eval_function(self: *@This(), func: obj.Function, args: []*const obj.Object) !*const obj.Object {
        var inner_env = obj.Environment.init(self.allocator, func.env);
        const inner_env_ptr = try self.allocator.create(obj.Environment);
        for (func.parameters, args) |param, arg| {
            inner_env.put(param.value, arg);
        }
        inner_env_ptr.* = inner_env;
        return self.eval_statements(func.body.statements, inner_env_ptr);
    }

    fn eval_statements(self: *@This(), statements: []ast.Statement, env: *obj.Environment) anyerror!*const obj.Object {
        var result = obj.NULL;
        for (statements) |stmt| {
            result = try self.eval(Node{ .statement = stmt }, env);
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
        .func => true,
    };
}

fn test_eval(input: []const u8, allocator: std.mem.Allocator) !*const obj.Object {
    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer, allocator);
    var program = try parser.parse_program();
    var evaluator = Evaluator{ .allocator = allocator };
    var env = obj.Environment.init(allocator, null);

    return try evaluator.eval(Node{ .program = &program }, &env);
}

test "integer" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
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
        const result = try test_eval(case.input, arena.allocator());
        switch (result.*) {
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
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
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
        const result = try test_eval(case.input, arena.allocator());
        switch (result.*) {
            .bool => |actual| try std.testing.expectEqual(case.expected, actual.value),
            inline else => |actual| {
                std.log.err("\nExpected bool, got {}\n", .{actual});
                return error.UnexpectedObjectType;
            },
        }
    }
}

test "bang" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
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
        const result = try test_eval(case.input, arena.allocator());
        switch (result.*) {
            .bool => |actual| try std.testing.expectEqual(case.expected, actual.value),
            inline else => |actual| {
                std.log.err("\nExpected bool, got {}\n", .{actual});
                return error.UnexpectedObjectType;
            },
        }
    }
}

test "if_else" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
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
        const result = try test_eval(case.input, arena.allocator());
        switch (result.*) {
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
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const cases = [_]struct {
        input: []const u8,
        expected: obj.Object,
    }{
        .{ .input = "return 10;", .expected = .{ .int = .{ .value = 10 } } },
        .{ .input = "return 10; 9;", .expected = .{ .int = .{ .value = 10 } } },
        .{ .input = "return 2 * 5; 9", .expected = .{ .int = .{ .value = 10 } } },
        .{ .input = "9; return 2 * 5; 9;", .expected = .{ .int = .{ .value = 10 } } },
        .{ .input = "if (10 > 1) { if (10 > 1) { return 10; } return 1; }", .expected = .{ .int = .{ .value = 10 } } },
    };
    for (cases) |case| {
        const result = try test_eval(case.input, arena.allocator());
        switch (result.*) {
            .int => |actual| try std.testing.expectEqual(case.expected.int.value, actual.value),
            inline else => |actual| {
                std.log.err("\nExpected int, got {}\n", .{actual});
                return error.UnexpectedObjectType;
            },
        }
    }
}

test "let" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const cases = [_]struct {
        input: []const u8,
        expected: obj.Object,
    }{
        .{ .input = "let a = 5; a;", .expected = .{ .int = .{ .value = 5 } } },
        .{ .input = "let a = 5 * 5; a;", .expected = .{ .int = .{ .value = 25 } } },
        .{ .input = "let a = 5; let b = a; b;", .expected = .{ .int = .{ .value = 5 } } },
        .{ .input = "let a = 5; let b = a; let c = a + b + 5; c;", .expected = .{ .int = .{ .value = 15 } } },
    };
    for (cases) |case| {
        const result = try test_eval(case.input, arena.allocator());
        switch (result.*) {
            .int => |actual| try std.testing.expectEqual(case.expected.int.value, actual.value),
            inline else => |actual| {
                std.log.err("\nExpected int, got {}\n", .{actual});
                return error.UnexpectedObjectType;
            },
        }
    }
}

test "func" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const cases = [_]struct {
        input: []const u8,
        expected: []const u8,
    }{
        .{ .input = "fn(x) { x + 2; }", .expected = "fn(x) { (x + 2); }" },
    };
    for (cases) |case| {
        const result = try test_eval(case.input, arena.allocator());
        switch (result.*) {
            .func => |actual| try std.testing.expectFmt(case.expected, "{}", .{actual}),
            inline else => |actual| {
                std.log.err("\nExpected func, got {}\n", .{actual});
                return error.UnexpectedObjectType;
            },
        }
    }
}

test "func_eval" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const cases = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "let identity = fn(x) { x; }; identity(5);", .expected = 5 },
        .{ .input = "let identity = fn(x) { return x; }; identity(5);", .expected = 5 },
        .{ .input = "let double = fn(x) { x * 2; }; double(5);", .expected = 10 },
        .{ .input = "let add = fn(x, y) { x + y; }; add(5, 5);", .expected = 10 },
        .{ .input = "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", .expected = 20 },
        .{ .input = "fn(x) { x; }(5)", .expected = 5 },
    };
    for (cases) |case| {
        const result = try test_eval(case.input, arena.allocator());
        switch (result.*) {
            .int => |actual| try std.testing.expectEqual(case.expected, actual.value),
            inline else => |actual| {
                std.log.err("\nExpected int, got {}\n", .{actual});
                return error.UnexpectedObjectType;
            },
        }
    }
}

test "func_closure" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const input =
        \\let newAdder = fn(x) {
        \\    fn(y) { x + y };
        \\};
        \\let addTwo = newAdder(2);
        \\addTwo(2);
    ;
    const expected = 4;
    const result = try test_eval(input, arena.allocator());
    switch (result.*) {
        .int => |actual| try std.testing.expectEqual(expected, actual.value),
        inline else => |actual| {
            std.log.err("\nExpected int, got {}\n", .{actual});
            return error.UnexpectedObjectType;
        },
    }
}
