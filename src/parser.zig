const std = @import("std");
const ast = @import("ast.zig");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;

const Precedence = enum {
    lowest,
    equals,
    less_greater,
    sum,
    product,
    prefix,
    call,

    pub fn of(token: Token) @This() {
        return switch (token) {
            .eq, .not_eq => .equals,
            .lt, .gt => .less_greater,
            .plus, .minus => .sum,
            .slash, .asterisk => .product,
            else => .lowest,
        };
    }
};

const ParserError = error{
    OutOfMemory,
    Overflow,
    InvalidCharacter,
};

const Parser = struct {
    lexer: *Lexer,
    allocator: std.mem.Allocator,
    errors: std.ArrayList(anyerror),

    curr_token: Token = Token.illegal,
    peek_token: Token = Token.illegal,

    expression_pointers: std.ArrayList(*const ast.Expression),

    pub fn init(lexer: *Lexer, allocator: std.mem.Allocator) @This() {
        var p = Parser{
            .lexer = lexer,
            .allocator = allocator,
            .errors = std.ArrayList(anyerror).init(allocator),
            .expression_pointers = std.ArrayList(*const ast.Expression).init(allocator),
        };
        p.next_token();
        p.next_token();
        return p;
    }

    pub fn deinit(self: *@This()) void {
        self.errors.deinit();
        self.expression_pointers.deinit();
    }

    pub fn print_errors(self: *@This()) void {
        for (self.errors.items) |val| {
            std.log.err("{s}\n", .{@errorName(val)});
        }
    }

    pub fn parse_program(self: *@This()) !*ast.Program {
        var program = try ast.Program.init(self.allocator);
        while (self.curr_token != Token.eof) : (self.next_token()) {
            const statement = self.parse_statement() catch |e| {
                try self.errors.append(e);
                continue;
            };
            try program.statements.append(statement);
        }
        program.expression_pointers = try self.expression_pointers.clone();
        return program;
    }

    fn next_token(self: *@This()) void {
        self.curr_token = self.peek_token;
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(self: *@This()) !ast.Statement {
        return switch (self.curr_token) {
            .let => .{ .let = try self.parse_let_statement() },
            .return_token => .{ .ret = try self.parse_return_statement() },
            .ident,
            .int,
            .minus,
            .bang,
            .lparen,
            .rparen,
            .lbrace,
            .rbrace,
            .function,
            .if_token,
            .true_token,
            .false_token,
            => .{ .exp = try self.parse_expression_statement() },
            .illegal,
            .eof,
            .assign,
            .plus,
            .asterisk,
            .slash,
            .lt,
            .gt,
            .eq,
            .not_eq,
            .comma,
            .semicolon,
            .else_token,
            => error.UnexpectedToken,
        };
    }

    fn parse_let_statement(self: *@This()) !ast.LetStatement {
        const token = self.curr_token;
        switch (self.peek_token) {
            .ident => self.next_token(),
            else => return error.UnexpectedToken,
        }
        const ident = ast.Identifier{
            .token = self.curr_token,
            .value = self.curr_token.get_value().?,
        };
        self.next_token();
        if (self.curr_token != .assign) {
            return error.UnexpectedToken;
        }
        self.next_token();
        const exp = try self.parse_expression(Precedence.lowest);
        if (self.peek_token == .semicolon) {
            self.next_token();
        }
        return ast.LetStatement{ .token = token, .name = ident, .value = exp };
    }

    fn parse_return_statement(self: *@This()) !ast.ReturnStatement {
        const token = self.curr_token;
        if (self.peek_token == .semicolon) {
            self.next_token();
            return ast.ReturnStatement{ .token = token, .value = null };
        }
        self.next_token();
        const exp = try self.parse_expression(Precedence.lowest);
        if (self.peek_token == .semicolon) {
            self.next_token();
        }
        return ast.ReturnStatement{ .token = token, .value = exp };
    }

    fn parse_expression_statement(self: *@This()) !ast.ExpressionStatement {
        // const token = self.curr_token;
        const expression = try self.parse_expression(Precedence.lowest);
        if (self.peek_token == Token.semicolon) self.next_token();
        return ast.ExpressionStatement{ .expression = expression };
    }

    fn parse_expression(self: *@This(), precedence: Precedence) !*const ast.Expression {
        var left_exp = try self.parse_prefix_expr();
        // NOTE: no need to check for `;`, because it has the lowest precendece
        // and the loop will break anyway
        while (@intFromEnum(precedence) < @intFromEnum(Precedence.of(self.peek_token))) {
            self.next_token();
            left_exp = try self.parse_infix_expr(left_exp);
        }
        return left_exp;
    }

    fn parse_prefix_expr(self: *@This()) ParserError!*const ast.Expression {
        const token = self.curr_token;
        const exp_ptr = try self.allocator.create(ast.Expression);
        exp_ptr.* = switch (token) {
            .ident => ast.Expression{ .ident = ast.Identifier{
                .token = self.curr_token,
                .value = self.curr_token.get_value().?,
            } },
            .int => ast.Expression{ .int = ast.Integer{
                .token = self.curr_token,
                .value = try std.fmt.parseInt(i64, self.curr_token.get_value().?, 10),
            } },
            .minus, .bang => blk: {
                const tok = self.curr_token;
                self.next_token();
                break :blk ast.Expression{ .pref = ast.Prefix{
                    .token = tok,
                    .right = try self.parse_expression(Precedence.prefix),
                } };
            },
            else => unreachable,
        };
        try self.expression_pointers.append(exp_ptr);
        return exp_ptr;
    }

    fn parse_infix_expr(self: *@This(), left: *const ast.Expression) ParserError!*const ast.Expression {
        const token = self.curr_token;
        self.next_token();
        const exp_ptr = try self.allocator.create(ast.Expression);
        exp_ptr.* = ast.Expression{
            .inf = ast.Infix{
                .token = token,
                .left = left,
                .right = try self.parse_expression(Precedence.of(token)),
            },
        };
        try self.expression_pointers.append(exp_ptr);
        return exp_ptr;
    }
};

const expect = std.testing.expect;
test "let_statements" {
    const input =
        \\ let x = 5;
        \\ let y = 1 + b;
        \\ let foobar = x / y;
    ;
    const expected = [_]struct {
        ident: []const u8,
        value: []const u8,
    }{
        .{ .ident = "x", .value = "5" },
        .{ .ident = "y", .value = "(1 + b)" },
        .{ .ident = "foobar", .value = "(x / y)" },
    };

    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer, std.testing.allocator);
    var program = try parser.parse_program();

    defer parser.deinit();
    defer program.deinit();

    try expect(program.statements.items.len == 3);
    try expect(parser.errors.items.len == 0);

    // std.debug.print("\n{}", .{program});

    for (program.statements.items, expected) |statement, case| {
        switch (statement) {
            .let => |val| {
                try std.testing.expectEqualStrings(case.ident, val.name.value);
                try std.testing.expectEqualStrings(case.ident, val.name.token.get_value().?);
                try std.testing.expectFmt(case.value, "{}", .{val.value});
            },
            else => return error.UnexpectedToken,
        }
    }
}

test "return_statements" {
    const input =
        \\ return 5;
        \\ return 1 + b;
        \\ return;
    ;
    const expected = [_][]const u8{ "5", "(1 + b)", "null" };

    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer, std.testing.allocator);
    var program = try parser.parse_program();

    defer parser.deinit();
    defer program.deinit();

    // std.debug.print("\n{}", .{program});

    try expect(program.statements.items.len == 3);
    try expect(parser.errors.items.len == 0);

    for (program.statements.items, expected) |statement, case| {
        switch (statement) {
            .ret => |val| {
                try std.testing.expectFmt(case, "{?}", .{val.value});
            },
            else => return error.UnexpectedToken,
        }
    }
}

// TODO: tests almost nothing, needs further work
// test "expressions" {
//     const input =
//         \\ foobar;
//         \\ 5;
//     ;
//
//     var lexer = Lexer.init(input);
//     var parser = Parser.init(&lexer, std.testing.allocator);
//     var program = try parser.parse_program();
//
//     defer parser.deinit();
//     defer program.deinit();
//
//     try expect(program.statements.items.len == 2);
//     try expect(parser.errors.items.len == 0);
//
//     std.debug.print("\n{}", .{program});
//
//     const expected_identifiers = [_][]const u8{ "foobar", "5" };
//     // NOTE: integer literal values are not validated for now
//     for (program.statements.items, expected_identifiers) |statement, ident| {
//         switch (statement) {
//             .exp => |val| {
//                 try expect(std.mem.eql(u8, val.token.get_value().?, ident));
//             },
//             else => unreachable,
//         }
//     }
// }

test "prefix" {
    const PrefixTestCase = struct {
        input: []const u8,
        operator: u8,
        integer: i64,
    };
    const test_cases = [_]PrefixTestCase{
        .{ .input = "!5;", .operator = '!', .integer = 5 },
        .{ .input = "-15;", .operator = '-', .integer = 15 },
    };

    for (test_cases) |case| {
        var lexer = Lexer.init(case.input);
        var parser = Parser.init(&lexer, std.testing.allocator);
        var program = try parser.parse_program();

        defer parser.deinit();
        defer program.deinit();

        // std.debug.print("\n{}", .{program});

        try expect(program.statements.items.len == 1);
        try expect(parser.errors.items.len == 0);

        for (program.statements.items) |statement| {
            switch (statement.exp.expression.*) {
                .pref => |pref| {
                    try expect(pref.right.int.value == case.integer);
                    switch (pref.token) {
                        .bang => try expect(case.operator == '!'),
                        .minus => try expect(case.operator == '-'),
                        else => unreachable,
                    }
                },
                else => unreachable,
            }
        }
    }
}

test "infix" {
    const InfixTestCase = struct {
        input: []const u8,
        left_value: i64,
        operator: Token,
        right_value: i64,
    };
    const test_cases = [_]InfixTestCase{
        .{ .input = "5 + 5;", .left_value = 5, .operator = .plus, .right_value = 5 },
        .{ .input = "5 - 5;", .left_value = 5, .operator = .minus, .right_value = 5 },
        .{ .input = "5 * 5;", .left_value = 5, .operator = .asterisk, .right_value = 5 },
        .{ .input = "5 / 5;", .left_value = 5, .operator = .slash, .right_value = 5 },
        .{ .input = "5 > 5;", .left_value = 5, .operator = .gt, .right_value = 5 },
        .{ .input = "5 < 5;", .left_value = 5, .operator = .lt, .right_value = 5 },
        .{ .input = "5 == 5;", .left_value = 5, .operator = .eq, .right_value = 5 },
        .{ .input = "5 != 5;", .left_value = 5, .operator = .not_eq, .right_value = 5 },
    };

    for (test_cases) |case| {
        var lexer = Lexer.init(case.input);
        var parser = Parser.init(&lexer, std.testing.allocator);
        var program = try parser.parse_program();

        defer parser.deinit();
        defer program.deinit();

        try expect(program.statements.items.len == 1);
        try expect(parser.errors.items.len == 0);

        for (program.statements.items) |statement| {
            switch (statement.exp.expression.*) {
                .inf => |inf| {
                    try expect(inf.left.int.value == case.left_value);
                    try expect(inf.right.int.value == case.right_value);
                    try expect(@intFromEnum(inf.token) == @intFromEnum(case.operator));
                },
                else => unreachable,
            }
        }
    }
}

test "precedence" {
    const input =
        \\-a * b
        \\!-a
        \\a + b + c
        \\a + b - c
        \\a * b * c
        \\a * b / c
        \\a + b / c
        \\a + b * c + d / e - f
        \\3 + 4; -5 * 5
        \\5 > 4 == 3 < 4
        \\5 < 4 != 3 > 4
        \\3 + 4 * 5 == 3 * 1 + 4 * 5
    ;
    const expected =
        \\((-a) * b);
        \\(!(-a));
        \\((a + b) + c);
        \\((a + b) - c);
        \\((a * b) * c);
        \\((a * b) / c);
        \\(a + (b / c));
        \\(((a + (b * c)) + (d / e)) - f);
        \\(3 + 4);
        \\((-5) * 5);
        \\((5 > 4) == (3 < 4));
        \\((5 < 4) != (3 > 4));
        \\((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));
        \\
    ;

    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer, std.testing.allocator);
    var program = try parser.parse_program();

    defer parser.deinit();
    defer program.deinit();

    try expect(parser.errors.items.len == 0);
    try std.testing.expectFmt(expected, "{}", .{program});
}
