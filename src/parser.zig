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

    UnexpectedToken,
};

const ErrorInfo = struct {
    err: ParserError,
    line: []const u8,
    row: u32,
    col: u16,
    token: Token,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        if (fmt.len != 0) std.fmt.invalidFmtError(fmt, self);
        // TODO: prettify
        return writer.print(
            "{}: row {d}, col {d}, token '{}'\n\ton line '{s}'.\n",
            .{ self.err, self.row, self.col, self.token, self.line },
        );
    }
};

const Parser = struct {
    lexer: *Lexer,
    allocator: std.mem.Allocator,
    errors: std.ArrayList(ErrorInfo),

    curr_token: Token = Token.illegal,
    peek_token: Token = Token.illegal,

    expression_pointers: std.ArrayList(*const ast.Expression),

    pub fn init(lexer: *Lexer, allocator: std.mem.Allocator) @This() {
        var p = Parser{
            .lexer = lexer,
            .allocator = allocator,
            .errors = std.ArrayList(ErrorInfo).init(allocator),
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
        std.log.err("\n\nParser encountered the following errors:", .{});
        for (self.errors.items) |err| {
            std.log.err("{}\n", .{err});
        }
    }

    pub fn parse_program(self: *@This()) !*ast.Program {
        var program = try ast.Program.init(self.allocator);
        while (self.curr_token != Token.eof) : (self.next_token()) {
            const statement = self.parse_statement() catch |err| {
                try self.errors.append(ErrorInfo{
                    .err = err,
                    .line = self.lexer.get_line(),
                    .token = self.lexer.snapshot.tok,
                    .row = self.lexer.snapshot.row,
                    .col = self.lexer.snapshot.col,
                });
                self.lexer.skip_line();
                continue;
            };
            try program.statements.append(statement);
        }
        program.expression_pointers = try self.expression_pointers.clone();
        if (self.errors.items.len > 0) {
            self.print_errors();
        }
        return program;
    }

    fn next_token(self: *@This()) void {
        self.lexer.take_snapshot();
        self.curr_token = self.peek_token;
        self.peek_token = self.lexer.next_token();
        // FIX: booo!
        self.lexer.snapshot.tok = self.peek_token;
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
            .lbrace,
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
            .rparen,
            .rbrace,
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
        errdefer self.allocator.destroy(exp_ptr);
        exp_ptr.* = switch (token) {
            .ident => ast.Expression{ .ident = ast.Identifier{
                .token = self.curr_token,
                .value = self.curr_token.get_value().?,
            } },
            .int => ast.Expression{ .int = ast.Integer{
                .token = self.curr_token,
                .value = try std.fmt.parseInt(i64, self.curr_token.get_value().?, 10),
            } },
            .true_token, .false_token => ast.Expression{ .bool = ast.Boolean{
                .token = self.curr_token,
                .value = if (token == .true_token) true else false,
            } },
            .minus, .bang => blk: {
                const tok = self.curr_token;
                self.next_token();
                break :blk ast.Expression{ .pref = ast.Prefix{
                    .token = tok,
                    .right = try self.parse_expression(Precedence.prefix),
                } };
            },
            .lparen => blk: {
                self.next_token();
                const exp = try self.parse_expression(Precedence.lowest);
                if (self.peek_token != .rparen) {
                    return error.UnexpectedToken;
                }
                self.next_token();
                break :blk exp.*;
            },
            else => return error.UnexpectedToken,
        };
        try self.expression_pointers.append(exp_ptr);
        return exp_ptr;
    }

    fn parse_infix_expr(self: *@This(), left: *const ast.Expression) ParserError!*const ast.Expression {
        const token = self.curr_token;
        self.next_token();
        const exp_ptr = try self.allocator.create(ast.Expression);
        errdefer self.allocator.destroy(exp_ptr);
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
    const cases = [_]struct {
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

    try expect(program.statements.items.len == cases.len);
    try expect(parser.errors.items.len == 0);

    // std.debug.print("\n{}", .{program});

    for (program.statements.items, cases) |statement, expected| {
        switch (statement) {
            .let => |val| {
                try std.testing.expectEqualStrings(expected.ident, val.name.value);
                try std.testing.expectEqualStrings(expected.ident, val.name.token.get_value().?);
                try std.testing.expectFmt(expected.value, "{}", .{val.value});
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
    const cases = [_][]const u8{ "5", "(1 + b)", "null" };

    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer, std.testing.allocator);
    var program = try parser.parse_program();

    defer parser.deinit();
    defer program.deinit();

    // std.debug.print("\n{}", .{program});

    try expect(program.statements.items.len == cases.len);
    try expect(parser.errors.items.len == 0);

    for (program.statements.items, cases) |statement, expected| {
        switch (statement) {
            .ret => |val| {
                try std.testing.expectFmt(expected, "{?}", .{val.value});
            },
            else => return error.UnexpectedToken,
        }
    }
}

test "identifiers" {
    const input =
        \\ foobar;
        \\ baz;
    ;
    const cases = [_][]const u8{ "foobar", "baz" };

    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer, std.testing.allocator);
    var program = try parser.parse_program();

    defer parser.deinit();
    defer program.deinit();

    try expect(program.statements.items.len == cases.len);
    try expect(parser.errors.items.len == 0);

    // std.debug.print("\n{}", .{program});

    for (program.statements.items, cases) |statement, expected| {
        switch (statement) {
            .exp => |exp| {
                switch (exp.expression.*) {
                    .ident => |ident| {
                        try expect(ident.token == .ident);
                        try std.testing.expectEqualStrings(expected, ident.value);
                    },
                    else => return error.UnexpectedToken,
                }
            },
            else => return error.UnexpectedToken,
        }
    }
}

test "integers" {
    const input =
        \\ 5;
        \\ 42;
    ;
    const cases = [_]u8{ 5, 42 };

    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer, std.testing.allocator);
    var program = try parser.parse_program();

    defer parser.deinit();
    defer program.deinit();

    try expect(program.statements.items.len == cases.len);
    try expect(parser.errors.items.len == 0);

    // std.debug.print("\n{}", .{program});

    for (program.statements.items, cases) |statement, expected| {
        switch (statement) {
            .exp => |exp| {
                switch (exp.expression.*) {
                    .int => |int| {
                        try expect(int.token == .int);
                        try expect(expected == int.value);
                    },
                    else => return error.UnexpectedToken,
                }
            },
            else => return error.UnexpectedToken,
        }
    }
}

test "booleans" {
    const input =
        \\ true;
        \\ false;
    ;
    const cases = [_]bool{ true, false };

    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer, std.testing.allocator);
    var program = try parser.parse_program();

    defer parser.deinit();
    defer program.deinit();

    try expect(program.statements.items.len == cases.len);
    try expect(parser.errors.items.len == 0);

    // std.debug.print("\n{}", .{program});

    for (program.statements.items, cases) |statement, expected| {
        switch (statement) {
            .exp => |exp| {
                switch (exp.expression.*) {
                    .bool => |boolean| {
                        try expect(boolean.token == .true_token or boolean.token == .false_token);
                        try expect(expected == boolean.value);
                    },
                    else => return error.UnexpectedToken,
                }
            },
            else => return error.UnexpectedToken,
        }
    }
}
test "prefix" {
    const input =
        \\ !5;
        \\ -15;
    ;
    const cases = [_]struct { token: Token, integer: i64 }{
        .{ .token = .bang, .integer = 5 },
        .{ .token = .minus, .integer = 15 },
    };

    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer, std.testing.allocator);
    var program = try parser.parse_program();

    defer parser.deinit();
    defer program.deinit();

    // std.debug.print("\n{}", .{program});

    try expect(program.statements.items.len == cases.len);
    try expect(parser.errors.items.len == 0);

    for (program.statements.items, cases) |statement, expected| {
        switch (statement.exp.expression.*) {
            .pref => |pref| {
                switch (pref.token) {
                    .minus => try expect(expected.token == .minus),
                    .bang => try expect(expected.token == .bang),
                    else => return error.UnexpectedToken,
                }
                try expect(pref.right.* == .int);
                try expect(pref.right.int.value == expected.integer);
            },
            else => return error.UnexpectedToken,
        }
    }
}

test "infix" {
    const input =
        \\ 5 + 5;
        \\ 5 - 5;
        \\ 5 * 5;
        \\ 5 / 5;
        \\ 5 > 5;
        \\ 5 < 5;
        \\ 5 == 5;
        \\ 5 != 5;
    ;
    const cases = [_]struct {
        left: i64,
        right: i64,
        operator: Token,
    }{
        .{ .left = 5, .right = 5, .operator = .plus },
        .{ .left = 5, .right = 5, .operator = .minus },
        .{ .left = 5, .right = 5, .operator = .asterisk },
        .{ .left = 5, .right = 5, .operator = .slash },
        .{ .left = 5, .right = 5, .operator = .gt },
        .{ .left = 5, .right = 5, .operator = .lt },
        .{ .left = 5, .right = 5, .operator = .eq },
        .{ .left = 5, .right = 5, .operator = .not_eq },
    };

    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer, std.testing.allocator);
    var program = try parser.parse_program();

    defer parser.deinit();
    defer program.deinit();

    try expect(program.statements.items.len == cases.len);
    try expect(parser.errors.items.len == 0);

    // std.debug.print("\n{}", .{program});

    for (program.statements.items, cases) |statement, expected| {
        switch (statement.exp.expression.*) {
            .inf => |inf| {
                if (inf.left.* != .int or inf.right.* != .int) {
                    return error.UnexpectedToken;
                }
                try expect(inf.left.int.value == expected.left);
                try expect(inf.right.int.value == expected.right);
                try expect(@intFromEnum(inf.token) == @intFromEnum(expected.operator));
            },
            else => return error.UnexpectedToken,
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
        \\foobar == true
        \\baz != false
        \\1 + (2 + 3) + 4
        \\(5 + 5) * 2
        \\2 / (5 + 5);
        \\)-(5 + 5)
        \\!(true == true)
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
        \\(foobar == true);
        \\(baz != false);
        \\((1 + (2 + 3)) + 4);
        \\((5 + 5) * 2);
        \\(2 / (5 + 5));
        \\(-(5 + 5));
        \\(!(true == true));
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
