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
            .lparen => .call,
            else => .lowest,
        };
    }
};

const ParserError = error{
    OutOfMemory,
    Overflow,
    InvalidCharacter,

    UnexpectedToken,
    InvalidPrefixToken,
    InvalidInfixToken,
    InvalidFunctionToken,
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
    }

    pub fn print_errors(self: *@This()) void {
        std.log.err("\n\nParser encountered the following errors:", .{});
        for (self.errors.items) |err| {
            std.log.err("{}\n", .{err});
        }
    }

    pub fn parse_program(self: *@This()) !ast.Program {
        var statements = std.ArrayList(ast.Statement).init(self.allocator);
        while (self.curr_token != Token.eof) : (self.next_token()) {
            const statement = self.parse_statement() catch |err| {
                try self.errors.append(ErrorInfo{
                    .err = err,
                    .line = self.lexer.get_line(),
                    .token = self.lexer.snapshot_frame.curr_token.tok,
                    .row = self.lexer.snapshot_frame.curr_token.row,
                    .col = self.lexer.snapshot_frame.curr_token.col,
                });
                self.lexer.skip_line();
                continue;
            };
            try statements.append(statement);
        }
        if (self.errors.items.len > 0) {
            self.print_errors();
        }
        return ast.Program{
            .allocator = self.allocator,
            .statements = try statements.toOwnedSlice(),
            .expression_pointers = try self.expression_pointers.toOwnedSlice(),
        };
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

    fn parse_block_statement(self: *@This()) !ast.BlockStatement {
        self.next_token();
        const token = self.curr_token;
        var statements = std.ArrayList(ast.Statement).init(self.allocator);
        while (self.curr_token != .rbrace and self.curr_token != .eof) {
            try statements.append(try self.parse_statement());
            self.next_token();
        }
        const block = ast.BlockStatement{
            .token = token,
            .statements = try statements.toOwnedSlice(),
        };
        return block;
    }

    fn parse_expression(self: *@This(), precedence: Precedence) !*const ast.Expression {
        var left_exp = try self.parse_prefix_expr();
        // std.debug.print("\nPARSE EXPRESSION:\nCURR_TOKEN: {}\nPEEK_TOKEN: {}\nLEFT: {}\n", .{ self.curr_token, self.peek_token, left_exp });
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
                .value = token == .true_token,
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
                if (self.peek_token != .rparen) return error.UnexpectedToken;
                self.next_token();
                break :blk exp.*;
            },
            .if_token => blk: {
                if (self.peek_token != .lparen) return error.UnexpectedToken;
                self.next_token();
                const condition = try self.parse_expression(Precedence.lowest);
                if (self.curr_token != .rparen) return error.UnexpectedToken;
                self.next_token();
                if (self.curr_token != .lbrace) return error.UnexpectedToken;
                const consequence = try self.parse_block_statement();
                var alternative: ?ast.BlockStatement = null;
                if (self.peek_token == .else_token) {
                    self.next_token();
                    if (self.peek_token != .lbrace) return error.UnexpectedToken;
                    self.next_token();
                    alternative = try self.parse_block_statement();
                }
                const exp = .{ .if_exp = ast.If{
                    .token = .if_token,
                    .condition = condition,
                    .consequence = consequence,
                    .alternative = alternative,
                } };
                break :blk exp;
            },
            .function => blk: {
                if (self.peek_token != .lparen) return error.UnexpectedToken;
                self.next_token();
                self.next_token();
                var params = std.ArrayList(ast.Identifier).init(self.allocator);
                errdefer params.deinit();
                while (self.curr_token != .rparen) : (self.next_token()) {
                    if (self.curr_token != .ident) return error.UnexpectedToken;
                    try params.append(ast.Identifier{
                        .token = self.curr_token,
                        .value = self.curr_token.get_value().?,
                    });
                    switch (self.peek_token) {
                        .comma => self.next_token(),
                        .rparen => {},
                        else => return error.UnexpectedToken,
                    }
                }
                self.next_token();
                if (self.curr_token != .lbrace) return error.UnexpectedToken;
                const exp = .{ .func = ast.Function{
                    .token = .function,
                    .parameters = try params.toOwnedSlice(),
                    .body = try self.parse_block_statement(),
                } };
                break :blk exp;
            },
            else => return error.InvalidPrefixToken,
        };
        try self.expression_pointers.append(exp_ptr);
        return exp_ptr;
    }

    fn parse_infix_expr(self: *@This(), left: *const ast.Expression) ParserError!*const ast.Expression {
        const exp = switch (self.curr_token) {
            .plus, .minus, .slash, .asterisk, .eq, .not_eq, .lt, .gt => blk: {
                const token = self.curr_token;
                self.next_token();
                break :blk ast.Expression{
                    .inf = ast.Infix{
                        .token = token,
                        .left = left,
                        .right = try self.parse_expression(Precedence.of(token)),
                    },
                };
            },
            .lparen => ast.Expression{ .call = try self.parse_call_expression(left) },
            else => return left,
        };
        const exp_ptr = try self.allocator.create(ast.Expression);
        errdefer self.allocator.destroy(exp_ptr);
        exp_ptr.* = exp;
        try self.expression_pointers.append(exp_ptr);
        return exp_ptr;
    }

    fn parse_call_expression(self: *@This(), function: *const ast.Expression) ParserError!ast.Call {
        return ast.Call{
            .token = self.curr_token,
            .func = switch (function.*) {
                .func => .{ .func = function.func },
                .ident => .{ .ident = function.ident },
                else => return error.InvalidFunctionToken,
            },
            .arguments = try self.parse_call_arguments(),
        };
    }

    fn parse_call_arguments(self: *@This()) ParserError![]ast.Expression {
        var args = std.ArrayList(ast.Expression).init(self.allocator);
        errdefer args.deinit();
        self.next_token();
        if (self.curr_token == .rparen) return args.toOwnedSlice();
        try args.append((try self.parse_expression(.lowest)).*);
        while (self.peek_token == .comma) {
            self.next_token();
            self.next_token();
            try args.append((try self.parse_expression(.lowest)).*);
        }
        self.next_token();
        if (self.curr_token != .rparen) return error.UnexpectedToken;
        return args.toOwnedSlice();
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

    try expect(program.statements.len == cases.len);
    try expect(parser.errors.items.len == 0);

    // std.debug.print("\n{}", .{program});

    for (program.statements, cases) |statement, expected| {
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

    try expect(program.statements.len == cases.len);
    try expect(parser.errors.items.len == 0);

    for (program.statements, cases) |statement, expected| {
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

    try expect(program.statements.len == cases.len);
    try expect(parser.errors.items.len == 0);

    // std.debug.print("\n{}", .{program});

    for (program.statements, cases) |statement, expected| {
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

    try expect(program.statements.len == cases.len);
    try expect(parser.errors.items.len == 0);

    // std.debug.print("\n{}", .{program});

    for (program.statements, cases) |statement, expected| {
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

    try expect(program.statements.len == cases.len);
    try expect(parser.errors.items.len == 0);

    // std.debug.print("\n{}", .{program});

    for (program.statements, cases) |statement, expected| {
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

    try expect(program.statements.len == cases.len);
    try expect(parser.errors.items.len == 0);

    for (program.statements, cases) |statement, expected| {
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

    try expect(program.statements.len == cases.len);
    try expect(parser.errors.items.len == 0);

    // std.debug.print("\n{}", .{program});

    for (program.statements, cases) |statement, expected| {
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

test "if" {
    const input =
        \\ if (x < y) { x }
        \\ if (len < 1) { return x } else { return 0 }
    ;
    const cases = [_]struct {
        condition: *const ast.Expression,
        consequence: []const u8,
        alternative: []const u8,
    }{
        .{
            .condition = &(ast.Expression{ .inf = .{
                .token = .lt,
                .left = &(ast.Expression{ .ident = .{ .token = .{ .ident = "x" }, .value = "x" } }),
                .right = &(ast.Expression{ .ident = .{ .token = .{ .ident = "y" }, .value = "y" } }),
            } }),
            .consequence = "x;",
            .alternative = "null",
        },
        .{
            .condition = &(ast.Expression{ .inf = .{
                .token = .lt,
                .left = &(ast.Expression{ .ident = .{ .token = .{ .ident = "len" }, .value = "len" } }),
                .right = &(ast.Expression{ .int = .{ .token = .{ .int = "1" }, .value = 1 } }),
            } }),
            .consequence = "return x;",
            .alternative = "return 0;",
        },
    };

    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer, std.testing.allocator);
    var program = try parser.parse_program();

    defer parser.deinit();
    defer program.deinit();

    try expect(program.statements.len == cases.len);
    try expect(parser.errors.items.len == 0);

    // std.debug.print("\n{}", .{program});

    for (program.statements, cases) |statement, expected| {
        switch (statement.exp.expression.*) {
            .if_exp => |if_exp| {
                try std.testing.expectFmt(expected.consequence, "{}", .{if_exp.consequence});
                try std.testing.expectFmt(expected.alternative, "{?}", .{if_exp.alternative});
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
        \\1 + (2 + 3) + 4;
        \\(5 + 5) * 2
        \\2 / (5 + 5);
        \\-(5 + 5)
        \\!(true == true)
        \\if (a != b) { return true }
        \\if (true) { x } else { a + b }
        \\fn (x, y) { return x + y };
        \\fn (x) { return x }
        \\fn () { return true }
        \\add(1, 2 * 3, 4 + 5);
    ;
    const expected =
        \\((-a) * b)
        \\(!(-a))
        \\((a + b) + c)
        \\((a + b) - c)
        \\((a * b) * c)
        \\((a * b) / c)
        \\(a + (b / c))
        \\(((a + (b * c)) + (d / e)) - f)
        \\(3 + 4)
        \\((-5) * 5)
        \\((5 > 4) == (3 < 4))
        \\((5 < 4) != (3 > 4))
        \\((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))
        \\(foobar == true)
        \\(baz != false)
        \\((1 + (2 + 3)) + 4)
        \\((5 + 5) * 2)
        \\(2 / (5 + 5))
        \\(-(5 + 5))
        \\(!(true == true))
        \\if (a != b) { return true; }
        \\if true { x; } else { (a + b); }
        \\fn (x, y) { return (x + y); }
        \\fn (x) { return x; }
        \\fn () { return true; }
        \\add(1, (2 * 3), (4 + 5))
        \\
    ;

    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer, std.testing.allocator);
    var program = try parser.parse_program();

    defer parser.deinit();
    defer program.deinit();

    // std.debug.print("{}", .{program});

    try expect(parser.errors.items.len == 0);
    try std.testing.expectFmt(expected, "{}", .{program});
}
