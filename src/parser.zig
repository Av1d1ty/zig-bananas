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

    UnexpectedAstNode,

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
        return writer.print(
            "{}: row {d}, col {d}, token '{}'\n\ton line '{s}'.\n",
            .{ self.err, self.row, self.col, self.token, self.line },
        );
    }
};

pub const Parser = struct {
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
        if (self.errors.items.len == 0) return;
        std.log.err("Parser encountered the following errors:", .{});
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
            else => return ParserError.UnexpectedToken,
        }
        const ident = ast.Identifier{
            .token = self.curr_token,
            .value = self.curr_token.get_value().?,
        };
        self.next_token();
        if (self.curr_token != .assign) {
            return ParserError.UnexpectedToken;
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
                self.next_token();
                break :blk ast.Expression{ .pref = ast.Prefix{
                    .operator = if (token == .minus) .minus else .bang,
                    .right = try self.parse_expression(Precedence.prefix),
                } };
            },
            .lparen => blk: {
                self.next_token();
                const exp = try self.parse_expression(Precedence.lowest);
                if (self.peek_token != .rparen) return ParserError.UnexpectedToken;
                self.next_token();
                break :blk exp.*;
            },
            .if_token => blk: {
                if (self.peek_token != .lparen) return ParserError.UnexpectedToken;
                self.next_token();
                const condition = try self.parse_expression(Precedence.lowest);
                if (self.curr_token != .rparen) return ParserError.UnexpectedToken;
                self.next_token();
                if (self.curr_token != .lbrace) return ParserError.UnexpectedToken;
                const consequence = try self.parse_block_statement();
                var alternative: ?ast.BlockStatement = null;
                if (self.peek_token == .else_token) {
                    self.next_token();
                    if (self.peek_token != .lbrace) return ParserError.UnexpectedToken;
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
                if (self.peek_token != .lparen) return ParserError.UnexpectedToken;
                self.next_token();
                self.next_token();
                var params = std.ArrayList(ast.Identifier).init(self.allocator);
                errdefer params.deinit();
                while (self.curr_token != .rparen) : (self.next_token()) {
                    if (self.curr_token != .ident) return ParserError.UnexpectedToken;
                    try params.append(ast.Identifier{
                        .token = self.curr_token,
                        .value = self.curr_token.get_value().?,
                    });
                    switch (self.peek_token) {
                        .comma => self.next_token(),
                        .rparen => {},
                        else => return ParserError.UnexpectedToken,
                    }
                }
                self.next_token();
                if (self.curr_token != .lbrace) return ParserError.UnexpectedToken;
                const exp = .{ .func = ast.Function{
                    .token = .function,
                    .parameters = try params.toOwnedSlice(),
                    .body = try self.parse_block_statement(),
                } };
                break :blk exp;
            },
            else => return ParserError.InvalidPrefixToken,
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
                        .operator = ast.infix_operator_from_token(token) catch unreachable,
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
                else => return ParserError.InvalidFunctionToken,
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
        if (self.curr_token != .rparen) return ParserError.UnexpectedToken;
        return args.toOwnedSlice();
    }
};

// TODO: add failing cases

const expect = std.testing.expect;
test "booleans" {
    const input =
        \\ true
        \\ false
    ;
    const cases = [_]ast.Boolean{
        ast.Boolean{ .token = .true_token, .value = true },
        ast.Boolean{ .token = .false_token, .value = false },
    };

    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer, std.testing.allocator);
    var program = try parser.parse_program();

    defer parser.deinit();
    defer program.deinit();

    parser.print_errors();
    try expect(program.statements.len == cases.len);
    try expect(parser.errors.items.len == 0);

    // std.debug.print("\n{}", .{program});

    for (program.statements, cases) |actual, expected| {
        if (actual == .exp and actual.exp.expression.* == .bool) {
            try std.testing.expectEqualDeep(expected, actual.exp.expression.bool);
        } else {
            return ParserError.UnexpectedAstNode;
        }
    }
}

test "integers" {
    const input =
        \\ 5
        \\ 42
    ;

    const cases = [_]ast.Integer{
        ast.Integer{ .token = .{ .int = "5" }, .value = 5 },
        ast.Integer{ .token = .{ .int = "42" }, .value = 42 },
    };

    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer, std.testing.allocator);
    var program = try parser.parse_program();

    defer parser.deinit();
    defer program.deinit();

    parser.print_errors();
    try expect(program.statements.len == cases.len);
    try expect(parser.errors.items.len == 0);

    // std.debug.print("\n{}", .{program});

    for (program.statements, cases) |actual, expected| {
        if (actual == .exp and actual.exp.expression.* == .int) {
            try std.testing.expectEqualDeep(expected, actual.exp.expression.int);
        } else {
            return ParserError.UnexpectedAstNode;
        }
    }
}

test "identifiers" {
    const input =
        \\ foobar
        \\ baz
    ;

    const cases = [_]ast.Identifier{
        ast.Identifier{ .token = .{ .ident = "foobar" }, .value = "foobar" },
        ast.Identifier{ .token = .{ .ident = "baz" }, .value = "baz" },
    };

    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer, std.testing.allocator);
    var program = try parser.parse_program();

    defer parser.deinit();
    defer program.deinit();

    parser.print_errors();
    try expect(program.statements.len == cases.len);
    try expect(parser.errors.items.len == 0);

    // std.debug.print("\n{}", .{program});

    for (program.statements, cases) |actual, expected| {
        if (actual == .exp and actual.exp.expression.* == .ident) {
            try std.testing.expectEqualDeep(expected, actual.exp.expression.ident);
        } else {
            return ParserError.UnexpectedAstNode;
        }
    }
}

test "let_statements" {
    const input =
        \\ let x = 5
        \\ let y = 1 + b
        \\ let foobar = x / y
    ;

    const cases = [_]ast.LetStatement{
        ast.LetStatement{
            .token = .let,
            .name = ast.Identifier{
                .token = .{ .ident = "x" },
                .value = "x",
            },
            .value = &(ast.Expression{ .int = ast.Integer{ .value = 5, .token = .{ .int = "5" } } }),
        },
        ast.LetStatement{
            .token = .let,
            .name = ast.Identifier{
                .token = .{ .ident = "y" },
                .value = "y",
            },
            .value = &(ast.Expression{ .inf = .{
                .operator  = .plus,
                .left = &(ast.Expression{ .int = ast.Integer{ .value = 1, .token = .{ .int = "1" } } }),
                .right = &(ast.Expression{ .ident = ast.Identifier{ .token = .{ .ident = "b" }, .value = "b" } }),
            } }),
        },
        ast.LetStatement{
            .token = .let,
            .name = ast.Identifier{
                .token = .{ .ident = "foobar" },
                .value = "foobar",
            },
            .value = &(ast.Expression{ .inf = .{
                .operator  = .slash,
                .left = &(ast.Expression{ .ident = ast.Identifier{ .token = .{ .ident = "x" }, .value = "x" } }),
                .right = &(ast.Expression{ .ident = ast.Identifier{ .token = .{ .ident = "y" }, .value = "y" } }),
            } }),
        },
    };

    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer, std.testing.allocator);
    var program = try parser.parse_program();

    defer parser.deinit();
    defer program.deinit();

    parser.print_errors();
    try expect(program.statements.len == cases.len);
    try expect(parser.errors.items.len == 0);

    // std.debug.print("\n{}", .{program});

    for (program.statements, cases) |actual, expected| {
        if (actual == .let) {
            try std.testing.expectEqualDeep(expected, actual.let);
        } else {
            return ParserError.UnexpectedAstNode;
        }
    }
}

test "return_statements" {
    const input =
        \\ return 5;
        \\ return 1 + b;
        \\ return;
    ;

    const cases = [_]ast.ReturnStatement{
        ast.ReturnStatement{
            .token = .return_token,
            .value = &(ast.Expression{ .int = ast.Integer{ .value = 5, .token = .{ .int = "5" } } }),
        },
        ast.ReturnStatement{
            .token = .return_token,
            .value = &(ast.Expression{ .inf = .{
                .operator = .plus,
                .left = &(ast.Expression{ .int = ast.Integer{ .value = 1, .token = .{ .int = "1" } } }),
                .right = &(ast.Expression{ .ident = ast.Identifier{ .token = .{ .ident = "b" }, .value = "b" } }),
            } }),
        },
        ast.ReturnStatement{
            .token = .return_token,
            .value = null,
        },
    };

    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer, std.testing.allocator);
    var program = try parser.parse_program();

    defer parser.deinit();
    defer program.deinit();

    // std.debug.print("\n{}", .{program});

    parser.print_errors();
    try expect(program.statements.len == cases.len);
    try expect(parser.errors.items.len == 0);

    for (program.statements, cases) |actual, expected| {
        if (actual == .ret) {
            try std.testing.expectEqualDeep(expected, actual.ret);
        } else {
            return ParserError.UnexpectedAstNode;
        }
    }
}

test "prefix" {
    const input =
        \\ -5
        \\ !true
    ;

    const cases = [_]ast.Prefix{
        ast.Prefix{
            .operator = .minus,
            .right = &(ast.Expression{ .int = ast.Integer{ .token = .{ .int = "5" }, .value = 5 } }),
        },
        ast.Prefix{
            .operator = .bang,
            .right = &(ast.Expression{ .bool = ast.Boolean{ .token = .true_token, .value = true } }),
        },
    };

    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer, std.testing.allocator);
    var program = try parser.parse_program();

    defer parser.deinit();
    defer program.deinit();

    // std.debug.print("\n{}", .{program});

    parser.print_errors();
    try expect(program.statements.len == cases.len);
    try expect(parser.errors.items.len == 0);

    for (program.statements, cases) |actual, expected| {
        if (actual == .exp and actual.exp.expression.* == .pref) {
            try std.testing.expectEqualDeep(expected, actual.exp.expression.pref);
        } else {
            return ParserError.UnexpectedAstNode;
        }
    }
}

test "infix" {
    const input =
        \\ 5 + 5
        \\ 5 - 5
        \\ 5 * 5
        \\ 5 / 5
        \\ 5 > 5
        \\ 5 < 5
        \\ 5 == 5
        \\ 5 != 5
    ;

    var cases: [8]ast.Infix = undefined;
    const tokens = [_]Token{ .plus, .minus, .asterisk, .slash, .gt, .lt, .eq, .not_eq };
    inline for (0..8, tokens) |i, token| {
        cases[i] = ast.Infix{
            .operator = try ast.infix_operator_from_token(token),
            .left = &(ast.Expression{ .int = .{ .token = .{ .int = "5" }, .value = 5 } }),
            .right = &(ast.Expression{ .int = .{ .token = .{ .int = "5" }, .value = 5 } }),
        };
    }

    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer, std.testing.allocator);
    var program = try parser.parse_program();

    defer parser.deinit();
    defer program.deinit();

    parser.print_errors();
    try expect(program.statements.len == cases.len);
    try expect(parser.errors.items.len == 0);

    // std.debug.print("\n{}", .{program});

    for (program.statements, cases) |actual, expected| {
        if (actual == .exp and actual.exp.expression.* == .inf) {
            try std.testing.expectEqualDeep(expected, actual.exp.expression.inf);
        } else {
            return ParserError.UnexpectedAstNode;
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
                .operator = .lt,
                .left = &(ast.Expression{ .ident = .{ .token = .{ .ident = "x" }, .value = "x" } }),
                .right = &(ast.Expression{ .ident = .{ .token = .{ .ident = "y" }, .value = "y" } }),
            } }),
            .consequence = "x;",
            .alternative = "null",
        },
        .{
            .condition = &(ast.Expression{ .inf = .{
                .operator = .lt,
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

    parser.print_errors();
    try expect(program.statements.len == cases.len);
    try expect(parser.errors.items.len == 0);

    // std.debug.print("\n{}", .{program});

    for (program.statements, cases) |statement, expected| {
        switch (statement.exp.expression.*) {
            .if_exp => |if_exp| {
                try std.testing.expectFmt(expected.consequence, "{}", .{if_exp.consequence});
                try std.testing.expectFmt(expected.alternative, "{?}", .{if_exp.alternative});
            },
            else => return ParserError.UnexpectedToken,
        }
    }
}

test "precedence" {
    const input =
        \\-a * b;
        \\!-a;
        \\a + b + c;
        \\a + b - c;
        \\a * b * c;
        \\a * b / c;
        \\a + b / c;
        \\a + b * c + d / e - f;
        \\3 + 4; -5 * 5;
        \\5 > 4 == 3 < 4;
        \\5 < 4 != 3 > 4;
        \\3 + 4 * 5 == 3 * 1 + 4 * 5;
        \\foobar == true;
        \\baz != false;
        \\1 + (2 + 3) + 4;
        \\(5 + 5) * 2;
        \\2 / (5 + 5);
        \\-(5 + 5);
        \\!(true == true);
        \\if (a != b) { return true; };
        \\if (true) { x } else { a + b; };
        \\fn (x, y) { return x + y; };
        \\fn (x) { return x };
        \\fn () { return true; };
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

    parser.print_errors();
    try expect(parser.errors.items.len == 0);
    try std.testing.expectFmt(expected, "{}", .{program});
}
