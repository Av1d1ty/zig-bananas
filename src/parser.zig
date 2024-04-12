const std = @import("std");
const ast = @import("ast.zig");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;

const Parser = struct {
    lexer: *Lexer,
    errors: std.ArrayList(anyerror),

    curr_token: Token = Token.illegal,
    peek_token: Token = Token.illegal,

    pub fn init(lexer: *Lexer) @This() {
        var p = Parser{
            .lexer = lexer,
            .errors = std.ArrayList(anyerror).init(std.testing.allocator),
        };
        p.next_token();
        p.next_token();
        return p;
    }

    pub fn parse_program(self: *@This()) !ast.Program {
        var program = ast.Program{
            .statements = std.ArrayList(ast.Statement).init(std.testing.allocator),
        };
        while (self.curr_token != Token.eof) : (self.next_token()) {
            const statement = self.parse_statement() catch |e| {
                try self.errors.append(e);
                continue;
            };
            try program.statements.append(statement);
        }
        return program;
    }

    fn next_token(self: *@This()) void {
        self.curr_token = self.peek_token;
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(self: *@This()) !ast.Statement {
        return switch (self.curr_token) {
            .let => .{ .let = try self.parse_let_statement() },
            else => error.InvalidStatement,
        };
    }

    fn parse_let_statement(self: *@This()) !ast.LetStatement {
        const token = self.curr_token;
        switch (self.peek_token) {
            .ident => self.next_token(),
            else => return error.NextTokenNotIdent,
        }
        const ident = ast.Identifier{ .token = self.curr_token };
        while (self.curr_token != Token.semicolon) : (self.next_token()) {}
        return ast.LetStatement{ .token = token, .name = ident };
    }
};

const expect = std.testing.expect;
test "let_statements" {
    const input =
        \\ let x = 5;
        \\ let y = 10;
        \\ let foobar = 838383;
    ;
    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer);
    const program = try parser.parse_program();
    defer program.statements.deinit();
    defer parser.errors.deinit();

    // if (parser.errors.items.len > 0) {
    //     for (parser.errors.items) |val| {
    //         std.log.err("{s}\n", .{@errorName(val)});
    //     }
    //     unreachable;
    // }

    try expect(program.statements.items.len == 3);

    const expected_identifiers = [_][]const u8{ "x", "y", "foobar" };
    for (program.statements.items, expected_identifiers) |statement, ident| {
        switch (statement) {
            .let => |val| {
                try expect(std.mem.eql(u8, val.name.token.get_value().?, ident));
            },
        }
    }
}
