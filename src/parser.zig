const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const ast = @import("ast.zig");

const Parser = struct {
    lexer: *Lexer,

    curr_token: Token = Token.illegal,
    peek_token: Token = Token.illegal,

    pub fn init(lexer: *Lexer) @This() {
        var p = Parser{ .lexer = lexer };

        p.next_token();
        p.next_token();

        return p;
    }

    pub fn parse_program(self: *@This()) !*ast.Program {
        var program = ast.Program{ .statements = std.ArrayList(ast.Statement)
            .init(std.testing.allocator) };

        while (self.curr_token != Token.eof) {
            const statement = try self.parse_statement() orelse continue;
            // FIX: problem here
            try program.statements.append(statement);
            std.debug.print("\ntoken3 {any}\n", .{program.statements.getLast().let.name});
            self.next_token();
        }
        return &program;
    }

    fn next_token(self: *@This()) void {
        self.curr_token = self.peek_token;
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(self: *@This()) !?ast.Statement {
        return switch (self.curr_token) {
            .let => .{ .let = try self.parse_let_statement() },
            else => null,
        };
    }

    fn parse_let_statement(self: *@This()) !*ast.LetStatement {
        const token = self.curr_token;
        if (@intFromEnum(self.peek_token) == @intFromEnum(Token.ident)) {
            self.next_token();
        } else {
            return error.NextTokenNotIdent;
        }
        const ident = ast.Identifier{
            .token = self.curr_token,
            // .value = self.curr_token.get_value(),
        };
        while (@intFromEnum(self.curr_token) != @intFromEnum(Token.semicolon)) : (self.next_token()) {}
        var let_statement = ast.LetStatement{ .token = token, .name = ident };
        return &let_statement;
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

    try expect(program.statements.items.len == 3);

    const expected_identifiers = [_][]const u8{ "x", "y", "foobar" };
    for (program.statements.items, expected_identifiers) |statement, ident| {
        try test_let_statement(statement, ident);
    }
}
fn test_let_statement(statement: ast.Statement, ident: []const u8) !void {
    switch (statement) {
        .let => |val| {
            std.debug.print("\ntoken {any}\n", .{val.name});
            if (std.mem.eql(u8, val.name.token.get_value() orelse unreachable, ident)) {
                return error.WrongIdentifier;
            }
        },
        // else => return error.IsNotLet,
    }
}
