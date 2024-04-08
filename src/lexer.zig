const std = @import("std");
const Token = @import("token.zig").Token;

const Lexer = struct {
    input: []const u8,
    position: u32 = 0,
    read_position: u32 = 0,
    ch: u8 = 0,

    pub fn init(input: []const u8) Lexer {
        var l = Lexer{ .input = input };
        l.read_char();
        return l;
    }

    pub fn read_char(self: *Lexer) void {
        self.ch = if (self.read_position >= self.input.len) 0 else self.input[self.read_position];
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(self: *Lexer) Token {
        const token: Token = switch (self.ch) {
            '=' => .assign,
            ';' => .semicolon,
            '(' => .lparen,
            ')' => .rparen,
            ',' => .comma,
            '+' => .plus,
            '{' => .lbrace,
            '}' => .rbrace,
            0 => Token.eof,
            else => Token.illegal,
        };
        self.read_char();
        return token;
    }
};

const expectEqualDeep = @import("std").testing.expectEqualDeep;
test "next_token" {
    const input =
        \\=+(){},;
    ;
    const tests = [_]Token{
        .assign,
        .plus,
        .lparen,
        .rparen,
        .lbrace,
        .rbrace,
        .comma,
        .eof,
    };

    var l = Lexer.init(input);
    for (tests) |expected| {
        try expectEqualDeep(l.next_token(), expected);
    }
}
