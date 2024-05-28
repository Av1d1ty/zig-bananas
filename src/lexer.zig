const std = @import("std");
const Token = @import("token.zig").Token;

const LexerSnapshot = struct {
    pos: u32,
    row: u32,
    col: u16,
    tok: Token,
};

pub const Lexer = struct {
    input: []const u8,
    position: u32 = 0,
    read_position: u32 = 0,
    row: u32 = 1,
    // FIX: there is probably an off by one error somewhere with columns
    col: u16 = 0,
    ch: u8 = 0,
    snapshot: LexerSnapshot = .{ .pos = 0, .row = 1, .col = 0, .tok = .illegal },

    pub fn init(input: []const u8) Lexer {
        var l = Lexer{ .input = input };
        l.read_char();
        return l;
    }

    fn read_char(self: *Lexer) void {
        self.ch = if (self.read_position >= self.input.len) 0 else self.input[self.read_position];
        self.position = self.read_position;
        self.read_position += 1;
        self.col += 1;
    }

    fn peek_char(self: *Lexer) u8 {
        return if (self.read_position >= self.input.len) 0 else self.input[self.read_position];
    }

    pub fn take_snapshot(self: *Lexer) void {
        self.snapshot = .{ .pos = self.position, .row = self.row, .col = self.col, .tok = .illegal };
    }

    pub fn next_token(self: *Lexer) Token {
        self.skip_whitespace();
        const token: Token = switch (self.ch) {
            '=' => blk: {
                if (self.peek_char() == '=') {
                    self.read_char();
                    break :blk .eq;
                } else {
                    break :blk .assign;
                }
            },
            ';' => .semicolon,
            '(' => .lparen,
            ')' => .rparen,
            ',' => .comma,
            '+' => .plus,
            '-' => .minus,
            '*' => .asterisk,
            '/' => .slash,
            '{' => .lbrace,
            '}' => .rbrace,
            '!' => blk: {
                if (self.peek_char() == '=') {
                    self.read_char();
                    break :blk .not_eq;
                } else {
                    break :blk .bang;
                }
            },
            '<' => .lt,
            '>' => .gt,
            0 => .eof,
            'A'...'Z', 'a'...'z', '_' => return Token.lookup_ident(self.read_identifier()),
            '0'...'9' => return .{ .int = self.read_int() },
            else => .illegal,
        };
        self.read_char();
        return token;
    }

    fn skip_whitespace(self: *Lexer) void {
        while (std.ascii.isWhitespace(self.ch)) {
            if (self.ch == '\n') {
                self.row += 1;
                self.col = 0;
            }
            self.read_char();
        }
    }

    fn read_identifier(self: *Lexer) []const u8 {
        const position = self.position;
        while (is_letter(self.ch)) {
            self.read_char();
        }
        return self.input[position..self.position];
    }

    fn read_int(self: *Lexer) []const u8 {
        const position = self.position;
        while (is_digit(self.ch)) {
            self.read_char();
        }
        return self.input[position..self.position];
    }

    fn is_letter(ch: u8) bool {
        return std.ascii.isAlphabetic(ch) or ch == '_';
    }

    fn is_digit(ch: u8) bool {
        return std.ascii.isDigit(ch);
    }

    pub fn get_line(self: *Lexer) []const u8 {
        const start_idx = self.snapshot.pos - self.snapshot.col + 1;
        var end_idx: usize = 0;
        for (self.input[self.snapshot.pos..], 0..) |char, offset| {
            if (char == '\n') {
                end_idx = self.snapshot.pos + offset;
                break;
            }
        } else return self.input[start_idx..];
        return self.input[start_idx..end_idx];
    }

    pub fn skip_line(self: *Lexer) void {
        while (self.ch != '\n') {
            self.read_char();
        }
        self.skip_whitespace();
    }
};

const expectEqualDeep = @import("std").testing.expectEqualDeep;
test "next_token" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\  x + y;
        \\};
        \\
        \\let result = add(five, ten);
        \\
        \\!-/*5;
        \\5 < 10 > 5;
        \\
        \\if (5 < 10) {
        \\  return true;
        \\} else {
        \\  return false;
        \\}
        \\
        \\10 == 10;
        \\10 != 9;
    ;
    const tests = [_]Token{
        .let,              .{ .ident = "five" }, .assign,             .{ .int = "5" },
        .semicolon,        .let,                 .{ .ident = "ten" }, .assign,
        .{ .int = "10" },  .semicolon,           .let,                .{ .ident = "add" },
        .assign,           .function,            .lparen,             .{ .ident = "x" },
        .comma,            .{ .ident = "y" },    .rparen,             .lbrace,
        .{ .ident = "x" }, .plus,                .{ .ident = "y" },   .semicolon,
        .rbrace,           .semicolon,           .let,                .{ .ident = "result" },
        .assign,           .{ .ident = "add" },  .lparen,             .{ .ident = "five" },
        .comma,            .{ .ident = "ten" },  .rparen,             .semicolon,
        .bang,             .minus,               .slash,              .asterisk,
        .{ .int = "5" },   .semicolon,           .{ .int = "5" },     .lt,
        .{ .int = "10" },  .gt,                  .{ .int = "5" },     .semicolon,
        .if_token,         .lparen,              .{ .int = "5" },     .lt,
        .{ .int = "10" },  .rparen,              .lbrace,             .return_token,
        .true_token,       .semicolon,           .rbrace,             .else_token,
        .lbrace,           .return_token,        .false_token,        .semicolon,
        .rbrace,           .{ .int = "10" },     .eq,                 .{ .int = "10" },
        .semicolon,        .{ .int = "10" },     .not_eq,             .{ .int = "9" },
        .semicolon,        .eof,
    };

    var l = Lexer.init(input);
    for (tests) |expected| {
        try expectEqualDeep(expected, l.next_token());
    }
}
