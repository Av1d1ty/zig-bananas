const std = @import("std");

pub const Token = union(enum) {
    ident: []const u8,
    int: []const u8,
    string: []const u8,

    illegal,
    eof,

    // Operators
    assign,
    plus,
    minus,
    bang,
    asterisk,
    slash,

    lt,
    gt,
    eq,
    not_eq,

    // Delimiters
    comma,
    semicolon,

    lparen,
    rparen,
    lbrace,
    rbrace,

    // Keywords
    let,
    function,
    if_token,
    else_token,
    true_token,
    false_token,
    return_token,

    pub fn lookup_ident(ident: []const u8) Token {
        const map = std.StaticStringMap(Token).initComptime(.{
            .{ "let", .let },
            .{ "fn", .function },
            .{ "if", .if_token },
            .{ "else", .else_token },
            .{ "true", .true_token },
            .{ "false", .false_token },
            .{ "return", .return_token },
        });
        return map.get(ident) orelse .{ .ident = ident };
    }

    pub fn get_value(self: Token) ?[]const u8 {
        return switch (self) {
            Token.ident, Token.int, Token.string => |val| val,
            else => null,
        };
    }

    pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        if (fmt.len != 0) std.fmt.invalidFmtError(fmt, self);
        const string = switch (self) {
            .int => |int| int,
            .ident => |ident| ident,
            .string => |string| string,
            .assign => "=",
            .plus => "+",
            .minus => "-",
            .bang => "!",
            .asterisk => "*",
            .slash => "/",
            .lt => "<",
            .gt => ">",
            .eq => "==",
            .not_eq => "!=",
            .comma => ",",
            .semicolon => ";",
            .lparen => "(",
            .rparen => ")",
            .lbrace => "{",
            .rbrace => "}",
            .return_token => "return",
            .let => "let",
            .function => "fn",
            .if_token => "if",
            .else_token => "else",
            .true_token => "true",
            .false_token => "false",
            .eof => "eof",
            .illegal => "illegal",
        };
        return writer.print("{s}", .{string});
    }
};
