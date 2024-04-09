const std = @import("std");
pub const Token = union(enum) {
    ident: []const u8,
    int: []const u8,

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
        const map = std.ComptimeStringMap(Token, .{
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
};
