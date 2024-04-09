const std = @import("std");
pub const Token = union(enum) {
    ident: []const u8,
    int: []const u8,

    illegal,
    eof,

    // Operators
    assign,
    plus,

    // Delimiters
    comma,
    semicolon,

    lparen,
    rparen,
    lbrace,
    rbrace,

    // Keywords
    function,
    let,

    pub fn lookup_ident(ident: []const u8) Token {
        const map = std.ComptimeStringMap(Token, .{
            .{ "let", .let },
            .{ "fn", .function },
        });
        return map.get(ident) orelse .{ .ident = ident };
    }
};
