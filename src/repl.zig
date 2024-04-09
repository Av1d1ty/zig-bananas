const std = @import("std");
const Token = @import("token.zig").Token;
const Lexer = @import("lexer.zig").Lexer;

const prompt = "λ ";

pub fn start() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var input: [100]u8 = undefined;

    while (true) {
        _ = try stdout.write(prompt);
        _ = try stdin.readUntilDelimiterOrEof(&input, '\n');

        var l = Lexer.init(&input);
        var token = l.next_token();
        while (token != Token.eof and token != Token.illegal) : (token = l.next_token()) {
            // stdout.print("Type: {}, Literal: {}", .{@tagName(token));
            try stdout.print("{s}\n", .{@tagName(token)});
        }
    }
}
