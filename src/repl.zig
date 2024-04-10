const std = @import("std");
const Token = @import("token.zig").Token;
const Lexer = @import("lexer.zig").Lexer;

const prompt = "λ ";

pub fn start() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    while (true) {
        _ = try stdout.write(prompt);
        const maybe_input = try stdin.readUntilDelimiterOrEofAlloc(alloc, '\n', 100);
        if (maybe_input) |input| {
            defer alloc.free(input);
            var l = Lexer.init(input);
            var token = l.next_token();
            while (token != Token.eof) : (token = l.next_token()) {
                try stdout.print("{s} {s}\n", .{ @tagName(token), token.get_value() orelse "" });
            }
        }
    }
}
