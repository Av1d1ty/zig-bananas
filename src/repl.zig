const std = @import("std");
const Token = @import("token.zig").Token;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Node = @import("ast.zig").Node;
const Evaluator = @import("eval.zig").Evaluator;

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
            var lexer = Lexer.init(input);

            var parser = Parser.init(&lexer, alloc);
            defer parser.deinit();

            var program = try parser.parse_program();
            defer program.deinit();

            var evaluator = Evaluator{ .allocator = alloc };
            const eval_result = evaluator.eval(Node{ .program = &program });
            try stdout.print("{}\n", .{eval_result});
        }
    }
}
