const std = @import("std");
const Token = @import("token.zig").Token;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Node = @import("ast.zig").Node;
const Evaluator = @import("eval.zig").Evaluator;
const Environment = @import("object.zig").Environment;

const prompt = "↯ ";

pub fn start() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var env = Environment.init(alloc, null);
    defer env.deinit();

    while (true) {
        _ = try stdout.write(prompt);
        const maybe_input = try stdin.readUntilDelimiterOrEofAlloc(alloc, '\n', 100);
        if (maybe_input) |input| {
            // defer alloc.free(input);

            var lexer = Lexer.init(input);

            var parser = Parser.init(&lexer, alloc);
            // defer parser.deinit();

            var program = try parser.parse_program();
            // defer program.deinit();

            var evaluator = Evaluator{ .allocator = alloc };
            const eval_result = evaluator.eval(Node{ .program = &program }, &env);
            try stdout.print("{!}\n", .{eval_result});
        }
    }
}
