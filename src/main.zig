const repl = @import("repl.zig");

pub fn main() !void {
    try repl.start();
}
