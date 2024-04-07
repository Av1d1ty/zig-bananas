const token = @import("token");

const Lexer = struct {
    input: []const u8,
    position: u32 = 0,
    read_position: u32 = 0,
    ch: u8 = 0,

    pub fn init(input: []const u8) Lexer {
        var l = Lexer{ .input = input };
        var tok = token.Token;
        tok.s = "test";
        l.read_char();
        return l;
    }

    pub fn read_char(self: *Lexer) void {
        self.ch = if (self.read_position >= self.input.len) 0 else self.input[self.read_position];
        self.position = self.read_position;
        self.read_position += 1;
    }
};

test "init" {
    var l = Lexer.init("xy");
    l.read_char();
}
