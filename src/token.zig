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
};
