#[derive(Debug, PartialEq)]
pub enum Literal {
    Identifier,
    String(std::string::String),
    Number(f64),
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    /// Opening parentheses `(`
    LeftParen,
    /// Closing parentheses `)`
    RightParen,
    /// Opening curly brace `{`
    LeftBrace,
    /// Closing curly brace `}`
    RightBrace,
    /// Comma `,`
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Comment
    Comment,
    EOF,
    // Literals.
    Literal(Literal),
    // Keywords.
    Keyword(Keyword),
}

#[derive(Debug)]
pub struct Token<'src> {
    pub(crate) ty: TokenType,
    pub(crate) lexeme: &'src str,
    pub(crate) span: Span,
}

/// The span of a token, line number indexed from 1,
/// column number indexed from 0, right exclusive.
#[derive(Debug, Copy, Clone)]
pub struct Span {
    pub(crate) line: usize,
    pub(crate) start: usize,
    pub(crate) end: usize,
}

impl<'src> Token<'src> {
    pub fn new(ty: TokenType, lexeme: &'src str, span: Span) -> Token<'src> {
        Token { ty, lexeme, span }
    }
}
