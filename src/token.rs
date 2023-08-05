use std::borrow::Cow;
use std::str::FromStr;

#[derive(Debug, PartialEq)]
pub enum Literal<'src> {
    Identifier,
    String(Cow<'src, str>),
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

impl FromStr for Keyword {
    type Err = crate::error::SyntaxError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "and" => Ok(Keyword::And),
            "class" => Ok(Keyword::Class),
            "else" => Ok(Keyword::Else),
            "false" => Ok(Keyword::False),
            "fun" => Ok(Keyword::Fun),
            "for" => Ok(Keyword::For),
            "if" => Ok(Keyword::If),
            "nil" => Ok(Keyword::Nil),
            "or" => Ok(Keyword::Or),
            "print" => Ok(Keyword::Print),
            "return" => Ok(Keyword::Return),
            "super" => Ok(Keyword::Super),
            "this" => Ok(Keyword::This),
            "true" => Ok(Keyword::True),
            "var" => Ok(Keyword::Var),
            "while" => Ok(Keyword::While),
            _ => Err(crate::error::SyntaxError::InvalidKeywords),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenType<'src> {
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
    Literal(Literal<'src>),
    // Keywords.
    Keyword(Keyword),
}

#[derive(Debug)]
pub struct Token<'src> {
    pub(crate) ty: TokenType<'src>,
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
    pub fn new(ty: TokenType<'src>, lexeme: &'src str, span: Span) -> Token<'src> {
        Token { ty, lexeme, span }
    }
}
