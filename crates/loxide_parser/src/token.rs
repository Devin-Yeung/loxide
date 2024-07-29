use crate::ast;
use crate::ast::{BinaryOperator, UnaryOperator};
use crate::error::SyntaxError;
use miette::SourceSpan;
use std::borrow::Cow;
use std::str::FromStr;

#[derive(Debug, PartialEq, Clone)]
pub enum Literal<'src> {
    String(Cow<'src, str>),
    Number(f64),
}

#[derive(Debug, PartialEq, Copy, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
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
    Identifier,
}

impl<'src> TokenType<'src> {
    pub fn name(&self) -> &'static str {
        match self {
            TokenType::LeftParen => "(",
            TokenType::RightParen => ")",
            TokenType::LeftBrace => "{",
            TokenType::RightBrace => "}",
            TokenType::Comma => ",",
            TokenType::Dot => ".",
            TokenType::Minus => "-",
            TokenType::Plus => "+",
            TokenType::Semicolon => ";",
            TokenType::Slash => "/",
            TokenType::Star => "*",
            TokenType::Bang => "!",
            TokenType::BangEqual => "!=",
            TokenType::Equal => "=",
            TokenType::EqualEqual => "==",
            TokenType::Greater => ">",
            TokenType::GreaterEqual => ">=",
            TokenType::Less => "<",
            TokenType::LessEqual => "<=",
            TokenType::Comment => "comment",
            TokenType::EOF => "eof",
            TokenType::Identifier => "identifier",
            TokenType::Literal(_) => "literal",
            TokenType::Keyword(_) => "keyword",
        }
    }
}

#[derive(Debug)]
pub struct Token<'src> {
    pub(crate) ty: TokenType<'src>,
    pub(crate) lexeme: &'src str,
    pub(crate) span: Span,
}

/// The span of a token, indexed from 0, left inclusive, right exclusive.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl From<Span> for (usize, usize) {
    fn from(val: Span) -> Self {
        (val.start, val.end)
    }
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
}

impl From<Span> for SourceSpan {
    fn from(val: Span) -> Self {
        SourceSpan::from(val.start..val.end)
    }
}

impl<'src> Token<'src> {
    pub fn new(ty: TokenType<'src>, lexeme: &'src str, span: Span) -> Token<'src> {
        Token { ty, lexeme, span }
    }
}

impl<'src> TryInto<BinaryOperator> for Token<'src> {
    type Error = SyntaxError;

    fn try_into(self) -> Result<BinaryOperator, Self::Error> {
        let op = match self.ty {
            TokenType::EqualEqual => BinaryOperator::EqualEqual,
            TokenType::BangEqual => BinaryOperator::BangEq,
            TokenType::GreaterEqual => BinaryOperator::GreaterEqual,
            TokenType::Greater => BinaryOperator::Greater,
            TokenType::Less => BinaryOperator::Less,
            TokenType::LessEqual => BinaryOperator::LessEqual,
            TokenType::Minus => BinaryOperator::Minus,
            TokenType::Plus => BinaryOperator::Plus,
            TokenType::Slash => BinaryOperator::Slash,
            TokenType::Star => BinaryOperator::Star,
            _ => return Err(SyntaxError::InvalidConversion),
        };
        Ok(op)
    }
}

impl<'src> TryInto<UnaryOperator> for Token<'src> {
    type Error = SyntaxError;

    fn try_into(self) -> Result<UnaryOperator, Self::Error> {
        let op = match self.ty {
            TokenType::Minus => UnaryOperator::Minus,
            TokenType::Bang => UnaryOperator::Bang,
            _ => return Err(SyntaxError::InvalidConversion),
        };
        Ok(op)
    }
}

impl<'src> TryInto<ast::Literal> for Token<'src> {
    type Error = SyntaxError;

    fn try_into(self) -> Result<ast::Literal, Self::Error> {
        let literal = match self.ty {
            TokenType::Literal(Literal::String(raw)) => ast::Literal::String(raw.to_string()),
            TokenType::Literal(Literal::Number(raw)) => ast::Literal::Number(raw),
            TokenType::Keyword(Keyword::Nil) => ast::Literal::Nil,
            TokenType::Keyword(Keyword::True) => ast::Literal::Boolean(true),
            TokenType::Keyword(Keyword::False) => ast::Literal::Boolean(false),
            _ => return Err(SyntaxError::InvalidConversion),
        };
        Ok(literal)
    }
}
