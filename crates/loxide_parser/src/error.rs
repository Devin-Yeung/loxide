use crate::token::Span;
use miette;
use miette::Diagnostic;
use thiserror::Error;

#[derive(Error, Debug, Eq, PartialEq, Copy, Clone, Diagnostic)]
pub enum SyntaxError {
    #[error("Unexpected character")]
    UnexpectedChar(#[label("this character is unexpected")] Span),
    #[error("Unexpected EOF")]
    UnexpectedEOF,
    #[error("Expected token {expected}, found {found}")]
    UnexpectedToken {
        expected: &'static str,
        found: &'static str,
    },
    #[error("Expected")]
    Expect(&'static str),
    #[error("Invalid Conversion")]
    InvalidConversion,
    #[error("Invalid Number")]
    InvalidNumber(#[label("this number is invalid")] Span),
    #[error("Invalid string literal")]
    InvalidStringLiteral(#[label("this string literal is invalid")] Span),
    #[error("Invalid keyword")]
    InvalidKeywords,
    #[error("Invalid assignment target.")]
    InvalidAssignmentTarget,
}
