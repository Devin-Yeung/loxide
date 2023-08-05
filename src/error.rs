use thiserror::Error;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum SyntaxError {
    #[error("Unexpected character at line {0}")]
    UnexpectedChar(usize),
    #[error("Invalid Number")]
    InvalidNumber,
    #[error("Invalid string literal")]
    InvalidStringLiteral,
    #[error("Invalid keyword")]
    InvalidKeywords,
}
