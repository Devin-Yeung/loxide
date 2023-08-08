use thiserror::Error;

#[derive(Error, Debug, Eq, PartialEq, Copy, Clone)]
pub enum SyntaxError {
    #[error("Unexpected character at line {0}")]
    UnexpectedChar(usize),
    #[error("Unexpected EOF")]
    UnexpectedEOF,
    #[error("Expected")]
    Expect(&'static str),
    #[error("Invalid Conversion")]
    InvalidConversion,
    #[error("Invalid Number")]
    InvalidNumber,
    #[error("Invalid string literal")]
    InvalidStringLiteral,
    #[error("Invalid keyword")]
    InvalidKeywords,
}
