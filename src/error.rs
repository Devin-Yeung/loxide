use thiserror::Error;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum SyntaxError {
    #[error("Unexpected character at line {0}")]
    UnexpectedChar(usize),
}
