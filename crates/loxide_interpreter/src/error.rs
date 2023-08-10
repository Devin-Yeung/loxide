use thiserror::Error;

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum RuntimeError {
    #[error("Invalid unary operand, expected: {0}")]
    InvalidUnaryOperand(&'static str),
}
