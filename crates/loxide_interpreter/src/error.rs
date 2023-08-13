use thiserror::Error;

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum RuntimeError {
    #[error("Invalid unary operand, expected: {0}")]
    InvalidUnaryOperand(&'static str),
    #[error("Invalid binary operand, expected: {0}")]
    InvalidBinaryOperand(&'static str),
    #[error("Incompatible operand, {lhs} is incompatible with {rhs}")]
    IncompatibleBinaryOperand {
        lhs: &'static str,
        rhs: &'static str,
    },
    #[error("Undefined Variable: {0}")]
    UndefinedVariable(String),
}
