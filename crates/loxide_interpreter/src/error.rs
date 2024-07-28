use crate::value::Value;
use loxide_parser::token::Span;
use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
#[non_exhaustive]
pub enum RuntimeError {
    #[error("Invalid unary operand, expected: {1}")]
    InvalidUnaryOperand(#[label("expect a {1} type here")] Span, &'static str),
    #[error("Invalid binary operand, expected: {0}")]
    InvalidBinaryOperand(&'static str),
    #[error("Incompatible operand, {lhs} is incompatible with {rhs}")]
    IncompatibleBinaryOperand {
        lhs: &'static str,
        rhs: &'static str,
    },
    #[error("Undefined Variable: {0}")]
    UndefinedVariable(String),
    #[error("Expected boolean in condition expression")]
    ExpectedBoolean,
    #[error("Return value")]
    ReturnValue(Value),
    #[error("Bad arity, expected: {expected}, found: {found}")]
    BadArity { expected: usize, found: usize },
    #[error("Function call on a non-callable value")]
    CallOnNonCallable,
}
