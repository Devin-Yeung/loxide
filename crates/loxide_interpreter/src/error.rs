use crate::value::{Value, ValueKind};
use loxide_parser::token::Span;
use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
#[non_exhaustive]
pub enum RuntimeError {
    #[error("Invalid unary operand, expected: {1}")]
    InvalidUnaryOperand(#[label("expect a {1} type here")] Span, &'static str),
    #[error("Invalid binary operand, expected: {1}")]
    InvalidBinaryOperand(#[label("expect a {1} type here")] Span, &'static str),
    #[error("Incompatible operand, {lhs_ty} is incompatible with {rhs_ty}")]
    IncompatibleBinaryOperand {
        #[label("has type `{lhs_ty}` here")]
        lhs_span: Span,
        lhs_ty: ValueKind,
        #[label("has type `{rhs_ty}` here")]
        rhs_span: Span,
        rhs_ty: ValueKind,
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
