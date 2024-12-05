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
    #[error("Undefined Variable: {1}")]
    UndefinedVariable(#[label("variable `{1}` is not defined")] Span, String),
    #[error("Expected boolean in condition expression")]
    ExpectedBoolean(
        #[label("expect a boolean type here, but got type `{1}`")] Span,
        ValueKind,
    ),
    #[error("Return value")]
    ReturnValue(Value),
    #[error("Bad arity, expected: {expected}, found: {found}")]
    BadArity {
        #[label("function declare here with {expected} parameters")]
        callee_span: Span,
        #[label("function call here with {found} parameters")]
        caller_span: Span,
        expected: usize,
        found: usize,
    },
    #[error("Function call on a non-callable value")]
    CallOnNonCallable(
        #[label("expect a callable value here, but got type `{1}`")] Span,
        ValueKind,
    ),
    #[error("Return in top level")]
    ReturnInTopLevel(#[label("return statement is not allowed in top level")] Span),
    #[error("Too many parameters, allow at most 255, found {1}")]
    TooManyParameters(#[label("Can't have more than 255 parameters")] Span, usize),
    #[error("Too many arguments, allow at most 255, found {1}")]
    TooManyArguments(#[label("Can't have more than 255 arguments")] Span, usize),
}

#[derive(Debug)]
pub(crate) enum PrivateRuntimeError {
    /// Bad Arity Error without Span
    BadArity { expected: usize, found: usize },
    /// Transparent Error of `RuntimeError`
    Transparent(RuntimeError),
}
