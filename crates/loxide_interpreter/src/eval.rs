use crate::error::RuntimeError;
use crate::value::Value;
use loxide_parser::ast::{Expr, Stmt};

pub trait Evaluable {
    fn eval(&self) -> Result<Value, RuntimeError>;
}

impl<'src> Evaluable for Expr<'src> {
    fn eval(&self) -> Result<Value, RuntimeError> {
        todo!()
    }
}

impl<'src> Evaluable for Stmt<'src> {
    fn eval(&self) -> Result<Value, RuntimeError> {
        todo!()
    }
}
