use crate::environment::Environment;
use crate::error::RuntimeError;
use crate::value::Value;
use loxide_parser::ast::FunDeclaration;
use std::rc::Rc;

#[derive(Debug)]
#[non_exhaustive]
pub enum Callable {
    Function(LoxFunction),
}

impl Clone for Callable {
    fn clone(&self) -> Self {
        todo!()
    }
}

impl PartialEq for Callable {
    fn eq(&self, other: &Self) -> bool {
        todo!()
    }
}

impl Callable {
    fn call(&self, arguments: Vec<Value>, env: &mut Environment) -> Result<Value, RuntimeError> {
        match self {
            Callable::Function(func) => func.call(arguments, env),
        }
    }
}

#[derive(Debug)]
pub struct LoxFunction {
    // we don't want to introduce any lifetime here,
    // since it will propagate to `Value`
    declaration: Rc<FunDeclaration>,
}

impl LoxFunction {
    pub fn new(declaration: Rc<FunDeclaration>) -> LoxFunction {
        LoxFunction { declaration }
    }

    pub fn call(
        &self,
        arguments: Vec<Value>,
        env: &mut Environment,
    ) -> Result<Value, RuntimeError> {
        todo!()
    }
}

impl Callable {
    pub fn function(declaration: Rc<FunDeclaration>) -> Callable {
        Callable::Function(LoxFunction::new(declaration))
    }
}
