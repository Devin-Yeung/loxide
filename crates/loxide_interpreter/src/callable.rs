use crate::environment::Environment;
use crate::error::RuntimeError;
use crate::eval::Evaluable;
use crate::value::Value;
use loxide_parser::ast::FunDeclaration;
use std::sync::Arc;

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum Callable {
    Function(LoxFunction),
}

impl PartialEq for Callable {
    fn eq(&self, _other: &Self) -> bool {
        todo!()
    }
}

impl Callable {
    pub fn call(
        &self,
        arguments: Vec<Value>,
        env: &mut Environment,
    ) -> Result<Value, RuntimeError> {
        match self {
            Callable::Function(func) => func.call(arguments, env),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoxFunction {
    // we don't want to introduce any lifetime here,
    // since it will propagate to `Value`
    declaration: Arc<FunDeclaration>,
}

impl LoxFunction {
    pub fn new(declaration: Arc<FunDeclaration>) -> LoxFunction {
        LoxFunction { declaration }
    }

    pub fn call(
        &self,
        arguments: Vec<Value>,
        env: &mut Environment,
    ) -> Result<Value, RuntimeError> {
        let mut env = env.extend();

        if self.declaration.params.len() != arguments.len() {
            return Err(RuntimeError::BadArity {
                span: self.declaration.paren_token,
                expected: self.declaration.params.len(),
                found: arguments.len(),
            });
        }

        // binding args to the environment
        for (value, arg) in arguments.into_iter().zip(&self.declaration.params) {
            env.define(arg.name.to_string(), value);
        }

        for stmt in &self.declaration.body {
            match stmt.eval(&mut env) {
                Ok(_) => continue,
                Err(RuntimeError::ReturnValue(ret)) => return Ok(ret),
                Err(e) => return Err(e),
            }
        }

        Ok(Value::Void)
    }
}

impl Callable {
    pub fn function(declaration: Arc<FunDeclaration>) -> Callable {
        Callable::Function(LoxFunction::new(declaration))
    }
}
