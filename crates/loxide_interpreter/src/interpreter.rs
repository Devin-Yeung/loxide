use crate::environment::Environment;
use crate::error::RuntimeError;
use crate::eval::Evaluable;
use crate::value::Value;
use std::io::Write;

pub struct Interpreter {
    global: Environment,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    pub fn new() -> Self {
        let global = Environment::global();

        Interpreter { global }
    }

    pub fn interpret<E: Evaluable, W: Write>(
        &mut self,
        executable: E,
        stdout: &mut W,
    ) -> Result<Value, RuntimeError> {
        executable.eval(&mut self.global, stdout)
    }
}
