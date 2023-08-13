use crate::error::RuntimeError;
use crate::value::Value;
use std::collections::HashMap;

pub struct Environment {
    pub values: HashMap<String, Value>,
}

impl<'src> Environment {
    pub fn new() -> Environment {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Result<&Value, RuntimeError> {
        self.values.get(name).map_or(
            Err(RuntimeError::UndefinedVariable(name.to_string())),
            |v| Ok(v),
        )
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn update(&mut self, name: &str, value: Value) -> Result<(), RuntimeError> {
        match self.values.get_mut(name) {
            None => return Err(RuntimeError::UndefinedVariable(name.to_string())),
            Some(existed) => {
                *existed = value;
                Ok(())
            }
        }
    }
}
