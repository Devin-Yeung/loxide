use crate::error::RuntimeError;
use crate::value::Value;
use std::collections::HashMap;

pub struct Environment<'a> {
    values: HashMap<String, Value>,
    parent: Option<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    pub fn new(parent: Option<&'a Environment<'a>>) -> Environment<'a> {
        Environment {
            values: HashMap::new(),
            parent,
        }
    }

    pub fn get(&self, name: &str) -> Result<&Value, RuntimeError> {
        self.values.get(name).map_or_else(
            || {
                self.parent
                    .map(|parent| parent.get(name))
                    .unwrap_or(Err(RuntimeError::UndefinedVariable(name.to_string())))
            },
            Ok,
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

#[cfg(test)]
mod test {
    use crate::environment::Environment;
    use crate::value::Value;

    #[test]
    fn nested() {
        let mut global = Environment::new(None);
        global.define("a".to_string(), Value::Number(1.0));
        let mut outer = Environment::new(Some(&global));
        outer.define("b".to_string(), Value::Number(2.0));
        outer.define("c".to_string(), Value::Number(0.0));
        let mut inner = Environment::new(Some(&outer));
        inner.define("c".to_string(), Value::Number(3.0)); // shadow c

        assert_eq!(inner.get("a").unwrap().as_number().unwrap(), 1.0);
        assert_eq!(inner.get("b").unwrap().as_number().unwrap(), 2.0);
        assert_eq!(inner.get("c").unwrap().as_number().unwrap(), 3.0);
        assert_eq!(outer.get("c").unwrap().as_number().unwrap(), 0.0);
    }
}
