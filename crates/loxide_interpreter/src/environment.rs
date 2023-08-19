use crate::error::RuntimeError;
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

struct Inner {
    values: HashMap<String, Value>,
    parent: Option<Rc<RefCell<Inner>>>,
}

impl Inner {
    fn get(&self, name: &str) -> Result<Value, RuntimeError> {
        self.values.get(name).map_or_else(
            || {
                self.parent
                    .as_ref()
                    .map(|parent| parent.borrow().get(name))
                    .unwrap_or(Err(RuntimeError::UndefinedVariable(name.to_string())))
            },
            |v| Ok(v.clone()),
        )
    }

    fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    fn mutate(&mut self, name: &str, value: Value) -> Result<(), RuntimeError> {
        match self.values.get_mut(name) {
            Some(existed) => {
                *existed = value;
                Ok(())
            }
            None => match &self.parent {
                None => Err(RuntimeError::UndefinedVariable(name.to_string())),
                Some(env) => env.borrow_mut().mutate(name, value),
            },
        }
    }
}

pub struct Environment {
    inner: Rc<RefCell<Inner>>,
}

impl Environment {
    pub fn global() -> Environment {
        Environment {
            inner: Rc::new(RefCell::new(Inner {
                values: HashMap::new(),
                parent: None,
            })),
        }
    }

    pub fn get(&self, name: &str) -> Result<Value, RuntimeError> {
        self.inner.borrow().get(name)
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.inner.borrow_mut().define(name, value);
    }

    pub fn mutate(&mut self, name: &str, value: Value) -> Result<(), RuntimeError> {
        self.inner.borrow_mut().mutate(name, value)
    }

    pub fn extend(&self) -> Environment {
        Environment {
            inner: Rc::new(RefCell::new(Inner {
                values: HashMap::new(),
                parent: Some(self.inner.clone()),
            })),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::environment::Environment;
    use crate::value::Value;

    macro_rules! define {
        ($scope:expr, {$($name:expr => $val:expr),*$(,)?}) => {{
            $($scope.define($name.to_string(), $val));*
        }};
    }

    macro_rules! check_scope {
        ($scope:expr, {$($name:expr => $val:expr),*$(,)?}) => {{
            $(assert_eq!($scope.get($name).unwrap().as_number().unwrap(), $val);)*
        }};
    }

    macro_rules! mutate {
        ($scope:expr, {$($name:expr => $val:expr),*$(,)?}) => {{
            $($scope.mutate($name, $val).unwrap());*
        }};
    }

    #[test]
    fn nested() {
        let mut global = Environment::global();
        define!(global, {
            "a" => Value::Number(1.0),
        });

        let mut outer = global.extend();
        define!(outer, {
            "b" => Value::Number(2.0),
            "c" => Value::Number(0.0),
        });

        let mut inner = outer.extend();
        define!(inner, {
            "c" => Value::Number(3.0)
        });

        check_scope!(inner, {
            "a" => 1.0,
            "b" => 2.0,
            "c" => 3.0,
        });

        check_scope!(outer, {
            "c" => 0.0,
        });
    }

    #[test]
    fn update_outer_scope() {
        let mut global = Environment::global();
        define!(global, {
            "a" => Value::Number(1.0),
        });

        let mut inner = global.extend();
        mutate!(inner, {
            "a" => Value::Number(2.0)
        });
        // if inner scope does not define a var, but outer scope does
        // update will update the outer scope
        check_scope!(global, {
            "a" => 2.0,
        })
    }

    #[test]
    fn shadowing_dont_update_outer_scope() {
        let mut global = Environment::global();
        define!(global, {
            "a" => Value::Number(1.0),
        });

        let mut inner = global.extend();
        define!(inner, {
            "a" => Value::Number(88.0),
        });
        // update the inner scope
        mutate!(inner, {
            "a" => Value::Number(0.0)
        });

        check_scope!(global, {
            "a" => 1.0,
        });
        check_scope!(inner, {
            "a" => 0.0,
        });
    }

    #[test]
    fn inherit_outer_scope() {
        let mut global = Environment::global();
        define!(global, {
            "a" => Value::Number(1.0),
        });
        let inner = global.extend();
        check_scope!(inner, {
            "a" => 1.0,
        });
    }
}
