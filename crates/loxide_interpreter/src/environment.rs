use crate::error::RuntimeError;
use crate::value::Value;
use loxide_parser::ast::Identifier;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

struct Inner {
    values: HashMap<String, Value>,
    parent: Option<Rc<RefCell<Inner>>>,
}

impl Inner {
    fn get(&self, ident: &Identifier) -> Result<Value, RuntimeError> {
        self.values.get(&ident.name).map_or_else(
            || {
                self.parent
                    .as_ref()
                    .map(|parent| parent.borrow().get(ident))
                    .unwrap_or(Err(RuntimeError::UndefinedVariable(
                        ident.span,
                        ident.name.to_string(),
                    )))
            },
            |v| Ok(v.clone()),
        )
    }

    fn define(&mut self, ident: &Identifier, value: Value) {
        self.values.insert(ident.name.to_string(), value);
    }

    fn mutate(&mut self, ident: &Identifier, value: Value) -> Result<(), RuntimeError> {
        match self.values.get_mut(&ident.name) {
            Some(existed) => {
                *existed = value;
                Ok(())
            }
            None => match &self.parent {
                None => Err(RuntimeError::UndefinedVariable(
                    ident.span,
                    ident.name.to_string(),
                )),
                Some(env) => env.borrow_mut().mutate(ident, value),
            },
        }
    }
}

pub struct Environment {
    inner: Rc<RefCell<Inner>>,
}

impl Environment {
    /// create a global scope
    pub fn global() -> Environment {
        Environment {
            inner: Rc::new(RefCell::new(Inner {
                values: HashMap::new(),
                parent: None,
            })),
        }
    }

    /// get the value of variable from the current scope,
    /// if current does not define this variable, search
    /// in the ancestor scope, if nothing found,
    /// return an UndefinedVariable runtime error
    pub fn get(&self, ident: &Identifier) -> Result<Value, RuntimeError> {
        self.inner.borrow().get(ident)
    }

    /// define variable in current scope,
    /// if variable already defined in current scope,
    /// the previous value will be overwritten
    pub fn define(&mut self, ident: &Identifier, value: Value) {
        self.inner.borrow_mut().define(ident, value);
    }

    /// mutate variable in current scope,
    /// if current does not define this variable,
    /// mutate the variable in the ancestor scope,
    /// if nothing found in ancestor scope,
    /// return an UndefinedVariable runtime error
    pub fn mutate(&mut self, ident: &Identifier, value: Value) -> Result<(), RuntimeError> {
        self.inner.borrow_mut().mutate(ident, value)
    }

    /// create a descendant scope
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
    use loxide_parser::ast::Identifier;
    use loxide_parser::token::Span;

    fn ident<S: AsRef<str>>(s: S) -> Identifier {
        Identifier {
            name: s.as_ref().to_string(),
            span: Span::new(0, 1),
        }
    }
    macro_rules! define {
        ($scope:expr, {$($name:expr => $val:expr),*$(,)?}) => {{
            $($scope.define(&ident($name), $val));*
        }};
    }

    macro_rules! check_scope {
        ($scope:expr, {$($name:expr => $val:expr),*$(,)?}) => {{
            $(assert_eq!($scope.get(&ident($name)).unwrap().as_number().unwrap(), $val);)*
        }};
    }

    macro_rules! mutate {
        ($scope:expr, {$($name:expr => $val:expr),*$(,)?}) => {{
            $($scope.mutate(&ident($name), $val).unwrap());*
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
