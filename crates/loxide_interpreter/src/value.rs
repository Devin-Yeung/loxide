use crate::callable::Callable;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    Void,
    Callable(Callable),
}

#[derive(Debug, Eq, PartialEq)]
pub enum ValueKind {
    Number,
    String,
    Boolean,
    Nil,
    Void,
    Callable,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(v) => write!(f, "num({})", v),
            Value::String(v) => write!(f, "str({})", v),
            Value::Boolean(v) => write!(f, "bool({})", v),
            Value::Nil => write!(f, "nil"),
            Value::Void => write!(f, "void"),
            Value::Callable(_) => write!(f, "<callable>"),
        }
    }
}

impl Display for ValueKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueKind::Number => write!(f, "number"),
            ValueKind::String => write!(f, "string"),
            ValueKind::Boolean => write!(f, "boolean"),
            ValueKind::Nil => write!(f, "nil"),
            ValueKind::Void => write!(f, "void"),
            ValueKind::Callable => write!(f, "callable"),
        }
    }
}

impl Value {
    pub fn kind(&self) -> ValueKind {
        match self {
            Value::Number(_) => ValueKind::Number,
            Value::String(_) => ValueKind::String,
            Value::Boolean(_) => ValueKind::Boolean,
            Value::Nil => ValueKind::Nil,
            Value::Void => ValueKind::Void,
            Value::Callable(_) => ValueKind::Callable,
        }
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Value::String(_))
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self, Value::Boolean(_))
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    pub fn as_number(&self) -> Option<f64> {
        match self {
            Value::Number(v) => Some(*v),
            _ => None,
        }
    }

    pub fn as_boolean(&self) -> Option<bool> {
        match self {
            Value::Boolean(v) => Some(*v),
            _ => None,
        }
    }
}
