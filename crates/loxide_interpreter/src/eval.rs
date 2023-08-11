use crate::error::RuntimeError;
use crate::value::Value;
use loxide_parser::ast::{
    BinaryExpr, BinaryOperator, Expr, ExprKind, GroupedExpr, Literal, Stmt, UnaryExpr,
    UnaryOperator,
};

pub trait Evaluable {
    fn eval(&self) -> Result<Value, RuntimeError>;
}

macro_rules! inner_or {
    ($val:expr, $ty:tt, $err:expr) => {{
        ::paste::paste! {
            if $val.[<is_$ty>]() {
                Ok($val.[<as_$ty>]().unwrap())
            } else {
                Err($err)
            }
        }
    }};
}

impl<'src> Evaluable for Stmt<'src> {
    fn eval(&self) -> Result<Value, RuntimeError> {
        match self {
            Stmt::Expression(e) => e.eval(),
            Stmt::PrintStmt(_) => todo!("design decision: print stmt cant be evaluated to a value"),
            _ => unreachable!(),
        }
    }
}

impl<'src> Evaluable for Expr<'src> {
    fn eval(&self) -> Result<Value, RuntimeError> {
        return match &self.kind {
            ExprKind::Literal(l) => l.eval(),
            ExprKind::Unary(u) => u.eval(),
            ExprKind::Binary(b) => b.eval(),
            ExprKind::Grouped(g) => g.eval(),
        };
    }
}

impl<'src> Evaluable for BinaryExpr<'src> {
    fn eval(&self) -> Result<Value, RuntimeError> {
        let lhs = self.lhs.eval()?;
        let rhs = self.rhs.eval()?;
        let val = match &self.operator {
            // TODO: design decision, if two types is incompatible, give error or false?
            BinaryOperator::EqualEqual => Value::Boolean(lhs == rhs),
            // TODO: design decision, if two types is incompatible, give error or false?
            BinaryOperator::BangEq => Value::Boolean(lhs != rhs),
            BinaryOperator::GreaterEqual => {
                let lhs = inner_or!(lhs, number, RuntimeError::InvalidBinaryOperand("number"))?;
                let rhs = inner_or!(rhs, number, RuntimeError::InvalidBinaryOperand("number"))?;
                Value::Boolean(lhs >= rhs)
            }
            BinaryOperator::Greater => {
                let lhs = inner_or!(lhs, number, RuntimeError::InvalidBinaryOperand("number"))?;
                let rhs = inner_or!(rhs, number, RuntimeError::InvalidBinaryOperand("number"))?;
                Value::Boolean(lhs > rhs)
            }
            BinaryOperator::Less => {
                let lhs = inner_or!(lhs, number, RuntimeError::InvalidBinaryOperand("number"))?;
                let rhs = inner_or!(rhs, number, RuntimeError::InvalidBinaryOperand("number"))?;
                Value::Boolean(lhs < rhs)
            }
            BinaryOperator::LessEqual => {
                let lhs = inner_or!(lhs, number, RuntimeError::InvalidBinaryOperand("number"))?;
                let rhs = inner_or!(rhs, number, RuntimeError::InvalidBinaryOperand("number"))?;
                Value::Boolean(lhs <= rhs)
            }
            BinaryOperator::Minus => {
                let lhs = inner_or!(lhs, number, RuntimeError::InvalidBinaryOperand("number"))?;
                let rhs = inner_or!(rhs, number, RuntimeError::InvalidBinaryOperand("number"))?;
                Value::Number(lhs - rhs)
            }
            BinaryOperator::Plus => {
                let lhs = inner_or!(lhs, number, RuntimeError::InvalidBinaryOperand("number"))?;
                let rhs = inner_or!(rhs, number, RuntimeError::InvalidBinaryOperand("number"))?;
                Value::Number(lhs + rhs)
            }
            BinaryOperator::Slash => {
                let lhs = inner_or!(lhs, number, RuntimeError::InvalidBinaryOperand("number"))?;
                let rhs = inner_or!(rhs, number, RuntimeError::InvalidBinaryOperand("number"))?;
                Value::Number(lhs / rhs)
            }
            BinaryOperator::Star => {
                let lhs = inner_or!(lhs, number, RuntimeError::InvalidBinaryOperand("number"))?;
                let rhs = inner_or!(rhs, number, RuntimeError::InvalidBinaryOperand("number"))?;
                Value::Number(lhs * rhs)
            }
        };
        Ok(val)
    }
}

impl<'src> Evaluable for UnaryExpr<'src> {
    fn eval(&self) -> Result<Value, RuntimeError> {
        let value = self.expr.eval()?;
        return match self.operator {
            UnaryOperator::Minus => {
                let num = inner_or!(value, number, RuntimeError::InvalidUnaryOperand("number"))?;
                Ok(Value::Number(-num))
            }
            UnaryOperator::Bang => {
                let bool = inner_or!(value, boolean, RuntimeError::InvalidUnaryOperand("boolean"))?;
                Ok(Value::Boolean(!bool))
            }
        };
    }
}

impl<'src> Evaluable for GroupedExpr<'src> {
    fn eval(&self) -> Result<Value, RuntimeError> {
        self.expr.eval()
    }
}

impl Evaluable for Literal<'_> {
    fn eval(&self) -> Result<Value, RuntimeError> {
        let value = match *self {
            Literal::String(ref v) => Value::String(v.to_string()),
            Literal::Number(v) => Value::Number(v),
            Literal::Boolean(v) => Value::Boolean(v),
            Literal::Nil => Value::Nil,
        };
        Ok(value)
    }
}

#[cfg(test)]
mod tests {
    use crate::error::RuntimeError;
    use crate::eval::Evaluable;
    use crate::value::Value;
    use loxide_macros::src;

    const SNAPSHOT_INPUT_BASE: &'static str = "snapshots/interpreter/snapshots-inputs";
    const SNAPSHOT_OUTPUT: &'static str = "../snapshots/interpreter/snapshots-outputs";

    fn eval(src: &str) -> Vec<Result<Value, RuntimeError>> {
        let mut parser = loxide_parser::parser::Parser::new(src);
        let (stmts, _) = parser.parse();
        stmts.into_iter().map(|stmt| stmt.eval()).collect()
    }

    #[test]
    fn literal() {
        insta::with_settings!({snapshot_path => SNAPSHOT_OUTPUT},{
            let src = src!(SNAPSHOT_INPUT_BASE, "literal.lox");
            let results = eval(&src);
            insta::assert_debug_snapshot!(results);
        })
    }

    #[test]
    fn valid_unary() {
        insta::with_settings!({snapshot_path => SNAPSHOT_OUTPUT},{
            let src = src!(SNAPSHOT_INPUT_BASE, "valid_unary.lox");
            let results = eval(&src);
            insta::assert_debug_snapshot!(results);
        })
    }

    #[test]
    fn invalid_unary() {
        insta::with_settings!({snapshot_path => SNAPSHOT_OUTPUT},{
            let src = src!(SNAPSHOT_INPUT_BASE, "invalid_unary.lox");
            let results: Vec<_> = src.split('\n').into_iter().map(|src| eval(&src)).collect();
            insta::assert_debug_snapshot!(results);
        })
    }

    #[test]
    fn valid_binary() {
        insta::with_settings!({snapshot_path => SNAPSHOT_OUTPUT},{
            let src = src!(SNAPSHOT_INPUT_BASE, "valid_binary.lox");
            let results = eval(&src);
            insta::assert_debug_snapshot!(results);
        })
    }
}
