use crate::environment::Environment;
use crate::error::RuntimeError;
use crate::value::Value;
use loxide_parser::ast::{
    AssignExpr, BinaryExpr, BinaryOperator, Expr, ExprKind, GroupedExpr, Literal, Stmt, UnaryExpr,
    UnaryOperator, Variable,
};
use loxide_testsuite::probe;

pub trait Evaluable {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError>;
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
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        match self {
            Stmt::Expression(e) => e.eval(env),
            Stmt::PrintStmt(e) => {
                let val = probe!(e.eval(env))?;
                Ok(Value::Void)
            }
            Stmt::VarDeclaration(var, expr) => {
                let val = expr.eval(env)?;
                env.define(var.name.to_string(), val);
                Ok(Value::Void)
            }
            Stmt::Block(stmts) => {
                // TODO: find a way to test the correctness of this evaluation
                let mut scope = env.extend();
                for stmt in stmts {
                    stmt.eval(&mut scope)?;
                }
                Ok(Value::Void)
            }
            _ => unreachable!(),
        }
    }
}

impl<'src> Evaluable for Option<Expr<'src>> {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        let val = match self {
            None => Value::Nil,
            Some(e) => e.eval(env)?,
        };
        Ok(val)
    }
}

impl<'src> Evaluable for Expr<'src> {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        return match &self.kind {
            ExprKind::Literal(l) => l.eval(env),
            ExprKind::Unary(u) => u.eval(env),
            ExprKind::Binary(b) => b.eval(env),
            ExprKind::Grouped(g) => g.eval(env),
            ExprKind::Variable(v) => v.eval(env),
            ExprKind::Assign(a) => a.eval(env),
            _ => unreachable!(),
        };
    }
}

impl<'src> Evaluable for AssignExpr<'src> {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        let val = self.value.eval(env)?;
        env.update(self.name.name, val.clone())?;
        Ok(val)
    }
}

impl<'src> Evaluable for BinaryExpr<'src> {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        let lhs = self.lhs.eval(env)?;
        let rhs = self.rhs.eval(env)?;
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
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        let value = self.expr.eval(env)?;
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
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        self.expr.eval(env)
    }
}

impl<'src> Evaluable for Variable<'src> {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        env.get(self.name).map(|v| v.to_owned())
    }
}

impl Evaluable for Literal<'_> {
    fn eval(&self, _env: &mut Environment) -> Result<Value, RuntimeError> {
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
    use crate::environment::Environment;
    use crate::error::RuntimeError;
    use crate::eval::Evaluable;
    use crate::value::Value;
    use loxide_macros::src;
    use loxide_testsuite::{footprints, register};

    const SNAPSHOT_INPUT_BASE: &'static str = "snapshots/interpreter/snapshots-inputs";
    const SNAPSHOT_OUTPUT: &'static str = "../snapshots/interpreter/snapshots-outputs";

    fn eval(src: &str) -> Vec<Result<Value, RuntimeError>> {
        let mut parser = loxide_parser::parser::Parser::new(src);
        let mut env = Environment::global();
        let (stmts, _) = parser.parse();
        stmts.into_iter().map(|stmt| stmt.eval(&mut env)).collect()
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

    #[test]
    fn variable() {
        insta::with_settings!({snapshot_path => SNAPSHOT_OUTPUT},{
            let src = src!(SNAPSHOT_INPUT_BASE, "variable.lox");
            let results = eval(&src);
            insta::assert_debug_snapshot!(results);
        })
    }

    #[test]
    fn block_stmt() {
        insta::with_settings!({snapshot_path => SNAPSHOT_OUTPUT},{
            register!();
            let src = src!(SNAPSHOT_INPUT_BASE, "block_stmt.lox");
            eval(&src);
            insta::assert_debug_snapshot!(footprints!());
        })
    }
}
