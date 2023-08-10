use crate::error::RuntimeError;
use crate::value::Value;
use loxide_parser::ast::{
    BinaryExpr, Expr, ExprKind, GroupedExpr, Literal, Stmt, UnaryExpr, UnaryOperator,
};

pub trait Evaluable {
    fn eval(&self) -> Result<Value, RuntimeError>;
}

impl<'src> Evaluable for Stmt<'src> {
    fn eval(&self) -> Result<Value, RuntimeError> {
        match self {
            Stmt::Expression(e) => e.eval(),
            _ => todo!(),
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
        todo!()
    }
}

impl<'src> Evaluable for UnaryExpr<'src> {
    fn eval(&self) -> Result<Value, RuntimeError> {
        let value = self.expr.eval()?;
        return match self.operator {
            UnaryOperator::Minus => {
                if value.is_number() {
                    Ok(Value::Number(-value.as_f64().unwrap()))
                } else {
                    Err(RuntimeError::InvalidUnaryOperand("number"))
                }
            }
            UnaryOperator::Bang => {
                if value.is_boolean() {
                    Ok(Value::Boolean(!value.as_boolean().unwrap()))
                } else {
                    Err(RuntimeError::InvalidUnaryOperand("boolean"))
                }
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
}
