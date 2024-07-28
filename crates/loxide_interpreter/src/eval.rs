use crate::callable::Callable;
use crate::environment::Environment;
use crate::error::RuntimeError;
use crate::value::{Value, ValueKind};
use loxide_parser::ast::{
    AssignExpr, BinaryExpr, BinaryOperator, CallExpr, ConditionStmt, Expr, ExprKind, ForStmt,
    GroupedExpr, Identifier, Literal, ReturnStmt, Stmt, UnaryExpr, UnaryOperator, WhileStmt,
};
use loxide_parser::token::Span;
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

impl Evaluable for Stmt {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        match self {
            Stmt::Expression(e) => e.eval(env),
            Stmt::PrintStmt(e) => {
                probe!(e.eval(env))?;
                Ok(Value::Void)
            }
            Stmt::VarDeclaration(var, expr) => {
                let val = expr.eval(env)?;
                env.define(var.name.to_string(), val);
                Ok(Value::Void)
            }
            Stmt::FunDeclaration(decl) => {
                let callable = Callable::function(decl.clone());
                env.define(decl.name.name.to_string(), Value::Callable(callable));
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
            Stmt::Condition(cond) => cond.eval(env),
            Stmt::While(cond) => cond.eval(env),
            Stmt::For(stmt) => stmt.eval(env),
            Stmt::ReturnStmt(stmt) => stmt.eval(env),
            _ => unreachable!(),
        }
    }
}

impl Evaluable for ConditionStmt {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        let mut env = env.extend();
        match self.condition.eval(&mut env)? {
            Value::Boolean(b) => {
                if b {
                    self.then_branch.eval(&mut env)
                } else {
                    match &self.else_branch {
                        None => Ok(Value::Void),
                        Some(stmt) => stmt.eval(&mut env),
                    }
                }
            }
            _ => Err(RuntimeError::ExpectedBoolean),
        }
    }
}

impl Evaluable for WhileStmt {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        while let Value::Boolean(b) = self.condition.eval(env)? {
            if b {
                self.body.eval(env)?;
            } else {
                return Ok(Value::Void);
            }
        }
        Err(RuntimeError::ExpectedBoolean)
    }
}

impl Evaluable for ForStmt {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        let mut env = env.extend();
        let _ = &self.initializer.eval(&mut env)?;
        match &self.condition {
            None => {
                // infinite loop
                loop {
                    self.body.eval(&mut env)?;
                    self.increment.eval(&mut env)?;
                }
            }
            Some(cond) => {
                while let Value::Boolean(b) = cond.eval(&mut env)? {
                    if b {
                        self.body.eval(&mut env)?;
                        self.increment.eval(&mut env)?;
                    } else {
                        return Ok(Value::Void);
                    }
                }
            }
        }
        Err(RuntimeError::ExpectedBoolean)
    }
}

impl Evaluable for ReturnStmt {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        self.value
            .as_ref()
            .map_or(Err(RuntimeError::ReturnValue(Value::Void)), |expr| {
                let value = expr.eval(env)?;
                Err(RuntimeError::ReturnValue(value))
            })
    }
}

impl Evaluable for Option<Box<Stmt>> {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        if let Some(stmt) = self {
            stmt.eval(env)
        } else {
            Ok(Value::Void)
        }
    }
}

impl Evaluable for Option<Box<Expr>> {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        let val = match self {
            None => Value::Nil,
            Some(e) => e.eval(env)?,
        };
        Ok(val)
    }
}

impl Evaluable for Option<Expr> {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        let val = match self {
            None => Value::Nil,
            Some(e) => e.eval(env)?,
        };
        Ok(val)
    }
}

impl Evaluable for Expr {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        match &self.kind {
            ExprKind::Literal(l) => l.eval(env),
            ExprKind::Unary(u) => u.eval(env),
            ExprKind::Binary(b) => b.eval(env),
            ExprKind::Grouped(g) => g.eval(env),
            ExprKind::Variable(v) => v.eval(env),
            ExprKind::Assign(a) => a.eval(env),
            ExprKind::Call(c) => c.eval(env),
            _ => unreachable!(),
        }
    }
}

impl Evaluable for CallExpr {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        let callee = self.callee.eval(env)?;
        let args = self
            .args
            .iter()
            .map(|expr| expr.eval(env))
            .collect::<Result<Vec<Value>, RuntimeError>>()?;

        if let Value::Callable(callable) = callee {
            return callable.call(args, env);
        }

        Err(RuntimeError::CallOnNonCallable)
    }
}

impl Evaluable for AssignExpr {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        let val = self.value.eval(env)?;
        env.mutate(&self.name.name, val.clone())?;
        Ok(val)
    }
}

impl Evaluable for BinaryExpr {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        let lhs = self.lhs.eval(env)?;
        let rhs = self.rhs.eval(env)?;
        let val = match &self.operator {
            op @ (BinaryOperator::EqualEqual | BinaryOperator::BangEq) => {
                return if lhs.kind() == rhs.kind()
                    && (lhs.kind() == ValueKind::Number
                        || lhs.kind() == ValueKind::Boolean
                        || lhs.kind() == ValueKind::String)
                {
                    match op {
                        BinaryOperator::EqualEqual => Ok(Value::Boolean(lhs == rhs)),
                        BinaryOperator::BangEq => Ok(Value::Boolean(lhs != rhs)),
                        _ => unreachable!(),
                    }
                } else {
                    Err(RuntimeError::IncompatibleBinaryOperand {
                        span: Span::new(self.lhs.span().start, self.rhs.span().end),
                        lhs: lhs.kind(),
                        rhs: rhs.kind(),
                    })
                }
            }
            BinaryOperator::GreaterEqual => {
                let lhs = inner_or!(
                    lhs,
                    number,
                    RuntimeError::InvalidBinaryOperand(self.lhs.span(), "number")
                )?;
                let rhs = inner_or!(
                    rhs,
                    number,
                    RuntimeError::InvalidBinaryOperand(self.rhs.span(), "number")
                )?;
                Value::Boolean(lhs >= rhs)
            }
            BinaryOperator::Greater => {
                let lhs = inner_or!(
                    lhs,
                    number,
                    RuntimeError::InvalidBinaryOperand(self.lhs.span(), "number")
                )?;
                let rhs = inner_or!(
                    rhs,
                    number,
                    RuntimeError::InvalidBinaryOperand(self.rhs.span(), "number")
                )?;
                Value::Boolean(lhs > rhs)
            }
            BinaryOperator::Less => {
                let lhs = inner_or!(
                    lhs,
                    number,
                    RuntimeError::InvalidBinaryOperand(self.lhs.span(), "number")
                )?;
                let rhs = inner_or!(
                    rhs,
                    number,
                    RuntimeError::InvalidBinaryOperand(self.rhs.span(), "number")
                )?;
                Value::Boolean(lhs < rhs)
            }
            BinaryOperator::LessEqual => {
                let lhs = inner_or!(
                    lhs,
                    number,
                    RuntimeError::InvalidBinaryOperand(self.lhs.span(), "number")
                )?;
                let rhs = inner_or!(
                    rhs,
                    number,
                    RuntimeError::InvalidBinaryOperand(self.rhs.span(), "number")
                )?;
                Value::Boolean(lhs <= rhs)
            }
            BinaryOperator::Minus => {
                let lhs = inner_or!(
                    lhs,
                    number,
                    RuntimeError::InvalidBinaryOperand(self.lhs.span(), "number")
                )?;
                let rhs = inner_or!(
                    rhs,
                    number,
                    RuntimeError::InvalidBinaryOperand(self.rhs.span(), "number")
                )?;
                Value::Number(lhs - rhs)
            }
            BinaryOperator::Plus => {
                let lhs = inner_or!(
                    lhs,
                    number,
                    RuntimeError::InvalidBinaryOperand(self.lhs.span(), "number")
                )?;
                let rhs = inner_or!(
                    rhs,
                    number,
                    RuntimeError::InvalidBinaryOperand(self.rhs.span(), "number")
                )?;
                Value::Number(lhs + rhs)
            }
            BinaryOperator::Slash => {
                let lhs = inner_or!(
                    lhs,
                    number,
                    RuntimeError::InvalidBinaryOperand(self.lhs.span(), "number")
                )?;
                let rhs = inner_or!(
                    rhs,
                    number,
                    RuntimeError::InvalidBinaryOperand(self.rhs.span(), "number")
                )?;
                Value::Number(lhs / rhs)
            }
            BinaryOperator::Star => {
                let lhs = inner_or!(
                    lhs,
                    number,
                    RuntimeError::InvalidBinaryOperand(self.lhs.span(), "number")
                )?;
                let rhs = inner_or!(
                    rhs,
                    number,
                    RuntimeError::InvalidBinaryOperand(self.rhs.span(), "number")
                )?;
                Value::Number(lhs * rhs)
            }
        };
        Ok(val)
    }
}

impl Evaluable for UnaryExpr {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        let value = self.expr.eval(env)?;
        match self.operator {
            UnaryOperator::Minus => {
                let num = inner_or!(
                    value,
                    number,
                    RuntimeError::InvalidUnaryOperand(self.expr.span(), "number")
                )?;
                Ok(Value::Number(-num))
            }
            UnaryOperator::Bang => {
                let bool = inner_or!(
                    value,
                    boolean,
                    RuntimeError::InvalidUnaryOperand(self.expr.span(), "boolean")
                )?;
                Ok(Value::Boolean(!bool))
            }
        }
    }
}

impl Evaluable for GroupedExpr {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        self.expr.eval(env)
    }
}

impl Evaluable for Identifier {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        env.get(&self.name)
    }
}

impl Evaluable for Literal {
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
    use loxide_diagnostic::reporter::{Reporter, Style};
    use loxide_testsuite::{footprints, register, unittest};
    use miette::Report;
    use std::sync::Arc;

    fn eval(src: &str) -> Vec<Result<Value, RuntimeError>> {
        let mut parser = loxide_parser::parser::Parser::new(src);
        let mut env = Environment::global();
        let (stmts, _) = parser.parse();
        stmts.into_iter().map(|stmt| stmt.eval(&mut env)).collect()
    }

    fn display_eval_error(src: &str) -> String {
        let mut parser = loxide_parser::parser::Parser::new(src);
        let mut env = Environment::global();
        let source = Arc::new(src.to_string());

        let (stmts, _) = parser.parse();
        stmts
            .into_iter()
            .map(|stmt| stmt.eval(&mut env))
            .filter(Result::is_err)
            .map(Result::unwrap_err)
            .map(|err| {
                let mut reporter = Reporter::new(Style::NoColor);
                let report: Report = err.into();
                reporter.push(report.with_source_code(source.clone()));
                reporter.report_to_string()
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    unittest!(literal, |src| {
        let results = eval(src);
        insta::assert_debug_snapshot!(results);
    });

    unittest!(valid_unary, |src| {
        let results = eval(src);
        insta::assert_debug_snapshot!(results);
    });

    unittest!(invalid_unary, |src| {
        let results = src
            .split('\n')
            .map(display_eval_error)
            .collect::<Vec<_>>()
            .join("\n");
        insta::assert_snapshot!(results);
    });

    unittest!(invalid_binary, |src| {
        let results = src
            .split('\n')
            .map(display_eval_error)
            .collect::<Vec<_>>()
            .join("\n");
        insta::assert_snapshot!(results);
    });

    unittest!(valid_binary, |src| {
        let results = eval(src);
        insta::assert_debug_snapshot!(results);
    });

    unittest!(variable, |src| {
        let results: Vec<_> = eval(src);
        insta::assert_debug_snapshot!(results);
    });

    unittest!(block_stmt, |src| {
        register!();
        eval(src);
        insta::assert_debug_snapshot!(footprints!());
    });

    unittest!(if_stmt, |src| {
        register!();
        eval(src);
        insta::assert_debug_snapshot!(footprints!());
    });

    unittest!(while_stmt, |src| {
        register!();
        eval(src);
        insta::assert_debug_snapshot!(footprints!());
    });

    unittest!(for_stmt, |src| {
        register!();
        eval(src);
        insta::assert_debug_snapshot!(footprints!());
    });

    unittest!(for_stmt_expect_bool, |src| {
        let results: Vec<_> = eval(src);
        insta::assert_debug_snapshot!(results);
    });

    unittest!(fn_call, |src| {
        register!();
        eval(src);
        insta::assert_debug_snapshot!(footprints!());
    });

    unittest!(bad_fn_call, |src| {
        let results: Vec<_> = eval(src);
        insta::assert_debug_snapshot!(results);
    });
}
