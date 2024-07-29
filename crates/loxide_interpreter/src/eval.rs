use crate::callable::Callable;
use crate::environment::Environment;
use crate::error::{PrivateRuntimeError, RuntimeError};
use crate::value::{Value, ValueKind};
use loxide_parser::ast::{
    AssignExpr, BinaryExpr, BinaryOperator, CallExpr, ConditionStmt, Expr, ExprKind, ForStmt,
    GroupedExpr, Identifier, Literal, ReturnStmt, Stmt, StmtKind, UnaryExpr, UnaryOperator,
    WhileStmt,
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

impl Evaluable for Stmt {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        self.kind.eval(env)
    }
}

impl Evaluable for StmtKind {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        match self {
            StmtKind::Expression(e) => e.eval(env),
            StmtKind::PrintStmt(e) => {
                probe!(e.eval(env)?, span => e.span().into());
                Ok(Value::Void)
            }
            StmtKind::VarDeclaration(var, expr) => {
                let val = expr.eval(env)?;
                env.define(var, val);
                Ok(Value::Void)
            }
            StmtKind::FunDeclaration(decl) => {
                let callable = Callable::function(decl.clone());
                env.define(&decl.name, Value::Callable(callable));
                Ok(Value::Void)
            }
            StmtKind::Block(stmts) => {
                // TODO: find a way to test the correctness of this evaluation
                let mut scope = env.extend();
                for stmt in stmts {
                    stmt.eval(&mut scope)?;
                }
                Ok(Value::Void)
            }
            StmtKind::Condition(cond) => cond.eval(env),
            StmtKind::While(cond) => cond.eval(env),
            StmtKind::For(stmt) => stmt.eval(env),
            StmtKind::ReturnStmt(stmt) => stmt.eval(env),
            _ => unreachable!(),
        }
    }
}

impl Evaluable for ConditionStmt {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        let mut env = env.extend();
        let guard = self.condition.eval(&mut env)?;
        match guard {
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
            _ => Err(RuntimeError::ExpectedBoolean(
                self.condition.span(),
                guard.kind(),
            )),
        }
    }
}

impl Evaluable for WhileStmt {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        let cond = &self.condition;
        loop {
            let guard = cond.eval(env)?;
            match guard {
                Value::Boolean(b) => {
                    if b {
                        self.body.eval(env)?;
                    } else {
                        return Ok(Value::Void);
                    }
                }
                _ => return Err(RuntimeError::ExpectedBoolean(cond.span(), guard.kind())),
            }
        }
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
            Some(cond) => loop {
                let guard = cond.eval(&mut env)?;
                match guard {
                    Value::Boolean(b) => {
                        if b {
                            self.body.eval(&mut env)?;
                            self.increment.eval(&mut env)?;
                        } else {
                            return Ok(Value::Void);
                        }
                    }
                    _ => return Err(RuntimeError::ExpectedBoolean(cond.span(), guard.kind())),
                }
            },
        }
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
            return match callable.call(args, env) {
                Ok(v) => Ok(v),
                Err(PrivateRuntimeError::BadArity { expected, found }) => {
                    Err(RuntimeError::BadArity {
                        callee_span: callable.params_span(),
                        caller_span: self.paren_token,
                        expected,
                        found,
                    })
                }
                Err(PrivateRuntimeError::Transparent(e)) => Err(e),
            };
        }

        Err(RuntimeError::CallOnNonCallable(
            self.callee.span(),
            callee.kind(),
        ))
    }
}

impl Evaluable for AssignExpr {
    fn eval(&self, env: &mut Environment) -> Result<Value, RuntimeError> {
        let val = self.value.eval(env)?;
        env.mutate(&self.name, val.clone())?;
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
                        lhs_ty: lhs.kind(),
                        lhs_span: self.lhs.span(),
                        rhs_ty: rhs.kind(),
                        rhs_span: self.rhs.span(),
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
        env.get(self)
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
    use crate::utils::test_utils::LINEBREAK;
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

    macro_rules! annotated_eval_test {
        ($src:expr) => {{
            let source = Arc::new($src.to_string());
            let mut reporter = Reporter::new(Style::NoColor);
            // linebreaks[0] means very char before index linebreaks[0] is in line 0
            let linebreaks = $src
                .char_indices()
                .map(|(i, c)| if c == '\n' { Some(i) } else { None })
                .filter(Option::is_some)
                .map(Option::unwrap)
                .collect::<Vec<_>>();

            let mut parser = loxide_parser::parser::Parser::new($src);
            let (stmts, _) = parser.parse();
            // setup env and start eval
            let mut env = Environment::global();
            register!();
            stmts
                .into_iter()
                .for_each(|stmt| match stmt.eval(&mut env) {
                    Ok(_) => {}
                    Err(err) => {
                        let report: Report = err.into();
                        reporter.push(report.with_source_code(source.clone()));
                    }
                });
            let mut insertion = footprints!()
                .into_iter()
                .map(|footprint| {
                    let end = footprint.span.expect("span info is missing").1;
                    let line = linebreaks
                        .iter()
                        .position(|&l| l > end)
                        .unwrap_or(linebreaks.len());
                    (footprint.id, line, footprint.content)
                })
                .collect::<Vec<_>>();
            // sort the insertion by line number first and then by id
            insertion.sort_by_key(|(id, line, _)| (*line, *id));

            let mut src = $src.lines().map(String::from).collect::<Vec<_>>();
            // insert in reverse order to avoid shifting line numbers
            for (_, line, content) in insertion.into_iter().rev() {
                src.insert(line + 1, format!("// => {}", content));
            }
            let msg = reporter.report_to_string();
            if !msg.is_empty() {
                src.push(format!(
                    "\n=== Error Section ===\n{}",
                    reporter.report_to_string()
                ));
            }
            src.join(LINEBREAK)
        }};
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
        insta::assert_snapshot!(annotated_eval_test!(src));
    });

    unittest!(valid_unary, |src| {
        insta::assert_snapshot!(annotated_eval_test!(src));
    });

    unittest!(invalid_unary, |src| {
        let results = src
            .split(LINEBREAK)
            .map(display_eval_error)
            .collect::<Vec<_>>()
            .join(LINEBREAK);
        insta::assert_snapshot!(results);
    });

    unittest!(invalid_binary, |src| {
        let results = src
            .split(LINEBREAK)
            .map(display_eval_error)
            .collect::<Vec<_>>()
            .join(LINEBREAK);
        insta::assert_snapshot!(results);
    });

    unittest!(valid_binary, |src| {
        insta::assert_snapshot!(annotated_eval_test!(src));
    });

    unittest!(variable, |src| {
        insta::assert_snapshot!(annotated_eval_test!(src));
    });

    unittest!(block_stmt, |src| {
        insta::assert_snapshot!(annotated_eval_test!(src));
    });

    unittest!(if_stmt, |src| {
        insta::assert_snapshot!(annotated_eval_test!(src));
    });

    unittest!(while_stmt, |src| {
        insta::assert_snapshot!(annotated_eval_test!(src));
    });

    unittest!(for_stmt, |src| {
        insta::assert_snapshot!(annotated_eval_test!(src));
    });

    unittest!(for_stmt_expect_bool, |src| {
        let result = display_eval_error(src);
        insta::assert_snapshot!(result);
    });

    unittest!(while_stmt_expect_bool, |src| {
        let result = display_eval_error(src);
        insta::assert_snapshot!(result);
    });

    unittest!(fn_call, |src| {
        insta::assert_snapshot!(annotated_eval_test!(src));
    });

    unittest!(bad_fn_call, |src| {
        let result = display_eval_error(src);
        insta::assert_snapshot!(result);
    });
}
