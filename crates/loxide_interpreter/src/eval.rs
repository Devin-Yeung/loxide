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
use std::io::Write;

pub trait Evaluable {
    fn eval(&self, env: &mut Environment, stdout: &mut impl Write) -> Result<Value, RuntimeError>;
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
    fn eval(&self, env: &mut Environment, stdout: &mut impl Write) -> Result<Value, RuntimeError> {
        self.kind.eval(env, stdout)
    }
}

impl Evaluable for StmtKind {
    fn eval(&self, env: &mut Environment, stdout: &mut impl Write) -> Result<Value, RuntimeError> {
        match self {
            StmtKind::Expression(e) => e.eval(env, stdout),
            StmtKind::PrintStmt(e) => {
                let v = e.eval(env, stdout)?;
                let _ = probe!(&v, span => e.span().into());
                writeln!(stdout, "{v}").expect("Failed to write to stdout");
                Ok(Value::Void)
            }
            StmtKind::VarDeclaration(var, expr) => {
                let val = expr.eval(env, stdout)?;
                env.define(var, val);
                Ok(Value::Void)
            }
            StmtKind::FunDeclaration(decl) => {
                let callable = Callable::function(decl.clone(), env.extend());
                env.define(&decl.name, Value::Callable(callable));
                Ok(Value::Void)
            }
            StmtKind::Block(stmts) => {
                // TODO: find a way to test the correctness of this evaluation
                let mut scope = env.extend();
                for stmt in stmts {
                    stmt.eval(&mut scope, stdout)?;
                }
                Ok(Value::Void)
            }
            StmtKind::Condition(cond) => cond.eval(env, stdout),
            StmtKind::While(cond) => cond.eval(env, stdout),
            StmtKind::For(stmt) => stmt.eval(env, stdout),
            StmtKind::ReturnStmt(stmt) => stmt.eval(env, stdout),
            _ => unreachable!(),
        }
    }
}

impl Evaluable for ConditionStmt {
    fn eval(&self, env: &mut Environment, stdout: &mut impl Write) -> Result<Value, RuntimeError> {
        let mut env = env.extend();
        let guard = self.condition.eval(&mut env, stdout)?;
        match guard {
            Value::Boolean(b) => {
                if b {
                    self.then_branch.eval(&mut env, stdout)
                } else {
                    match &self.else_branch {
                        None => Ok(Value::Void),
                        Some(stmt) => stmt.eval(&mut env, stdout),
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
    fn eval(&self, env: &mut Environment, stdout: &mut impl Write) -> Result<Value, RuntimeError> {
        let cond = &self.condition;
        loop {
            let guard = cond.eval(env, stdout)?;
            match guard {
                Value::Boolean(b) => {
                    if b {
                        self.body.eval(env, stdout)?;
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
    fn eval(&self, env: &mut Environment, stdout: &mut impl Write) -> Result<Value, RuntimeError> {
        let mut env = env.extend();
        let _ = &self.initializer.eval(&mut env, stdout)?;
        match &self.condition {
            None => {
                // infinite loop
                loop {
                    self.body.eval(&mut env, stdout)?;
                    self.increment.eval(&mut env, stdout)?;
                }
            }
            Some(cond) => loop {
                let guard = cond.eval(&mut env, stdout)?;
                match guard {
                    Value::Boolean(b) => {
                        if b {
                            self.body.eval(&mut env, stdout)?;
                            self.increment.eval(&mut env, stdout)?;
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
    fn eval(&self, env: &mut Environment, stdout: &mut impl Write) -> Result<Value, RuntimeError> {
        self.value
            .as_ref()
            .map_or(Err(RuntimeError::ReturnValue(Value::Void)), |expr| {
                let value = expr.eval(env, stdout)?;
                Err(RuntimeError::ReturnValue(value))
            })
    }
}

impl Evaluable for Option<Box<Stmt>> {
    fn eval(&self, env: &mut Environment, stdout: &mut impl Write) -> Result<Value, RuntimeError> {
        if let Some(stmt) = self {
            stmt.eval(env, stdout)
        } else {
            Ok(Value::Void)
        }
    }
}

impl Evaluable for Option<Box<Expr>> {
    fn eval(&self, env: &mut Environment, stdout: &mut impl Write) -> Result<Value, RuntimeError> {
        let val = match self {
            None => Value::Nil,
            Some(e) => e.eval(env, stdout)?,
        };
        Ok(val)
    }
}

impl Evaluable for Option<Expr> {
    fn eval(&self, env: &mut Environment, stdout: &mut impl Write) -> Result<Value, RuntimeError> {
        let val = match self {
            None => Value::Nil,
            Some(e) => e.eval(env, stdout)?,
        };
        Ok(val)
    }
}

impl Evaluable for Expr {
    fn eval(&self, env: &mut Environment, stdout: &mut impl Write) -> Result<Value, RuntimeError> {
        match &self.kind {
            ExprKind::Literal(l) => l.eval(env, stdout),
            ExprKind::Unary(u) => u.eval(env, stdout),
            ExprKind::Binary(b) => b.eval(env, stdout),
            ExprKind::Grouped(g) => g.eval(env, stdout),
            ExprKind::Variable(v) => v.eval(env, stdout),
            ExprKind::Assign(a) => a.eval(env, stdout),
            ExprKind::Call(c) => c.eval(env, stdout),
            _ => unreachable!(),
        }
    }
}

impl Evaluable for CallExpr {
    fn eval(&self, env: &mut Environment, stdout: &mut impl Write) -> Result<Value, RuntimeError> {
        let callee = self.callee.eval(env, stdout)?;
        let args = self
            .args
            .iter()
            .map(|expr| expr.eval(env, stdout))
            .collect::<Result<Vec<Value>, RuntimeError>>()?;

        if let Value::Callable(callable) = callee {
            return match callable.call(args, env, stdout) {
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
    fn eval(&self, env: &mut Environment, stdout: &mut impl Write) -> Result<Value, RuntimeError> {
        let val = self.value.eval(env, stdout)?;
        env.mutate(&self.name, val.clone())?;
        Ok(val)
    }
}

impl Evaluable for BinaryExpr {
    fn eval(&self, env: &mut Environment, stdout: &mut impl Write) -> Result<Value, RuntimeError> {
        let lhs = self.lhs.eval(env, stdout)?;
        let rhs = self.rhs.eval(env, stdout)?;
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
    fn eval(&self, env: &mut Environment, stdout: &mut impl Write) -> Result<Value, RuntimeError> {
        let value = self.expr.eval(env, stdout)?;
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
    fn eval(&self, env: &mut Environment, stdout: &mut impl Write) -> Result<Value, RuntimeError> {
        self.expr.eval(env, stdout)
    }
}

impl Evaluable for Identifier {
    fn eval(&self, env: &mut Environment, _stdout: &mut impl Write) -> Result<Value, RuntimeError> {
        env.get(self)
    }
}

impl Evaluable for Literal {
    fn eval(
        &self,
        _env: &mut Environment,
        _stdout: &mut impl Write,
    ) -> Result<Value, RuntimeError> {
        let value = match *self {
            Literal::String(ref v) => Value::String(v.to_string()),
            Literal::Number(v) => Value::Number(v),
            Literal::Boolean(v) => Value::Boolean(v),
            Literal::Nil => Value::Nil,
        };
        Ok(value)
    }
}
