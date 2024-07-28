use crate::token::Span;
use std::sync::Arc;

#[derive(Debug, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub(crate) span: Span,
}

impl Expr {
    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, PartialEq)]
#[non_exhaustive]
pub enum ExprKind {
    Literal(Literal),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Grouped(Box<Expr>),
    Variable(Identifier),
    Assign(AssignExpr),
    Call(CallExpr),
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil,
}

#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOperator {
    Minus,
    Bang,
}

#[derive(Debug, PartialEq)]
pub struct UnaryExpr {
    pub operator: UnaryOperator,
    pub expr: Box<Expr>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum BinaryOperator {
    EqualEqual,
    BangEq,
    GreaterEqual,
    Greater,
    Less,
    LessEqual,
    Minus,
    Plus,
    Slash,
    Star,
}

#[derive(Debug, PartialEq)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub operator: BinaryOperator,
}

#[derive(Debug)]
pub struct GroupedExpr {
    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct AssignExpr {
    pub name: Identifier,
    pub value: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    /// Parentheses token of the arguments
    pub paren_token: Span,
    pub args: Vec<Expr>,
}

impl AssignExpr {
    pub fn new(name: &str, value: Expr) -> Self {
        AssignExpr {
            name: Identifier {
                name: name.to_string(),
            },
            value: Box::new(value),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub struct ConditionStmt {
    pub condition: Expr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
}

#[derive(Debug, PartialEq)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Box<Stmt>,
}

#[derive(Debug, PartialEq)]
pub struct ForStmt {
    pub initializer: Option<Box<Stmt>>,
    pub condition: Option<Box<Expr>>,
    pub increment: Option<Box<Expr>>,
    pub body: Box<Stmt>,
}

#[derive(Debug, PartialEq)]
pub struct FunDeclaration {
    pub name: Identifier,
    /// Parentheses token of the params
    pub paren_token: Span,
    pub params: Vec<Identifier>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, PartialEq)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
}

#[derive(Debug, PartialEq)]
#[non_exhaustive]
pub enum Stmt {
    Expression(Expr),
    PrintStmt(Expr),
    ReturnStmt(ReturnStmt),
    VarDeclaration(Identifier, Option<Expr>),
    FunDeclaration(Arc<FunDeclaration>),
    Block(Vec<Stmt>),
    Condition(ConditionStmt),
    While(WhileStmt),
    For(ForStmt),
}
