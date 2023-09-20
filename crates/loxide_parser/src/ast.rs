use std::rc::Rc;

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Literal {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Minus,
    Bang,
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub operator: UnaryOperator,
    pub expr: Box<Expr>,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub operator: BinaryOperator,
}

#[derive(Debug)]
pub struct GroupedExpr {
    pub expr: Box<Expr>,
}

#[derive(Debug)]
pub struct AssignExpr {
    pub name: Identifier,
    pub value: Box<Expr>,
}

#[derive(Debug)]
pub struct CallExpr {
    pub callee: Box<Expr>,
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

#[derive(Debug)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug)]
pub struct ConditionStmt {
    pub condition: Expr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
}

#[derive(Debug)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Box<Stmt>,
}

#[derive(Debug)]
pub struct ForStmt {
    pub initializer: Option<Box<Stmt>>,
    pub condition: Option<Box<Expr>>,
    pub increment: Option<Box<Expr>>,
    pub body: Box<Stmt>,
}

#[derive(Debug)]
pub struct FunDeclaration {
    pub name: Identifier,
    pub params: Vec<Identifier>,
    pub body: Vec<Stmt>,
}

#[derive(Debug)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
}

#[derive(Debug)]
#[non_exhaustive]
pub enum Stmt {
    Expression(Expr),
    PrintStmt(Expr),
    ReturnStmt(ReturnStmt),
    VarDeclaration(Identifier, Option<Expr>),
    FunDeclaration(Rc<FunDeclaration>),
    Block(Vec<Stmt>),
    Condition(ConditionStmt),
    While(WhileStmt),
    For(ForStmt),
}
