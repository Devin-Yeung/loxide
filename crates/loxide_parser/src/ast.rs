use crate::token::Token;

#[derive(Debug)]
pub struct Expr<'src> {
    pub kind: ExprKind<'src>,
}

#[derive(Debug)]
pub enum ExprKind<'src> {
    Literal(Literal<'src>),
    Unary(UnaryExpr<'src>),
    Binary(BinaryExpr<'src>),
    Grouped(Box<Expr<'src>>),
}

#[derive(Debug)]
pub enum Literal<'src> {
    String(std::borrow::Cow<'src, str>),
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
pub struct UnaryExpr<'src> {
    pub operator: UnaryOperator,
    pub expr: Box<Expr<'src>>,
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
pub struct BinaryExpr<'src> {
    pub lhs: Box<Expr<'src>>,
    pub rhs: Box<Expr<'src>>,
    pub operator: BinaryOperator,
}

#[derive(Debug)]
pub struct GroupedExpr<'src> {
    pub expr: Box<Expr<'src>>,
}

#[derive(Debug)]
pub struct Variable<'src> {
    pub name: Token<'src>,
}

#[derive(Debug)]
#[non_exhaustive]
pub enum Stmt<'src> {
    Expression(Expr<'src>),
    PrintStmt(Expr<'src>),
    VarDeclaration(Variable<'src>, Expr<'src>),
}
