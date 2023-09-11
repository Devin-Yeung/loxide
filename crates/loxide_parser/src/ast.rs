#[derive(Debug)]
pub struct Expr<'src> {
    pub kind: ExprKind<'src>,
}

#[derive(Debug)]
#[non_exhaustive]
pub enum ExprKind<'src> {
    Literal(Literal<'src>),
    Unary(UnaryExpr<'src>),
    Binary(BinaryExpr<'src>),
    Grouped(Box<Expr<'src>>),
    Variable(Variable<'src>),
    Assign(AssignExpr<'src>),
    Call(CallExpr<'src>),
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
pub struct AssignExpr<'src> {
    pub name: Variable<'src>,
    pub value: Box<Expr<'src>>,
}

#[derive(Debug)]
pub struct CallExpr<'src> {
    pub callee: Box<Expr<'src>>,
    pub args: Vec<Expr<'src>>,
}

impl<'src> AssignExpr<'src> {
    pub fn new(name: &'src str, value: Expr<'src>) -> Self {
        AssignExpr {
            name: Variable { name },
            value: Box::new(value),
        }
    }
}

#[derive(Debug)]
pub struct Variable<'src> {
    pub name: &'src str,
}

#[derive(Debug)]
pub struct ConditionStmt<'src> {
    pub condition: Expr<'src>,
    pub then_branch: Box<Stmt<'src>>,
    pub else_branch: Option<Box<Stmt<'src>>>,
}

#[derive(Debug)]
pub struct WhileStmt<'src> {
    pub condition: Expr<'src>,
    pub body: Box<Stmt<'src>>,
}

#[derive(Debug)]
pub struct ForStmt<'src> {
    pub initializer: Option<Box<Stmt<'src>>>,
    pub condition: Option<Box<Expr<'src>>>,
    pub increment: Option<Box<Expr<'src>>>,
    pub body: Box<Stmt<'src>>,
}

#[derive(Debug)]
#[non_exhaustive]
pub enum Stmt<'src> {
    Expression(Expr<'src>),
    PrintStmt(Expr<'src>),
    VarDeclaration(Variable<'src>, Option<Expr<'src>>),
    Block(Vec<Stmt<'src>>),
    Condition(ConditionStmt<'src>),
    While(WhileStmt<'src>),
    For(ForStmt<'src>),
}
