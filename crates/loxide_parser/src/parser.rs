use crate::ast::ExprKind::Binary;
use crate::ast::Literal::{Boolean, Nil};
use crate::ast::{
    AssignExpr, BinaryExpr, CallExpr, ConditionStmt, Expr, ExprKind, ForStmt, FunDeclaration,
    Identifier, ReturnStmt, Stmt, StmtKind, UnaryExpr, UnaryOperator, WhileStmt,
};
use crate::error::SyntaxError;
use crate::scanner::Scanner;
use crate::token::{Keyword, Span, Token, TokenType};
use std::iter::Peekable;
use std::sync::Arc;

pub struct Parser<'src> {
    tokens: Peekable<Scanner<'src>>,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> Parser {
        Parser {
            tokens: Scanner::new(src).peekable(),
        }
    }

    pub fn parse(&mut self) -> (Vec<Stmt>, Vec<SyntaxError>) {
        let mut statements = Vec::<Stmt>::new();
        let mut errors = Vec::<SyntaxError>::new();
        loop {
            match self.peek_type() {
                Ok(TokenType::EOF) => break,
                Ok(_) => match self.declaration() {
                    Ok(stmt) => {
                        statements.push(stmt);
                        while self.consume_if(TokenType::Comment) {
                            continue;
                        }
                    }
                    Err(err) => {
                        errors.push(err);
                        self.synchronize();
                    }
                },
                Err(err) => errors.push(err),
            }
        }
        (statements, errors)
    }

    fn consume(&mut self, ty: TokenType) -> Result<Token, SyntaxError> {
        let token = self.peek_token()?;
        if token.ty == ty {
            Ok(self.advance()?)
        } else {
            Err(SyntaxError::UnexpectedToken {
                span: token.span,
                expected: ty.name(),
                found: token.ty.name(),
            })
        }
    }

    fn consume_identifier(&mut self) -> Result<Identifier, SyntaxError> {
        let token = self.consume(TokenType::Identifier)?;
        Ok(Identifier {
            name: token.lexeme.to_string(),
            span: token.span,
        })
    }

    fn consume_if(&mut self, ty: TokenType) -> bool {
        if self.peek_type() == Ok(ty) {
            self.advance().unwrap();
            return true;
        }
        false
    }

    fn peek_type(&mut self) -> Result<TokenType, SyntaxError> {
        self.skip_comments();
        match self.tokens.peek() {
            None => Ok(TokenType::EOF),
            Some(Ok(token)) => Ok(token.ty.clone()),
            Some(Err(err)) => Err(*err),
        }
    }

    fn skip_comments(&mut self) {
        match self.tokens.peek() {
            Some(Ok(token)) if token.ty == TokenType::Comment => {
                let _ = self.advance(); // drop it
                self.skip_comments();
            }
            _ => { /* Do nothing */ }
        }
    }

    fn peek_token(&mut self) -> Result<&Token, SyntaxError> {
        self.skip_comments();
        match self.tokens.peek() {
            None => Err(SyntaxError::UnexpectedEOF),
            Some(Ok(token)) => Ok(token), // never be a comment
            Some(Err(err)) => Err(*err),
        }
    }

    fn advance(&mut self) -> Result<Token, SyntaxError> {
        self.tokens
            .next()
            .unwrap_or(Err(SyntaxError::UnexpectedEOF))
    }

    /// parse declaration according to following rules:
    ///
    /// ```text
    /// declaration  → funDecl
    ///              | varDecl
    ///              | statement ;
    /// ```
    fn declaration(&mut self) -> Result<Stmt, SyntaxError> {
        match self.peek_type()? {
            TokenType::Keyword(Keyword::Var) => self.var_declaration(),
            TokenType::Keyword(Keyword::Fun) => self.func_declaration(),
            _ => self.statement(),
        }
    }

    /// parse function declaration according to following rules:
    ///
    /// ```text
    /// funDecl  → "fun" function ;
    /// ```
    fn func_declaration(&mut self) -> Result<Stmt, SyntaxError> {
        self.consume(TokenType::Keyword(Keyword::Fun))?;
        self.function()
    }

    /// parse function body according to following rules:
    ///
    /// ```text
    /// function  → IDENTIFIER "(" parameters? ")" block ;
    /// ```
    fn function(&mut self) -> Result<Stmt, SyntaxError> {
        let name = self.consume_identifier()?;
        let left_paren = self.consume(TokenType::LeftParen)?.span;
        let params = match self.peek_type()? {
            TokenType::RightParen => Vec::new(),
            _ => self.parameters()?,
        };
        let right_parent = self.consume(TokenType::RightParen)?.span;
        let (body, body_span) = self.spanned_block()?;

        Ok(Stmt {
            span: Span::new(name.span().start, body_span.end),
            kind: StmtKind::FunDeclaration(Arc::new(FunDeclaration {
                name,
                paren_token: Span::new(left_paren.start, right_parent.end),
                params,
                body,
            })),
        })
    }

    /// parse parameters according to following rules:
    ///
    /// ```text
    /// parameters  → IDENTIFIER ( "," IDENTIFIER )* ;
    /// ```
    ///
    fn parameters(&mut self) -> Result<Vec<Identifier>, SyntaxError> {
        let mut idents = Vec::<Identifier>::new();

        idents.push(self.consume_identifier()?);

        while self.consume_if(TokenType::Comma) {
            idents.push(self.consume_identifier()?);
        }

        Ok(idents)
    }

    /// parse var declaration according to following rules:
    ///
    /// ```text
    /// varDecl  → "var" IDENTIFIER ( "=" expression )? ";" ;
    /// ```
    fn var_declaration(&mut self) -> Result<Stmt, SyntaxError> {
        let var = self.consume(TokenType::Keyword(Keyword::Var))?.span;
        let name = self.consume_identifier()?;
        let mut expr: Option<Expr> = None;
        if self.peek_type() == Ok(TokenType::Equal) {
            self.consume(TokenType::Equal)?;
            expr = Some(self.expression()?);
        }
        let semi = self.consume(TokenType::Semicolon)?.span;

        let span = Span::new(var.start, semi.end);
        let kind = StmtKind::VarDeclaration(name, expr);
        Ok(Stmt { kind, span })
    }

    /// parse statement according to following rules:
    ///
    /// ```text
    /// statement  → exprStmt
    ///            | printStmt
    ///            | returnStmt
    ///            | ifStmt
    ///            | whileStmt
    ///            | forStmt
    ///            | block ;
    /// ```
    fn statement(&mut self) -> Result<Stmt, SyntaxError> {
        match self.peek_type()? {
            TokenType::Keyword(Keyword::Print) => self.print_stmt(),
            TokenType::LeftBrace => self.block_stmt(),
            TokenType::Keyword(Keyword::If) => self.if_stmt(),
            TokenType::Keyword(Keyword::While) => self.while_stmt(),
            TokenType::Keyword(Keyword::For) => self.for_stmt(),
            TokenType::Keyword(Keyword::Return) => self.return_stmt(),
            _ => self.expression_stmt(),
        }
    }

    /// parse if statement according to following rules:
    /// ```text
    /// ifStmt  → "if" "(" expression ")" statement
    ///           ( "else" statement )? ;
    /// ```
    ///
    /// note: the else is bound to the nearest if that precedes it
    /// for the following dangling else case
    /// ```text
    /// if (first)
    /// |   if (second)
    /// |      when_true();
    /// else
    ///     when_false();
    /// ```
    /// the actual semantic is:
    /// ```text
    /// if (first)
    ///     if (second)
    ///     |   when_true();
    ///     else
    ///         when_false();
    /// ```
    fn if_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        let kw = self.consume(TokenType::Keyword(Keyword::If))?.span;
        self.consume(TokenType::LeftParen)?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen)?;
        let then_branch = Box::new(self.statement()?);
        // else statement if optional
        let else_branch = if self.consume_if(TokenType::Keyword(Keyword::Else)) {
            // the else is bound to the nearest if that precedes it
            Some(Box::new(self.statement()?))
        } else {
            None
        };
        // track the span
        let span = Span::new(
            kw.start,
            else_branch
                .as_ref()
                .map_or_else(|| then_branch.span.end, |stmt| stmt.span.end),
        );
        let kind = StmtKind::Condition(ConditionStmt {
            condition,
            then_branch,
            else_branch,
        });
        Ok(Stmt { kind, span })
    }

    /// parse while statement according to following rules:
    /// ```text
    /// whileStmt  → "while" "(" expression ")" statement ;
    /// ```
    fn while_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        let kw = self.consume(TokenType::Keyword(Keyword::While))?.span;
        self.consume(TokenType::LeftParen)?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen)?;
        let body = Box::new(self.statement()?);

        let span = Span::new(kw.start, body.span.end);
        let kind = StmtKind::While(WhileStmt { condition, body });
        Ok(Stmt { kind, span })
    }

    /// parse for statement according to following rules:
    /// ```text
    /// forStmt  → "for"
    ///          "("
    ///             ( varDecl | exprStmt | ";" )
    ///               expression? ";"
    ///               expression?
    ///          ")"
    ///          statement ;
    /// ```
    fn for_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        let kw = self.consume(TokenType::Keyword(Keyword::For))?.span;
        self.consume(TokenType::LeftParen)?;
        let initializer = match self.peek_type()? {
            TokenType::Semicolon => {
                self.consume(TokenType::Semicolon)?;
                None
            }
            _ => Some(Box::new(self.declaration()?)),
        };
        let condition = match self.peek_type()? {
            TokenType::Semicolon => None,
            _ => Some(Box::new(self.expression()?)),
        };
        self.consume(TokenType::Semicolon)?;
        let increment = match self.peek_type()? {
            TokenType::RightParen => None,
            _ => Some(Box::new(self.expression()?)),
        };
        self.consume(TokenType::RightParen)?;
        let body = Box::new(self.statement()?);
        // track the span
        let span = Span::new(kw.start, body.span.end);
        let kind = StmtKind::For(ForStmt {
            initializer,
            condition,
            increment,
            body,
        });

        Ok(Stmt { kind, span })
    }

    /// parse block statement according to following rules:
    /// ```text
    /// block  → "{" declaration* "}" ;
    /// ```
    fn block_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        let (stmts, span) = self.spanned_block()?;
        Ok(Stmt {
            kind: StmtKind::Block(stmts),
            span,
        })
    }

    fn block(&mut self) -> Result<Vec<Stmt>, SyntaxError> {
        let (stmts, _) = self.spanned_block()?;
        Ok(stmts)
    }

    /// helper method for parsing  a block statement
    /// ```text
    /// block  → "{" declaration* "}" ;
    /// ```
    fn spanned_block(&mut self) -> Result<(Vec<Stmt>, Span), SyntaxError> {
        let lbrace = self.consume(TokenType::LeftBrace)?.span;
        let mut stmts: Vec<Stmt> = Vec::new();
        loop {
            match self.peek_type()? {
                TokenType::RightBrace => break,
                _ => {
                    stmts.push(self.declaration()?);
                    while self.consume_if(TokenType::Comment) {
                        continue;
                    }
                }
            }
        }
        let rbrace = self.consume(TokenType::RightBrace)?.span;
        let span = Span::new(lbrace.start, rbrace.end);
        Ok((stmts, span))
    }

    /// parse print statement according to following rules:
    ///
    /// ```text
    /// printStmt  → "print" expression ";" ;
    /// ```
    fn print_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        self.consume(TokenType::Keyword(Keyword::Print))?;
        let expr = self.expression()?;
        let semi = self.consume(TokenType::Semicolon)?.span;
        Ok(Stmt {
            span: Span::new(expr.span.start, semi.end),
            kind: StmtKind::PrintStmt(expr),
        })
    }

    /// parse expression statement according to following rules:
    ///
    /// ```text
    /// exprStmt  → expression ";" ;
    /// ```
    fn expression_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        let expr = self.expression()?;
        let semi = self.consume(TokenType::Semicolon)?.span;
        Ok(Stmt {
            span: Span::new(expr.span.start, semi.end),
            kind: StmtKind::Expression(expr),
        })
    }

    /// parse return statement according to following rules:
    ///
    /// ```text
    /// returnStmt  → "return" expression? ";" ;
    /// ```
    fn return_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        let kw = self.consume(TokenType::Keyword(Keyword::Return))?.span;
        let value = match self.peek_type()? {
            TokenType::Semicolon => None,
            _ => Some(self.expression()?),
        };
        let semi = self.consume(TokenType::Semicolon)?.span;

        Ok(Stmt {
            span: Span::new(kw.start, semi.end),
            kind: StmtKind::ReturnStmt(ReturnStmt { value }),
        })
    }

    /// parse expression according to following rules:
    ///
    /// ```text
    /// expression  → assignment ;
    /// ```
    fn expression(&mut self) -> Result<Expr, SyntaxError> {
        self.assignment()
    }

    /// assignment  → IDENTIFIER "=" assignment
    ///             | equality ;
    fn assignment(&mut self) -> Result<Expr, SyntaxError> {
        let expr = self.equality()?;

        if self.consume_if(TokenType::Equal) {
            let value = self.assignment()?;
            let span = Span::new(expr.span.start, value.span.end);
            return match expr.kind {
                ExprKind::Variable(v) => Ok(Expr {
                    kind: ExprKind::Assign(AssignExpr::new(v, value)),
                    span,
                }),
                _ => Err(SyntaxError::InvalidAssignmentTarget(expr.span())),
            };
        }
        Ok(expr)
    }

    /// parse equality expression according to following rules:
    ///
    /// ```text
    /// equality  → comparison ( ( "!=" | "==" ) comparison )* ;
    /// ```
    fn equality(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.comparison()?;
        loop {
            let ops = match self.peek_type()? {
                TokenType::EqualEqual | TokenType::BangEqual => self.advance()?.try_into()?,
                _ => break,
            };
            let rhs: Expr = self.comparison()?;
            let span = Span::new(expr.span.start, rhs.span.end);
            expr = Expr {
                kind: Binary(BinaryExpr {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    operator: ops,
                }),
                span,
            }
        }
        Ok(expr)
    }

    /// parse comparison expression according to following rules:
    ///
    /// ```text
    /// comparison  → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    /// ```
    fn comparison(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.term()?;
        loop {
            let ops = match self.peek_type()? {
                TokenType::Greater
                | TokenType::GreaterEqual
                | TokenType::Less
                | TokenType::LessEqual => self.advance()?.try_into()?,
                _ => break,
            };
            let rhs = self.term()?;
            let span = Span::new(expr.span.start, rhs.span.end);
            expr = Expr {
                kind: Binary(BinaryExpr {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    operator: ops,
                }),
                span,
            }
        }
        Ok(expr)
    }

    /// parse term expression according to following rules:
    ///
    /// ```text
    /// term  → factor ( ( "-" | "+" ) factor )* ;
    /// ```
    fn term(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.factor()?;
        loop {
            let ops = match self.peek_type()? {
                TokenType::Minus | TokenType::Plus => self.advance()?.try_into()?,
                _ => break,
            };
            let rhs = self.factor()?;
            let span = Span::new(expr.span.start, rhs.span.end);
            expr = Expr {
                kind: Binary(BinaryExpr {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    operator: ops,
                }),
                span,
            }
        }
        Ok(expr)
    }

    /// parse factor expression according to following rules:
    ///
    /// ```text
    /// factor  → unary ( ( "/" | "*" ) unary )* ;
    /// ```
    fn factor(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.unary()?;
        loop {
            let ops = match self.peek_type()? {
                TokenType::Slash | TokenType::Star => self.advance()?.try_into()?,
                _ => break,
            };
            let rhs = self.unary()?;
            let span = Span::new(expr.span.start, rhs.span.end);
            expr = Expr {
                kind: Binary(BinaryExpr {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    operator: ops,
                }),
                span,
            }
        }
        Ok(expr)
    }

    /// parse unary expression according to following rules:
    ///
    /// ```text
    /// unary  → ( "!" | "-" ) unary
    ///        | call ;
    /// ```
    fn unary(&mut self) -> Result<Expr, SyntaxError> {
        let expr = match self.peek_type()? {
            TokenType::Bang | TokenType::Minus => {
                let operator = self.advance()?;
                let start = operator.span.start;
                let operator: UnaryOperator = operator.try_into()?;
                let expr = Box::new(self.unary()?);

                Expr {
                    span: Span::new(start, expr.span.end),
                    kind: ExprKind::Unary(UnaryExpr { operator, expr }),
                }
            }
            _ => self.call()?,
        };
        Ok(expr)
    }

    /// parse unary expression according to following rules:
    ///
    /// ```text
    /// call  → primary ( "(" arguments? ")" )* ;
    /// ```
    fn call(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.primary()?;
        loop {
            match self.peek_type() {
                Ok(TokenType::LeftParen) => {
                    let lparen = self.consume(TokenType::LeftParen)?.span.start;
                    let args = self.arguments()?;
                    let rparen = self.consume(TokenType::RightParen)?.span.end;
                    expr = Expr {
                        span: Span::new(expr.span.start, rparen),
                        kind: ExprKind::Call(CallExpr {
                            callee: Box::new(expr),
                            paren_token: Span::new(lparen, rparen),
                            args,
                        }),
                    }
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    /// parse arguments expression according to following rules:
    ///
    /// ```text
    /// arguments  → expression ( "," expression )* ;
    /// ```
    fn arguments(&mut self) -> Result<Vec<Expr>, SyntaxError> {
        let mut args = Vec::new();
        loop {
            match self.peek_type()? {
                TokenType::RightParen => break,
                TokenType::Comma => {
                    self.consume(TokenType::Comma)?;
                }
                _ => args.push(self.expression()?),
            }
        }
        Ok(args)
    }

    /// parse primary expression according to following rules:
    ///
    /// ```text
    /// primary  → NUMBER | STRING | "true" | "false" | "nil"
    ///          | "(" expression ")"
    ///          | IDENTIFIER ;
    /// ```
    fn primary(&mut self) -> Result<Expr, SyntaxError> {
        let token = self.peek_token()?;
        let expr = match &token.ty {
            TokenType::Keyword(Keyword::True) => {
                let span = self.advance()?.span;
                Expr {
                    kind: ExprKind::Literal(Boolean(true)),
                    span,
                }
            }
            TokenType::Keyword(Keyword::False) => {
                let span = self.advance()?.span;
                Expr {
                    kind: ExprKind::Literal(Boolean(false)),
                    span,
                }
            }
            TokenType::Keyword(Keyword::Nil) => {
                let span = self.advance()?.span;
                Expr {
                    kind: ExprKind::Literal(Nil),
                    span,
                }
            }
            TokenType::Literal(_) => {
                let literal = self.advance()?;
                let span = literal.span;
                Expr {
                    kind: ExprKind::Literal(literal.try_into()?),
                    span,
                }
            }
            TokenType::LeftParen => {
                let start = self.consume(TokenType::LeftParen)?.span.start;
                let expr = self.expression()?;
                let end = self.consume(TokenType::RightParen)?.span.end;
                Expr {
                    kind: ExprKind::Grouped(Box::new(expr)),
                    span: Span::new(start, end),
                }
            }
            TokenType::Identifier => {
                let name = self.consume_identifier()?;
                Expr {
                    span: name.span(),
                    kind: ExprKind::Variable(name),
                }
            }
            ty => {
                return Err(SyntaxError::UnexpectedToken {
                    span: token.span,
                    expected: "primary expression",
                    found: ty.name(),
                })
            }
        };
        Ok(expr)
    }

    fn synchronize(&mut self) {
        loop {
            match self.peek_type() {
                Ok(ty) => match ty {
                    TokenType::Semicolon => {
                        self.advance().unwrap();
                        while self.consume_if(TokenType::Comment) {
                            continue; // remove comment if necessary, exhaustively
                        }
                        return;
                    }
                    TokenType::Keyword(Keyword::Class)
                    | TokenType::Keyword(Keyword::Fun)
                    | TokenType::Keyword(Keyword::Var)
                    | TokenType::Keyword(Keyword::For)
                    | TokenType::Keyword(Keyword::If)
                    | TokenType::Keyword(Keyword::While)
                    | TokenType::Keyword(Keyword::Print)
                    | TokenType::Keyword(Keyword::Return)
                    | TokenType::EOF => return,
                    _ => {
                        self.advance().unwrap();
                    }
                },
                _ => {
                    unreachable!()
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Parser;
    use crate::utils::test_utils::SPAN_FILTER;
    use loxide_testsuite::unittest;

    unittest!(primary, filters => vec![SPAN_FILTER], |src| {
        let asts = src
            .split('\n')
            .map(|line| Parser::new(line).primary())
            .collect::<Vec<_>>();
        insta::assert_debug_snapshot!(asts);
    });

    unittest!(unary, filters => vec![SPAN_FILTER], |src| {
        let asts = src
            .split('\n')
            .map(|line| Parser::new(line).unary())
            .collect::<Vec<_>>();
        insta::assert_debug_snapshot!(asts);
    });

    unittest!(factor, filters => vec![SPAN_FILTER], |src| {
        let asts = src
            .split('\n')
            .map(|line| Parser::new(line).factor())
            .collect::<Vec<_>>();
        insta::assert_debug_snapshot!(asts);
    });

    unittest!(term, filters => vec![SPAN_FILTER], |src| {
        let asts = src
            .split('\n')
            .map(|line| Parser::new(line).term())
            .collect::<Vec<_>>();
        insta::assert_debug_snapshot!(asts);
    });

    unittest!(comparison, filters => vec![SPAN_FILTER], |src| {
        let asts = src
            .split('\n')
            .map(|line| Parser::new(line).comparison())
            .collect::<Vec<_>>();
        insta::assert_debug_snapshot!(asts);
    });

    unittest!(equality, filters => vec![SPAN_FILTER], |src| {
        let asts = src
            .split('\n')
            .map(|line| Parser::new(line).equality())
            .collect::<Vec<_>>();
        insta::assert_debug_snapshot!(asts);
    });

    unittest!(expression, filters => vec![SPAN_FILTER], |src| {
        let asts = src
            .split('\n')
            .map(|line| Parser::new(line).expression())
            .collect::<Vec<_>>();
        insta::assert_debug_snapshot!(asts);
    });

    unittest!(single_statement, filters => vec![SPAN_FILTER], |src| {
        let asts = src
            .split('\n')
            .map(|line| Parser::new(line).statement())
            .collect::<Vec<_>>();
        insta::assert_debug_snapshot!(asts);
    });

    unittest!(many_statements, filters => vec![SPAN_FILTER], |src| {
        let mut parser = Parser::new(src);
        let results = parser.parse();
        insta::assert_debug_snapshot!(results);
    });

    unittest!(declarations, filters => vec![SPAN_FILTER], |src| {
        let mut parser = Parser::new(src);
        let results = parser.parse();
        insta::assert_debug_snapshot!(results);
    });

    unittest!(block_statement, filters => vec![SPAN_FILTER], |src| {
        let mut parser = Parser::new(src);
        let results = parser.parse();
        insta::assert_debug_snapshot!(results);
    });

    unittest!(if_statement, filters => vec![SPAN_FILTER], |src| {
        let mut parser = Parser::new(src);
        let results = parser.parse();
        insta::assert_debug_snapshot!(results);
    });

    unittest!(while_statement, filters => vec![SPAN_FILTER], |src| {
        let mut parser = Parser::new(src);
        let results = parser.parse();
        insta::assert_debug_snapshot!(results);
    });

    unittest!(for_statement, filters => vec![SPAN_FILTER], |src| {
        let mut parser = Parser::new(src);
        let results = parser.parse();
        insta::assert_debug_snapshot!(results);
    });

    unittest!(fn_call, filters => vec![SPAN_FILTER], |src| {
        let mut parser = Parser::new(src);
        let results = parser.parse();
        insta::assert_debug_snapshot!(results);
    });

    unittest!(fn_decl, filters => vec![SPAN_FILTER], |src| {
        let mut parser = Parser::new(src);
        let results = parser.parse();
        insta::assert_debug_snapshot!(results);
    });

    unittest!(return_stmt, filters => vec![SPAN_FILTER], |src| {
        let mut parser = Parser::new(src);
        let results = parser.parse();
        insta::assert_debug_snapshot!(results);
    });
}
