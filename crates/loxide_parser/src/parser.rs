use crate::ast;
use crate::ast::ExprKind::Binary;
use crate::ast::Literal::{Boolean, Nil};
use crate::ast::{
    AssignExpr, BinaryExpr, CallExpr, ConditionStmt, Expr, ExprKind, ForStmt, FunDeclaration,
    Identifier, ReturnStmt, Stmt, UnaryExpr, WhileStmt,
};
use crate::error::SyntaxError;
use crate::scanner::Scanner;
use crate::token::{Keyword, Token, TokenType};
use std::iter::Peekable;

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
        match self.peek_type()? {
            found if found == ty => Ok(self.advance()?),
            found => Err(SyntaxError::UnexpectedToken {
                expected: ty.name(),
                found: found.name(),
            }),
        }
    }

    fn consume_if(&mut self, ty: TokenType) -> bool {
        if self.peek_type() == Ok(ty) {
            self.advance().unwrap();
            return true;
        }
        false
    }

    fn peek_type(&mut self) -> Result<TokenType, SyntaxError> {
        match self.tokens.peek() {
            None => Ok(TokenType::EOF),
            Some(Ok(token)) => Ok(token.ty.clone()),
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
        let name = Identifier {
            name: self.consume(TokenType::Identifier)?.lexeme.to_string(),
        };
        self.consume(TokenType::LeftParen)?;
        let params = match self.peek_type()? {
            TokenType::RightParen => Vec::new(),
            _ => self.parameters()?,
        };
        self.consume(TokenType::RightParen)?;
        let body = self.block()?;
        Ok(Stmt::FunDeclaration(FunDeclaration { name, params, body }))
    }

    /// parse parameters according to following rules:
    ///
    /// ```text
    /// parameters  → IDENTIFIER ( "," IDENTIFIER )* ;
    /// ```
    ///
    fn parameters(&mut self) -> Result<Vec<Identifier>, SyntaxError> {
        let mut idents = Vec::<Identifier>::new();

        idents.push(Identifier {
            name: self.consume(TokenType::Identifier)?.lexeme.to_string(),
        });

        while self.consume_if(TokenType::Comma) {
            idents.push(Identifier {
                name: self.consume(TokenType::Identifier)?.lexeme.to_string(),
            });
        }

        Ok(idents)
    }

    /// parse var declaration according to following rules:
    ///
    /// ```text
    /// varDecl  → "var" IDENTIFIER ( "=" expression )? ";" ;
    /// ```
    fn var_declaration(&mut self) -> Result<Stmt, SyntaxError> {
        self.consume(TokenType::Keyword(Keyword::Var))?;
        let name = self.consume(TokenType::Identifier)?.lexeme.to_string();
        let mut expr: Option<Expr> = None;
        if self.peek_type() == Ok(TokenType::Equal) {
            self.consume(TokenType::Equal)?;
            expr = Some(self.expression()?);
        }
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::VarDeclaration(Identifier { name }, expr))
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
        self.consume(TokenType::Keyword(Keyword::If))?;
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
        Ok(Stmt::Condition(ConditionStmt {
            condition,
            then_branch,
            else_branch,
        }))
    }

    /// parse while statement according to following rules:
    /// ```text
    /// whileStmt  → "while" "(" expression ")" statement ;
    /// ```
    fn while_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        self.consume(TokenType::Keyword(Keyword::While))?;
        self.consume(TokenType::LeftParen)?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen)?;
        let body = Box::new(self.statement()?);
        Ok(Stmt::While(WhileStmt { condition, body }))
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
        self.consume(TokenType::Keyword(Keyword::For))?;
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
        Ok(Stmt::For(ForStmt {
            initializer,
            condition,
            increment,
            body,
        }))
    }

    /// parse block statement according to following rules:
    /// ```text
    /// block  → "{" declaration* "}" ;
    /// ```
    fn block_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        Ok(Stmt::Block(self.block()?))
    }

    /// helper method for parsing  a block statement
    /// ```text
    /// block  → "{" declaration* "}" ;
    /// ```
    fn block(&mut self) -> Result<Vec<Stmt>, SyntaxError> {
        self.consume(TokenType::LeftBrace)?;
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
        self.consume(TokenType::RightBrace)?;
        Ok(stmts)
    }

    /// parse print statement according to following rules:
    ///
    /// ```text
    /// printStmt  → "print" expression ";" ;
    /// ```
    fn print_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        self.consume(TokenType::Keyword(Keyword::Print))?;
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::PrintStmt(expr))
    }

    /// parse expression statement according to following rules:
    ///
    /// ```text
    /// exprStmt  → expression ";" ;
    /// ```
    fn expression_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::Expression(expr))
    }

    /// parse return statement according to following rules:
    ///
    /// ```text
    /// returnStmt  → "return" expression? ";" ;
    /// ```
    fn return_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        self.consume(TokenType::Keyword(Keyword::Return))?;
        let value = match self.peek_type()? {
            TokenType::Semicolon => None,
            _ => Some(self.expression()?),
        };
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::ReturnStmt(ReturnStmt { value }))
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
            return match expr.kind {
                ExprKind::Variable(v) => Ok(Expr {
                    kind: ExprKind::Assign(AssignExpr::new(&v.name, value)),
                }),
                _ => Err(SyntaxError::InvalidAssignmentTarget),
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
            expr = Expr {
                kind: Binary(BinaryExpr {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    operator: ops,
                }),
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
            expr = Expr {
                kind: Binary(BinaryExpr {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    operator: ops,
                }),
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
            expr = Expr {
                kind: Binary(BinaryExpr {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    operator: ops,
                }),
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
            expr = Expr {
                kind: Binary(BinaryExpr {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    operator: ops,
                }),
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
                let operator = self.advance()?.try_into()?;
                let expr = Box::new(self.unary()?);
                Expr {
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
            if self.consume_if(TokenType::LeftParen) {
                let args = self.arguments()?;
                expr = Expr {
                    kind: ExprKind::Call(CallExpr {
                        callee: Box::new(expr),
                        args,
                    }),
                };
                self.consume(TokenType::RightParen)?;
            } else {
                break;
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
        let expr = match self.peek_type()? {
            TokenType::Keyword(Keyword::True) => {
                self.advance()?;
                Expr {
                    kind: ExprKind::Literal(Boolean(true)),
                }
            }
            TokenType::Keyword(Keyword::False) => {
                self.advance()?;
                Expr {
                    kind: ExprKind::Literal(Boolean(false)),
                }
            }
            TokenType::Keyword(Keyword::Nil) => {
                self.advance()?;
                Expr {
                    kind: ExprKind::Literal(Nil),
                }
            }
            TokenType::Literal(_) => {
                let literal: ast::Literal = self.advance()?.try_into()?;
                Expr {
                    kind: ExprKind::Literal(literal),
                }
            }
            TokenType::LeftParen => {
                self.consume(TokenType::LeftParen)?;
                let expr = self.expression()?;
                self.consume(TokenType::RightParen)?;
                Expr {
                    kind: ExprKind::Grouped(Box::new(expr)),
                }
            }
            TokenType::Identifier => {
                let name = self.consume(TokenType::Identifier)?.lexeme;
                Expr {
                    kind: ExprKind::Variable(Identifier {
                        name: name.to_string(),
                    }),
                }
            }
            _ => return Err(SyntaxError::Expect("expression")),
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
    use loxide_testsuite::unittest;

    unittest!(primary, |src| {
        let asts = src
            .split('\n')
            .map(|line| Parser::new(line).primary())
            .collect::<Vec<_>>();
        insta::assert_debug_snapshot!(asts);
    });

    unittest!(unary, |src| {
        let asts = src
            .split('\n')
            .map(|line| Parser::new(line).unary())
            .collect::<Vec<_>>();
        insta::assert_debug_snapshot!(asts);
    });

    unittest!(factor, |src| {
        let asts = src
            .split('\n')
            .map(|line| Parser::new(line).factor())
            .collect::<Vec<_>>();
        insta::assert_debug_snapshot!(asts);
    });

    unittest!(term, |src| {
        let asts = src
            .split('\n')
            .map(|line| Parser::new(line).term())
            .collect::<Vec<_>>();
        insta::assert_debug_snapshot!(asts);
    });

    unittest!(comparison, |src| {
        let asts = src
            .split('\n')
            .map(|line| Parser::new(line).comparison())
            .collect::<Vec<_>>();
        insta::assert_debug_snapshot!(asts);
    });

    unittest!(equality, |src| {
        let asts = src
            .split('\n')
            .map(|line| Parser::new(line).equality())
            .collect::<Vec<_>>();
        insta::assert_debug_snapshot!(asts);
    });

    unittest!(expression, |src| {
        let asts = src
            .split('\n')
            .map(|line| Parser::new(line).expression())
            .collect::<Vec<_>>();
        insta::assert_debug_snapshot!(asts);
    });

    unittest!(single_statement, |src| {
        let asts = src
            .split('\n')
            .map(|line| Parser::new(line).statement())
            .collect::<Vec<_>>();
        insta::assert_debug_snapshot!(asts);
    });

    unittest!(many_statements, |src| {
        let mut parser = Parser::new(src);
        let results = parser.parse();
        insta::assert_debug_snapshot!(results);
    });

    unittest!(declarations, |src| {
        let mut parser = Parser::new(src);
        let results = parser.parse();
        insta::assert_debug_snapshot!(results);
    });

    unittest!(block_statement, |src| {
        let mut parser = Parser::new(src);
        let results = parser.parse();
        insta::assert_debug_snapshot!(results);
    });

    unittest!(if_statement, |src| {
        let mut parser = Parser::new(src);
        let results = parser.parse();
        insta::assert_debug_snapshot!(results);
    });

    unittest!(while_statement, |src| {
        let mut parser = Parser::new(src);
        let results = parser.parse();
        insta::assert_debug_snapshot!(results);
    });

    unittest!(for_statement, |src| {
        let mut parser = Parser::new(src);
        let results = parser.parse();
        insta::assert_debug_snapshot!(results);
    });

    unittest!(fn_call, |src| {
        let mut parser = Parser::new(src);
        let results = parser.parse();
        insta::assert_debug_snapshot!(results);
    });

    unittest!(fn_decl, |src| {
        let mut parser = Parser::new(src);
        let results = parser.parse();
        insta::assert_debug_snapshot!(results);
    });

    unittest!(return_stmt, |src| {
        let mut parser = Parser::new(src);
        let results = parser.parse();
        insta::assert_debug_snapshot!(results);
    });
}
