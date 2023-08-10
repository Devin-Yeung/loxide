// expression     → equality ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ")" ;

use crate::ast;
use crate::ast::ExprKind::Binary;
use crate::ast::Literal::{Boolean, Nil};
use crate::ast::{BinaryExpr, Expr, ExprKind, Stmt, UnaryExpr};
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
                Ok(_) => match self.statement() {
                    Ok(stmt) => statements.push(stmt),
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

    fn consume(&mut self, ty: TokenType) -> Result<(), SyntaxError> {
        return if self.peek_type()? == ty {
            self.advance()?;
            Ok(())
        } else {
            todo!()
        };
    }

    fn consume_if(&mut self, ty: TokenType) {
        if self.peek_type() == Ok(ty) {
            self.advance().unwrap();
        }
    }

    fn peek_type(&mut self) -> Result<TokenType<'src>, SyntaxError> {
        match self.tokens.peek() {
            None => Ok(TokenType::EOF),
            Some(&Ok(ref token)) => Ok(token.ty.clone()),
            Some(&Err(ref err)) => Err(err.clone()),
        }
    }

    fn advance(&mut self) -> Result<Token<'src>, SyntaxError> {
        self.tokens
            .next()
            .unwrap_or_else(|| Err(SyntaxError::UnexpectedEOF))
    }

    fn statement(&mut self) -> Result<Stmt<'src>, SyntaxError> {
        self.expression_stmt()
    }

    fn expression_stmt(&mut self) -> Result<Stmt<'src>, SyntaxError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::Expression(expr))
    }

    fn expression(&mut self) -> Result<Expr<'src>, SyntaxError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr<'src>, SyntaxError> {
        let mut expr = self.comparison()?;
        loop {
            let token = match self.peek_type()? {
                TokenType::EqualEqual | TokenType::BangEqual => self.advance()?,
                _ => break,
            };
            let rhs = self.comparison()?;
            expr = Expr {
                kind: Binary(BinaryExpr {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    operator: token.try_into()?,
                }),
            }
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr<'src>, SyntaxError> {
        let mut expr = self.term()?;
        loop {
            let token = match self.peek_type()? {
                TokenType::Greater
                | TokenType::GreaterEqual
                | TokenType::Less
                | TokenType::LessEqual => self.advance()?,
                _ => break,
            };
            let rhs = self.term()?;
            expr = Expr {
                kind: Binary(BinaryExpr {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    operator: token.try_into()?,
                }),
            }
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr<'src>, SyntaxError> {
        let mut expr = self.factor()?;
        loop {
            let token = match self.peek_type()? {
                TokenType::Minus | TokenType::Plus => self.advance()?,
                _ => break,
            };
            let rhs = self.factor()?;
            expr = Expr {
                kind: Binary(BinaryExpr {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    operator: token.try_into()?,
                }),
            }
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr<'src>, SyntaxError> {
        let mut expr = self.unary()?;
        loop {
            let token = match self.peek_type()? {
                TokenType::Slash | TokenType::Star => self.advance()?,
                _ => break,
            };
            let rhs = self.unary()?;
            expr = Expr {
                kind: Binary(BinaryExpr {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    operator: token.try_into()?,
                }),
            }
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr<'src>, SyntaxError> {
        let expr = match self.peek_type()? {
            TokenType::Bang | TokenType::Minus => {
                let operator = self.advance()?.try_into()?;
                let expr = Box::new(self.unary()?);
                Expr {
                    kind: ExprKind::Unary(UnaryExpr { operator, expr }),
                }
            }
            _ => self.primary()?,
        };
        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr<'src>, SyntaxError> {
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
                        self.consume_if(TokenType::Comment); // remove comment if necessary
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
    use loxide_macros::src;
    const SNAPSHOT_OUTPUT_BASE: &'static str = "../snapshots/parser/snapshots-outputs";
    const SNAPSHOT_INPUT_BASE: &'static str = "snapshots/parser/snapshots-inputs";

    #[test]
    fn primary() {
        insta::with_settings!({snapshot_path => SNAPSHOT_OUTPUT_BASE},{
            let src = src!(SNAPSHOT_INPUT_BASE, "primary.lox");
            let asts = src.split('\n').map(|line| {
                Parser::new(line).primary()
            }).collect::<Vec<_>>();
            insta::assert_debug_snapshot!(asts);
        })
    }

    #[test]
    fn unary() {
        insta::with_settings!({snapshot_path => SNAPSHOT_OUTPUT_BASE},{
            let src = src!(SNAPSHOT_INPUT_BASE, "unary.lox");
            let asts = src.split('\n').map(|line| {
                Parser::new(line).unary()
            }).collect::<Vec<_>>();
            insta::assert_debug_snapshot!(asts);
        })
    }

    #[test]
    fn factor() {
        insta::with_settings!({snapshot_path => SNAPSHOT_OUTPUT_BASE},{
            let src = src!(SNAPSHOT_INPUT_BASE, "factor.lox");
            let asts = src.split('\n').map(|line| {
                Parser::new(line).factor()
            }).collect::<Vec<_>>();
            insta::assert_debug_snapshot!(asts);
        })
    }

    #[test]
    fn term() {
        insta::with_settings!({snapshot_path => SNAPSHOT_OUTPUT_BASE},{
            let src = src!(SNAPSHOT_INPUT_BASE, "term.lox");
            let asts = src.split('\n').map(|line| {
                Parser::new(line).term()
            }).collect::<Vec<_>>();
            insta::assert_debug_snapshot!(asts);
        })
    }

    #[test]
    fn comparison() {
        insta::with_settings!({snapshot_path => SNAPSHOT_OUTPUT_BASE},{
            let src = src!(SNAPSHOT_INPUT_BASE, "comparison.lox");
            let asts = src.split('\n').map(|line| {
                Parser::new(line).comparison()
            }).collect::<Vec<_>>();
            insta::assert_debug_snapshot!(asts);
        })
    }

    #[test]
    fn equality() {
        insta::with_settings!({snapshot_path => SNAPSHOT_OUTPUT_BASE},{
            let src = src!(SNAPSHOT_INPUT_BASE, "equality.lox");
            let asts = src.split('\n').map(|line| {
                Parser::new(line).equality()
            }).collect::<Vec<_>>();
            insta::assert_debug_snapshot!(asts);
        })
    }

    #[test]
    fn expression() {
        insta::with_settings!({snapshot_path => SNAPSHOT_OUTPUT_BASE},{
            let src = src!(SNAPSHOT_INPUT_BASE, "expression.lox");
            let asts = src.split('\n').map(|line| {
                Parser::new(line).expression()
            }).collect::<Vec<_>>();
            insta::assert_debug_snapshot!(asts);
        })
    }

    #[test]
    fn single_statement() {
        insta::with_settings!({snapshot_path => SNAPSHOT_OUTPUT_BASE},{
            let src = src!(SNAPSHOT_INPUT_BASE, "single_statement.lox");
            let asts = src.split('\n').map(|line| {
                Parser::new(line).statement()
            }).collect::<Vec<_>>();
            insta::assert_debug_snapshot!(asts);
        })
    }

    #[test]
    fn many_statements() {
        insta::with_settings!({snapshot_path => SNAPSHOT_OUTPUT_BASE},{
            let src = src!(SNAPSHOT_INPUT_BASE, "many_statements.lox");
            let mut parser = Parser::new(&src);
            let results = parser.parse();
            insta::assert_debug_snapshot!(results);
        })
    }
}
