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
use crate::ast::{BinaryExpr, Expr, ExprKind};
use crate::error::SyntaxError;
use crate::scanner::Scanner;
use crate::token::{Keyword, Token, TokenType};
use std::iter::Peekable;

pub struct Parser<'src> {
    tokens: Peekable<Scanner<'src>>,
}

impl<'src> Parser<'src> {
    fn new(src: &'src str) -> Parser {
        Parser {
            tokens: Scanner::new(src).peekable(),
        }
    }

    fn consume(&mut self, ty: TokenType) -> Result<(), SyntaxError> {
        return if self.peek_type()? == ty {
            self.advance()?;
            Ok(())
        } else {
            todo!()
        };
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

    fn expression(&mut self) -> Result<Expr<'src>, SyntaxError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr<'src>, SyntaxError> {
        let mut expr = self.comparison()?;
        loop {
            let token = match self.peek_type()? {
                TokenType::Bang | TokenType::BangEqual => self.advance()?,
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
        todo!()
    }

    fn primary(&mut self) -> Result<Expr<'src>, SyntaxError> {
        let expr = match self.peek_type()? {
            TokenType::Keyword(Keyword::True) => Expr {
                kind: ExprKind::Literal(Boolean(true)),
            },
            TokenType::Keyword(Keyword::False) => Expr {
                kind: ExprKind::Literal(Boolean(false)),
            },
            TokenType::Keyword(Keyword::Nil) => Expr {
                kind: ExprKind::Literal(Nil),
            },
            TokenType::Literal(_) => {
                let literal: ast::Literal = self.advance()?.try_into()?;
                Expr {
                    kind: ExprKind::Literal(literal),
                }
            }
            _ => unreachable!("TODO"),
        };
        Ok(expr)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Parser;
    use crate::src;
    const SNAPSHOT_OUTPUT_BASE: &'static str = "../snapshots/parser/snapshots-outputs";
    const SNAPSHOT_INPUT_BASE: &'static str = "../snapshots/parser/snapshots-inputs";

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
}
