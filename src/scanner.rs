use crate::error::SyntaxError;
use crate::prophetic::Prophet;
use crate::token::{Keyword, Literal, Span, Token, TokenType};
use std::borrow::Cow;
use std::str::Chars;

pub struct Scanner<'src> {
    src: &'src str,
    prophet: Prophet<Chars<'src>>,
    current: Span,
    /// Number of chars already have been consumed
    /// this field increased only when we yield a token
    consumed: usize,
}

impl<'src> Scanner<'src> {
    pub fn new(src: &'src str) -> Scanner<'src> {
        Scanner {
            src,
            prophet: Prophet::new(src.chars()),
            current: Span {
                line: 1,
                start: 0,
                end: 0,
            },
            consumed: 0,
        }
    }

    pub fn consume_if(&mut self, expected: char) -> bool {
        return if self.peek() == Some(expected) {
            self.advance();
            true
        } else {
            false
        };
    }

    fn peek(&mut self) -> Option<char> {
        self.prophet.peek()
    }

    fn peek_next(&mut self) -> Option<char> {
        self.prophet.peek_next()
    }

    /// advance will take care of span and iter for you
    fn advance(&mut self) -> Option<char> {
        match self.prophet.peek() {
            Some('\n') => {
                self.current.line += 1;
                self.current.start = 0;
                self.current.end = 0;
            }
            Some(_) => {
                self.current.end += 1;
            }
            _ => { /* Do nothing */ }
        }
        self.prophet.next()
    }

    pub fn is_at_end(&self) -> bool {
        self.prophet.peek() == None
    }

    fn eat_whitespace(&mut self) {
        loop {
            match self.peek() {
                Some(' ') | Some('\n') | Some('\r') | Some('\t') => {
                    self.advance();
                    self.consumed += 1
                }
                _ => {
                    self.current.start = self.current.end;
                    break;
                }
            }
        }
    }

    pub fn scan_token(&mut self) -> Result<Token<'src>, SyntaxError> {
        self.eat_whitespace();
        let ty = match self.advance() {
            Some('(') => TokenType::LeftParen,
            Some(')') => TokenType::RightParen,
            Some('{') => TokenType::LeftBrace,
            Some('}') => TokenType::RightBrace,
            Some(',') => TokenType::Comma,
            Some('.') => TokenType::Dot,
            Some('-') => TokenType::Minus,
            Some('+') => TokenType::Plus,
            Some(';') => TokenType::Semicolon,
            Some('*') => TokenType::Star,
            Some('!') => match self.consume_if('=') {
                true => TokenType::BangEqual,
                false => TokenType::Bang,
            },
            Some('=') => match self.consume_if('=') {
                true => TokenType::EqualEqual,
                false => TokenType::Equal,
            },
            Some('<') => match self.consume_if('=') {
                true => TokenType::LessEqual,
                false => TokenType::Less,
            },
            Some('>') => match self.consume_if('=') {
                true => TokenType::GreaterEqual,
                false => TokenType::Greater,
            },
            Some('/') => match self.consume_if('/') {
                true => {
                    while self.peek() != Some('\n') && !self.is_at_end() {
                        self.advance();
                    }
                    TokenType::Comment
                }
                false => TokenType::Slash,
            },
            Some('\"') => TokenType::Literal(Literal::String(self.string()?)),
            Some(c) if c.is_digit(10) => TokenType::Literal(Literal::Number(self.number()?)),
            Some(c) if c.is_ascii_alphabetic() || c == '_' => self.identifier(),
            None => TokenType::EOF,
            _ => return Err(SyntaxError::UnexpectedChar(self.current.line)),
        };
        Ok(self.yield_token(ty))
    }

    fn yield_token(&mut self, ty: TokenType<'src>) -> Token<'src> {
        let lexeme = self.lexeme();
        self.consumed += lexeme.len();
        Token {
            ty,
            lexeme,
            span: self.current,
        }
    }

    /// return the lexeme currently working on
    fn lexeme(&self) -> &'src str {
        let len = self.current.end - self.current.start;
        &self.src[self.consumed..self.consumed + len]
    }

    fn string(&mut self) -> Result<Cow<'src, str>, SyntaxError> {
        loop {
            match self.advance() {
                None => return Err(SyntaxError::InvalidStringLiteral),
                Some('\"') => {
                    // discard the left quote
                    // return owned string only when *string escape* happens
                    let lexeme = self.lexeme();
                    return Ok(Cow::Borrowed(
                        &lexeme[1..lexeme.len() - 1], // trim the left and right quote
                    ));
                }
                _ => { /* Continue */ }
            }
        }
    }

    fn number(&mut self) -> Result<f64, SyntaxError> {
        while let Some(c) = self.peek() {
            match c {
                '.' => {
                    if self.peek_next().map_or(false, |c| c.is_digit(10)) {
                        self.advance()
                    } else {
                        return Err(SyntaxError::InvalidNumber);
                    }
                }
                c if c.is_digit(10) => self.advance(),
                _ => break,
            };
        }
        self.lexeme()
            .parse::<f64>()
            .map_err(|_| SyntaxError::InvalidNumber)
    }

    fn identifier(&mut self) -> TokenType<'src> {
        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() {
                self.advance();
            } else {
                break;
            }
        }
        let content =
            &self.src[self.consumed..self.consumed + self.current.end - self.current.start];
        content
            .parse::<Keyword>()
            .map(|k| TokenType::Keyword(k))
            .unwrap_or(TokenType::Identifier)
    }
}

impl<'src> Iterator for Scanner<'src> {
    type Item = Result<Token<'src>, SyntaxError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.scan_token() {
            Ok(x) if x.ty == TokenType::EOF => return None,
            Ok(x) if x.ty != TokenType::Comment => return Some(Ok(x)),
            Err(e) => return Some(Err(e)),
            _ => {}
        }
        None
    }
}

impl<'src> From<&'src str> for Scanner<'src> {
    fn from(value: &'src str) -> Self {
        Scanner {
            src: value,
            prophet: Prophet::new(value.chars()),
            current: Span {
                line: 1,
                start: 0,
                end: 0,
            },
            consumed: 0,
        }
    }
}

impl<'src> From<&'src String> for Scanner<'src> {
    fn from(value: &'src String) -> Self {
        Scanner {
            src: value,
            prophet: Prophet::new(value.chars()),
            current: Span {
                line: 1,
                start: 0,
                end: 0,
            },
            consumed: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::scanner::Scanner;
    use crate::{src, tokens};
    const SNAPSHOT_OUTPUT_BASE: &'static str = "../snapshots/parser/snapshots-outputs";
    const SNAPSHOT_INPUT_BASE: &'static str = "../snapshots/parser/snapshots-inputs";

    #[test]
    fn single_char() {
        insta::with_settings!({snapshot_path => SNAPSHOT_OUTPUT_BASE},{
                let src = src!(SNAPSHOT_INPUT_BASE, "single_char.lox");
                let scanner = Scanner::from(&src);
                let tokens = scanner
                    .map(|x| x.unwrap())
                    .collect::<Vec<_>>();
                insta::assert_debug_snapshot!(tokens);
            }
        )
    }

    #[test]
    fn single_or_double_char() {
        insta::with_settings!({snapshot_path => SNAPSHOT_OUTPUT_BASE},{
                let src = src!(SNAPSHOT_INPUT_BASE, "single_or_double_char.lox");
                let scanner = Scanner::from(&src);
                let tokens = scanner
                    .map(|x| x.unwrap())
                    .collect::<Vec<_>>();
                insta::assert_debug_snapshot!(tokens);
            }
        )
    }

    #[test]
    fn valid_number() {
        insta::with_settings!({snapshot_path => SNAPSHOT_OUTPUT_BASE},{
                let src = src!(SNAPSHOT_INPUT_BASE, "valid_number.lox");
                let scanner = Scanner::from(&src);
                let tokens = scanner
                    .collect::<Vec<_>>();
                insta::assert_debug_snapshot!(tokens);
            }
        )
    }

    #[test]
    fn invalid_number() {
        insta::with_settings!({snapshot_path => SNAPSHOT_OUTPUT_BASE},{
            let src = src!(SNAPSHOT_INPUT_BASE, "invalid_number.lox");
            // each line a single new src
            let tokens = src.split('\n').map(|l| tokens!(l)).collect::<Vec<_>>();
            insta::assert_debug_snapshot!(tokens);
        })
    }

    #[test]
    fn valid_string() {
        insta::with_settings!({snapshot_path => SNAPSHOT_OUTPUT_BASE},{
            let src = src!(SNAPSHOT_INPUT_BASE, "valid_string.lox");
            let scanner = Scanner::from(&src);
            let tokens = scanner
                .collect::<Vec<_>>();
            insta::assert_debug_snapshot!(tokens);
        })
    }

    #[test]
    fn invalid_string() {
        insta::with_settings!({snapshot_path => SNAPSHOT_OUTPUT_BASE},{
            let src = src!(SNAPSHOT_INPUT_BASE, "invalid_string.lox");
            // each line a single new src
            let tokens = src.split('\n').map(|l| tokens!(l)).collect::<Vec<_>>();
            insta::assert_debug_snapshot!(tokens);
        })
    }

    #[test]
    fn valid_indents_and_keywords() {
        insta::with_settings!({snapshot_path => SNAPSHOT_OUTPUT_BASE},{
            let src = src!(SNAPSHOT_INPUT_BASE, "valid_idents_and_keywords.lox");
            let scanner = Scanner::from(&src);
            let tokens = scanner.collect::<Vec<_>>();
            insta::assert_debug_snapshot!(tokens);
        })
    }
}
