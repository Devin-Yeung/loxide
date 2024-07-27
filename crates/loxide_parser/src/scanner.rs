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
            current: Span { start: 0, end: 0 },
            consumed: 0,
        }
    }

    pub fn consume_if(&mut self, expected: char) -> bool {
        if self.peek() == Some(expected) {
            self.advance();
            true
        } else {
            false
        }
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
            Some(_) => {
                self.current.end += 1;
            }
            _ => { /* Do nothing */ }
        }
        self.prophet.next()
    }

    pub fn is_at_end(&self) -> bool {
        self.prophet.peek().is_none()
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
            Some('\"') => self.string()?,
            Some(c) if c.is_ascii_digit() => self.number()?,
            Some(c) if c.is_ascii_alphabetic() || c == '_' => self.identifier(),
            None => TokenType::EOF,
            _ => return Err(SyntaxError::UnexpectedChar(self.current)),
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

    fn string(&mut self) -> Result<TokenType<'src>, SyntaxError> {
        loop {
            match self.advance() {
                None => return Err(SyntaxError::InvalidStringLiteral(self.current)),
                Some('\"') => {
                    // discard the left quote
                    // return owned string only when *string escape* happens
                    let lexeme = self.lexeme();
                    // trim the left and right quote
                    let raw_str = Cow::Borrowed(&lexeme[1..lexeme.len() - 1]);
                    return Ok(TokenType::Literal(Literal::String(raw_str)));
                }
                _ => { /* Continue */ }
            }
        }
    }

    fn number(&mut self) -> Result<TokenType<'src>, SyntaxError> {
        while let Some(c) = self.peek() {
            match c {
                '.' => {
                    if self.peek_next().map_or(false, |c| c.is_ascii_digit()) {
                        self.advance()
                    } else {
                        return Err(SyntaxError::InvalidNumber(Span {
                            start: self.current.start,
                            end: self.current.end + 1,
                        }));
                    }
                }
                c if c.is_ascii_digit() => self.advance(),
                _ => break,
            };
        }
        let num = self
            .lexeme()
            .parse::<f64>()
            .map_err(|_| SyntaxError::InvalidNumber(self.current))?;
        Ok(TokenType::Literal(Literal::Number(num)))
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
            .map(TokenType::Keyword)
            .unwrap_or(TokenType::Identifier)
    }
}

impl<'src> Iterator for Scanner<'src> {
    type Item = Result<Token<'src>, SyntaxError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.scan_token() {
            Ok(x) if x.ty == TokenType::EOF => None,
            Ok(x) => Some(Ok(x)),
            Err(e) => Some(Err(e)),
        }
    }
}

impl<'src> From<&'src str> for Scanner<'src> {
    fn from(value: &'src str) -> Self {
        Scanner {
            src: value,
            prophet: Prophet::new(value.chars()),
            current: Span { start: 0, end: 0 },
            consumed: 0,
        }
    }
}

impl<'src> From<&'src String> for Scanner<'src> {
    fn from(value: &'src String) -> Self {
        Scanner {
            src: value,
            prophet: Prophet::new(value.chars()),
            current: Span { start: 0, end: 0 },
            consumed: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::scanner::Scanner;
    use crate::utils::test_utils::{LINEBREAK, SPAN_FILTER};
    use loxide_diagnostic::reporter::{Reporter, Style};
    use loxide_testsuite::unittest;
    use miette::Report;
    use std::sync::Arc;

    macro_rules! tokens {
        ($src:expr) => {{
            let scanner = $crate::scanner::Scanner::from($src);
            scanner.collect::<::std::vec::Vec<_>>()
        }};
    }

    fn display_tokenize_error<S: AsRef<str>>(src: S) -> String {
        let mut reporter = Reporter::new(Style::NoColor);
        let source = Arc::new(src.as_ref().to_string());
        let errs = tokens!(src.as_ref())
            .into_iter()
            .filter(|x| x.is_err())
            .map(|x| {
                let report: Report = x.unwrap_err().into();
                report.with_source_code(source.clone())
            })
            .collect::<Vec<_>>();
        reporter.extend(errs);
        reporter.report_to_string()
    }

    unittest!(single_char, filters => vec![SPAN_FILTER], |src| {
        let scanner = Scanner::from(src);
        let tokens = scanner.map(|x| x.unwrap()).collect::<Vec<_>>();
        insta::assert_debug_snapshot!(tokens);
    });

    unittest!(single_or_double_char, filters => vec![SPAN_FILTER], |src| {
        let scanner = Scanner::from(src);
        let tokens = scanner.map(|x| x.unwrap()).collect::<Vec<_>>();
        insta::assert_debug_snapshot!(tokens);
    });

    unittest!(valid_number, filters => vec![SPAN_FILTER], |src| {
        let scanner = Scanner::from(src);
        let tokens = scanner.collect::<Vec<_>>();
        insta::assert_debug_snapshot!(tokens);
    });

    unittest!(valid_string, filters => vec![SPAN_FILTER], |src| {
        let scanner = Scanner::from(src);
        let tokens = scanner.collect::<Vec<_>>();
        insta::assert_debug_snapshot!(tokens);
    });

    unittest!(valid_idents_and_keywords, filters => vec![SPAN_FILTER], |src| {
        let scanner = Scanner::from(src);
        let tokens = scanner.collect::<Vec<_>>();
        insta::assert_debug_snapshot!(tokens);
    });

    unittest!(expressions, filters => vec![SPAN_FILTER], |src| {
        let scanner = Scanner::from(src);
        let tokens = scanner.collect::<Vec<_>>();
        insta::assert_debug_snapshot!(tokens);
    });

    unittest!(should_fail, |src| {
        let output = src
            .split(LINEBREAK)
            .map(display_tokenize_error)
            .collect::<Vec<_>>()
            .join("\n");

        insta::assert_snapshot!(output);
    });
}
