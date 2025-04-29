use std::ops::Range;
pub type Span = Range<usize>;
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind<'source> {
    Semicolon,
    Equal,
    OpenCurly,
    CloseCurly,
    Number(u64),
    Operator(Operator),
    Keyword(Keyword),
    Identifier(&'source str),
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token<'source> {
    pub span: Span,
    pub kind: TokenKind<'source>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
    More,
    Less,
    Any,
}

impl Operator {
    pub fn prec(self) -> u8 {
        match self {
            Operator::Less | Operator::More => 0,
            Operator::Plus | Operator::Minus => 1,
            Operator::Star | Operator::Slash => 2,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Keyword {
    Return,
    Let,
    If,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    pub span: Span,
    pub kind: ErrorKind,
}
#[derive(Debug, PartialEq, Eq)]
pub enum ErrorKind {
    SquishedNumber,
}

pub type LexResult<T> = Result<T, Error>;

pub struct Lexer<'source> {
    src: &'source str,
    pos: usize,
}

impl<'source> Lexer<'source> {
    pub fn new(src: &'source str) -> Self {
        Self { src, pos: 0 }
    }

    fn parse_single_char_op(&mut self, kind: TokenKind<'source>) -> Token<'source> {
        self.next();
        Token {
            span: self.pos - 1..self.pos,
            kind,
        }
    }

    pub fn lex(mut self) -> LexResult<Vec<Token<'source>>> {
        let mut tokens = vec![];
        while !self.done() {
            self.skip_ws();
            if self.peek().is_none() {
                return Ok(tokens);
            }
            // Safe ^
            match self.peek().unwrap() {
                c if c.is_ascii_digit() => tokens.push(self.number()?),
                '+' => {
                    tokens.push(self.parse_single_char_op(TokenKind::Operator(Operator::Plus)));
                }
                '-' => {
                    tokens.push(self.parse_single_char_op(TokenKind::Operator(Operator::Minus)));
                }
                '*' => {
                    tokens.push(self.parse_single_char_op(TokenKind::Operator(Operator::Star)));
                }
                '/' => {
                    tokens.push(self.parse_single_char_op(TokenKind::Operator(Operator::Slash)));
                }
                '>' => {
                    tokens.push(self.parse_single_char_op(TokenKind::Operator(Operator::More)));
                }
                '<' => {
                    tokens.push(self.parse_single_char_op(TokenKind::Operator(Operator::Less)));
                }
                '=' => {
                    tokens.push(self.parse_single_char_op(TokenKind::Equal));
                }
                '{' => {
                    tokens.push(self.parse_single_char_op(TokenKind::OpenCurly));
                }
                '}' => {
                    tokens.push(self.parse_single_char_op(TokenKind::CloseCurly));
                }
                ';' => {
                    tokens.push(self.parse_single_char_op(TokenKind::Semicolon));
                }
                c if c.is_ascii_alphabetic() || c == '_' => {
                    let (ident, span) = self.ident();
                    match ident {
                        "return" => tokens.push(Token {
                            span,
                            kind: TokenKind::Keyword(Keyword::Return),
                        }),
                        "let" => tokens.push(Token {
                            span,
                            kind: TokenKind::Keyword(Keyword::Let),
                        }),
                        "if" => tokens.push(Token {
                            span,
                            kind: TokenKind::Keyword(Keyword::If),
                        }),
                        other => tokens.push(Token {
                            span,
                            kind: TokenKind::Identifier(other),
                        }),
                    };
                }
                c => todo!("Unexpected char found: {c}"),
            }
        }
        Ok(tokens)
    }

    fn number(&mut self) -> LexResult<Token<'source>> {
        let begin = self.pos;
        while self.peek().is_some_and(|c| c.is_ascii_digit()) {
            self.next();
        }
        let end = self.pos;
        match self.peek() {
            Some(c) if c.is_ascii_alphabetic() => Err(Error {
                span: begin..end + 1,
                kind: ErrorKind::SquishedNumber,
            }),
            _ => Ok(Token {
                span: begin..end,
                kind: TokenKind::Number(self.src[begin..end].parse().unwrap()),
            }),
        }
    }

    fn ident(&mut self) -> (&'source str, Span) {
        let begin = self.pos;
        while self
            .peek()
            .is_some_and(|c| c.is_ascii_alphanumeric() || c == '_')
        {
            self.next();
        }
        let end = self.pos;
        (&self.src[begin..end], begin..end)
    }

    fn skip_ws(&mut self) {
        while self.peek().is_some_and(|c| c.is_ascii_whitespace()) {
            self.next();
        }
    }

    fn peek(&self) -> Option<char> {
        self.src[self.pos..].chars().next()
    }
    fn next(&mut self) -> Option<char> {
        self.pos += 1;
        self.peek()
    }
    fn done(&self) -> bool {
        self.pos >= self.src.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn number_good() {
        let src = "123";
        let ts = Lexer::new(src).lex().unwrap();
        assert_eq!(
            ts,
            vec![Token {
                span: 0..3,
                kind: TokenKind::Number(123)
            }]
        );
    }
    #[test]
    fn number_bad() {
        let src = "123a";
        assert_eq!(
            Lexer::new(src).lex(),
            Err(Error {
                kind: ErrorKind::SquishedNumber,
                span: 0..4
            })
        );
    }

    #[test]
    fn operator() {
        let src = "+";
        assert_eq!(
            Lexer::new(src).lex(),
            Ok(vec![Token {
                span: 0..1,
                kind: TokenKind::Operator(Operator::Plus)
            }])
        );
    }
}
