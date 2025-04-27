use crate::lex::{Operator, Span, Token, TokenKind};
#[derive(Debug, PartialEq)]
pub enum ExprKind {
    Number(u64),
    Binary {
        left: Box<Expr>,
        op: Operator,
        right: Box<Expr>,
    },
}

#[derive(Debug, PartialEq)]
pub struct Expr {
    kind: ExprKind,
    span: Span,
}

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    UnexpectedEOF,
    UnexpectedToken { got: TokenKind, expected: TokenKind },
}
#[derive(Debug, PartialEq)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

pub type ParseResult<T> = Result<T, Error>;

pub struct Parser<'parser> {
    tokens: &'parser [Token],
    pos: usize,
}

impl<'parser> Parser<'parser> {
    pub fn new(tokens: &'parser [Token]) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn parse(mut self) -> ParseResult<Expr> {
        self.parse_expr()
    }

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_primary()?;

        while let Some(op) = self.parse_operator()? {
            let right = self.parse_primary()?;
            let span = left.span.start..right.span.end;
            left = Expr {
                span,
                kind: ExprKind::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
            };
        }

        Ok(left)
    }

    fn parse_primary(&mut self) -> ParseResult<Expr> {
        match self.peek().cloned() {
            None => Err(Error {
                kind: ErrorKind::UnexpectedEOF,
                span: self.prev_token_span(),
            }),
            Some(Token {
                span,
                kind: TokenKind::Number(n),
            }) => {
                self.next();
                Ok(Expr {
                    span: span.clone(),
                    kind: ExprKind::Number(n),
                })
            }
            Some(Token { span, kind }) => Err(Error {
                span: span.clone(),
                kind: ErrorKind::UnexpectedToken {
                    got: kind,
                    expected: TokenKind::Number(0),
                },
            }),
        }
    }

    fn prev_token_span(&self) -> Span {
        self.tokens[self.pos - 1].span.clone()
    }

    fn parse_operator(&mut self) -> ParseResult<Option<Operator>> {
        match self.peek() {
            None => Ok(None),
            Some(Token {
                span,
                kind: TokenKind::Operator(op),
            }) => {
                let op = *op;
                self.next();
                Ok(Some(op))
            }
            Some(Token { span, kind }) => Err(Error {
                span: span.clone(),
                kind: ErrorKind::UnexpectedToken {
                    got: *kind,
                    expected: TokenKind::Operator(Operator::Any),
                },
            }),
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }
    fn next(&mut self) -> Option<&Token> {
        let t = self.tokens.get(self.pos);
        self.pos += 1;
        t
    }

    fn finished(&self) -> bool {
        self.pos >= self.tokens.len()
    }
}
