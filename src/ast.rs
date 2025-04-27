use crate::lex::{Keyword, Operator, Span, Token, TokenKind};

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
pub enum StatementKind {
    Return(Expr),
}

#[derive(Debug, PartialEq)]
pub struct Statement {
    kind: StatementKind,
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

    pub fn parse(mut self) -> ParseResult<Vec<Statement>> {
        let mut stmts = vec![];
        while !self.finished() {
            stmts.push(self.parse_statement()?);
        }
        Ok(stmts)
    }

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        match self.peek().unwrap().clone() {
            Token {
                span,
                kind: TokenKind::Keyword(Keyword::Return),
            } => {
                self.next();
                let value = self.parse_expr()?;
                self.expect_semicolon()?;
                Ok(Statement {
                    span: span.start..value.span.end,
                    kind: StatementKind::Return(value),
                })
            }
            t => todo!("{t:?}"),
        }
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
            Some(Token {
                span,
                kind: TokenKind::Operator(op),
            }) => {
                let op = *op;
                self.next();
                Ok(Some(op))
            }
            Some(_) | None => Ok(None),
        }
    }

    fn expect_semicolon(&mut self) -> ParseResult<()> {
        match self.peek() {
            Some(Token {
                span: _,
                kind: TokenKind::Semicolon,
            }) => {
                self.next();
                Ok(())
            }
            Some(Token { span, kind }) => Err(Error {
                span: span.clone(),
                kind: ErrorKind::UnexpectedToken {
                    got: *kind,
                    expected: TokenKind::Semicolon,
                },
            }),
            None => Err(Error {
                span: self.prev_token_span(),
                kind: ErrorKind::UnexpectedEOF,
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
