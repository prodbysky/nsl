use crate::lex::{Keyword, Operator, Span, Token, TokenKind};

#[derive(Debug, PartialEq)]
pub enum ExprKind<'source> {
    Number(u64),
    VariableName(&'source str),
    Binary {
        left: Box<Expr<'source>>,
        op: Operator,
        right: Box<Expr<'source>>,
    },
}

#[derive(Debug, PartialEq)]
pub struct Expr<'source> {
    pub kind: ExprKind<'source>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum StatementKind<'source> {
    Return(Expr<'source>),
    DefineVar {
        name: &'source str,
        value: Expr<'source>,
    },
    AssignVar {
        name: &'source str,
        value: Expr<'source>,
    },
}

#[derive(Debug, PartialEq)]
pub struct Statement<'source> {
    pub kind: StatementKind<'source>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum ErrorKind<'source> {
    UnexpectedEOF,
    UnexpectedToken {
        got: TokenKind<'source>,
        expected: TokenKind<'source>,
    },
}
#[derive(Debug, PartialEq)]
pub struct Error<'source> {
    pub kind: ErrorKind<'source>,
    pub span: Span,
}

pub type ParseResult<'source, T> = Result<T, Error<'source>>;

pub struct Parser<'source> {
    tokens: &'source [Token<'source>],
    pos: usize,
}

impl<'source> Parser<'source> {
    pub fn new(tokens: &'source [Token]) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn parse(mut self) -> ParseResult<'source, Vec<Statement<'source>>> {
        let mut stmts = vec![];
        while !self.finished() {
            stmts.push(self.parse_statement()?);
        }
        Ok(stmts)
    }

    fn parse_statement(&mut self) -> ParseResult<'source, Statement<'source>> {
        match self.peek().unwrap().clone() {
            Token {
                span,
                kind: TokenKind::Keyword(Keyword::Return),
            } => {
                self.next();
                let value = self.parse_expr(0)?;
                self.expect_semicolon()?;
                Ok(Statement {
                    span: span.start..value.span.end,
                    kind: StatementKind::Return(value),
                })
            }
            Token {
                span,
                kind: TokenKind::Keyword(Keyword::Let),
            } => {
                self.next();
                let name = self.expect_ident()?;
                self.expect_equal()?;
                let expr = self.parse_expr(0)?;
                self.expect_semicolon()?;
                Ok(Statement {
                    span: span.start..expr.span.end,
                    kind: StatementKind::DefineVar { name, value: expr },
                })
            }
            Token {
                span,
                kind: TokenKind::Identifier(name),
            } => {
                self.next();
                self.expect_equal()?;
                let expr = self.parse_expr(0)?;
                self.expect_semicolon()?;
                Ok(Statement {
                    span: span.start..expr.span.end,
                    kind: StatementKind::AssignVar { name, value: expr },
                })
            }
            t => todo!("{t:?}"),
        }
    }

    fn parse_expr(&mut self, min_prec: u8) -> ParseResult<'source, Expr<'source>> {
        let mut left = self.parse_primary()?;

        while let Some(op_token) = self.peek() {
            if let TokenKind::Operator(op) = op_token.kind {
                let prec = op.prec();
                if prec < min_prec {
                    break;
                }

                self.next();

                let right = self.parse_expr(prec + 1)?;
                let span = left.span.start..right.span.end;
                left = Expr {
                    span,
                    kind: ExprKind::Binary {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                    },
                };
            } else {
                break;
            }
        }

        Ok(left)
    }

    fn parse_primary(&mut self) -> ParseResult<'source, Expr<'source>> {
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
            Some(Token {
                span,
                kind: TokenKind::Identifier(name),
            }) => {
                self.next();
                Ok(Expr {
                    span: span.clone(),
                    kind: ExprKind::VariableName(name),
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

    fn expect_semicolon(&mut self) -> ParseResult<'source, ()> {
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

    fn expect_equal(&mut self) -> ParseResult<'source, ()> {
        match self.peek() {
            Some(Token {
                span: _,
                kind: TokenKind::Equal,
            }) => {
                self.next();
                Ok(())
            }
            Some(Token { span, kind }) => Err(Error {
                span: span.clone(),
                kind: ErrorKind::UnexpectedToken {
                    got: *kind,
                    expected: TokenKind::Equal,
                },
            }),
            None => Err(Error {
                span: self.prev_token_span(),
                kind: ErrorKind::UnexpectedEOF,
            }),
        }
    }

    fn expect_ident(&mut self) -> ParseResult<'source, &'source str> {
        match self.peek().cloned() {
            Some(Token {
                span: _,
                kind: TokenKind::Identifier(id),
            }) => {
                self.next();
                Ok(id)
            }
            Some(Token { span, kind }) => Err(Error {
                span: span.clone(),
                kind: ErrorKind::UnexpectedToken {
                    got: kind,
                    expected: TokenKind::Identifier("any"),
                },
            }),
            None => Err(Error {
                span: self.prev_token_span(),
                kind: ErrorKind::UnexpectedEOF,
            }),
        }
    }

    fn peek(&self) -> Option<&Token<'source>> {
        self.tokens.get(self.pos)
    }
    fn next(&mut self) -> Option<&Token<'source>> {
        let t = self.tokens.get(self.pos);
        self.pos += 1;
        t
    }

    fn finished(&self) -> bool {
        self.pos >= self.tokens.len()
    }
}
