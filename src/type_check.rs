use crate::ast::{self, ExprKind, Statement, StatementKind};
use crate::lex::{Operator, Span};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Integer,
    Bool,
    Error,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Integer => write!(f, "integer"),
            Type::Bool => write!(f, "bool"),
            Type::Error => write!(f, "error"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expr<'source> {
    pub expr: ast::Expr<'source>,
    pub ty: Type,
}

#[derive(Debug, PartialEq)]
pub struct TypeError<'source> {
    pub span: Span,
    pub kind: TypeErrorKind<'source>,
}

#[derive(Debug, PartialEq)]
pub enum TypeErrorKind<'source> {
    Mismatch {
        expected: Type,
        got: Type,
    },
    Undefined(&'source str),
    InvalidBinaryOp {
        left: Type,
        op: Operator,
        right: Type,
    },
    InvalidCondition(Type),
}

pub type TypeCheckerResult<'source, T> = Result<T, TypeError<'source>>;

pub struct TypeChecker<'source> {
    variables: HashMap<&'source str, Type>,
    errors: Vec<TypeError<'source>>,
}

impl<'source> TypeChecker<'source> {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            errors: vec![],
        }
    }
    pub fn check(mut self, ast: &[Statement<'source>]) -> Vec<TypeError<'source>> {
        for st in ast {
            self.check_statement(st);
        }
        self.errors
    }

    fn check_statement(&mut self, st: &Statement<'source>) {
        match &st.kind {
            StatementKind::Return(expr) => {
                let expr_typed = self.check_expr(expr);

                if expr_typed.ty != Type::Integer && expr_typed.ty != Type::Error {
                    self.errors.push(TypeError {
                        span: expr.span.clone(),
                        kind: TypeErrorKind::Mismatch {
                            expected: Type::Integer,
                            got: expr_typed.ty,
                        },
                    });
                }
            }
            StatementKind::DefineVar { name, value } => {
                let expr_typed = self.check_expr(value);
                if expr_typed.ty != Type::Error {
                    self.variables.insert(name, expr_typed.ty);
                }
            }
            StatementKind::AssignVar { name, value } => {
                if let Some(var_type) = self.variables.get(name).cloned() {
                    let typed_expr = self.check_expr(value);
                    if typed_expr.ty != var_type && typed_expr.ty != Type::Error {
                        self.errors.push(TypeError {
                            span: value.span.clone(),
                            kind: TypeErrorKind::Mismatch {
                                expected: var_type.clone(),
                                got: typed_expr.ty,
                            },
                        });
                    }
                } else {
                    self.errors.push(TypeError {
                        span: st.span.clone(),
                        kind: TypeErrorKind::Undefined(name),
                    });
                }
            }
            StatementKind::If { cond, block } | StatementKind::While { cond, block } => {
                let typed_cond = self.check_expr(cond);
                if typed_cond.ty != Type::Bool && typed_cond.ty != Type::Error {
                    self.errors.push(TypeError {
                        span: cond.span.clone(),
                        kind: TypeErrorKind::InvalidCondition(typed_cond.ty),
                    });
                }

                let previous_variables = self.variables.clone();
                for block_stmt in block {
                    self.check_statement(block_stmt);
                }
                self.variables = previous_variables;
            }
        }
    }

    fn check_expr(&mut self, expr: &ast::Expr<'source>) -> Expr<'source> {
        let ty = match &expr.kind {
            ExprKind::Number(_) => Type::Integer,
            ExprKind::VariableName(name) => {
                if let Some(ty) = self.variables.get(name) {
                    ty.clone()
                } else {
                    self.errors.push(TypeError {
                        span: expr.span.clone(),
                        kind: TypeErrorKind::Undefined(name),
                    });
                    Type::Error
                }
            }
            ExprKind::Binary { left, op, right } => {
                let left_typed = self.check_expr(left);
                let right_typed = self.check_expr(right);

                if left_typed.ty == Type::Error || right_typed.ty == Type::Error {
                    Type::Error
                } else {
                    match op {
                        Operator::Plus | Operator::Minus | Operator::Star | Operator::Slash => {
                            if left_typed.ty != Type::Integer {
                                self.errors.push(TypeError {
                                    span: left.span.clone(),
                                    kind: TypeErrorKind::Mismatch {
                                        expected: Type::Integer,
                                        got: left_typed.ty,
                                    },
                                });
                                Type::Error
                            } else if right_typed.ty != Type::Integer {
                                self.errors.push(TypeError {
                                    span: right.span.clone(),
                                    kind: TypeErrorKind::Mismatch {
                                        expected: Type::Integer,
                                        got: right_typed.ty,
                                    },
                                });
                                Type::Error
                            } else {
                                Type::Integer
                            }
                        }
                        Operator::Less | Operator::More => {
                            if left_typed.ty != Type::Integer {
                                self.errors.push(TypeError {
                                    span: left.span.clone(),
                                    kind: TypeErrorKind::Mismatch {
                                        expected: Type::Integer,
                                        got: left_typed.ty,
                                    },
                                });
                                Type::Error
                            } else if right_typed.ty != Type::Integer {
                                self.errors.push(TypeError {
                                    span: right.span.clone(),
                                    kind: TypeErrorKind::Mismatch {
                                        expected: Type::Integer,
                                        got: right_typed.ty,
                                    },
                                });
                                Type::Error
                            } else {
                                Type::Bool
                            }
                        }
                        _ => {
                            self.errors.push(TypeError {
                                span: expr.span.clone(),
                                kind: TypeErrorKind::InvalidBinaryOp {
                                    left: left_typed.ty,
                                    op: *op,
                                    right: right_typed.ty,
                                },
                            });
                            Type::Error
                        }
                    }
                }
            }
        };
        Expr {
            expr: expr.clone(),
            ty,
        }
    }
}
