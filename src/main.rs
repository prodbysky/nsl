use annotate_snippets::{Level, Renderer, Snippet};
mod ast;
mod lex;
mod type_check;

use clap::Parser;
use thiserror::Error;
fn main() -> Result<(), NslError> {
    let conf = Config::parse();
    if conf.office {
        println!("compilation...");
        let mut i = std::hint::black_box(0);
        while i != i32::MAX {
            i = std::hint::black_box(i.wrapping_add(1));
        }
        println!("...failed");
        std::process::Command::new("xdg-open")
            .arg("https://xkcd.com/303/")
            .spawn()
            .unwrap()
            .wait()
            .unwrap();
        return Ok(());
    }

    let source_code = std::fs::read_to_string(&conf.input_name)?;
    let error_renderer = Renderer::styled();
    let tokens = match lex::Lexer::new(&source_code).lex() {
        Ok(t) => t,
        Err(lex::Error { kind, span }) => {
            let title = match kind {
                lex::ErrorKind::SquishedNumber => "Number literal not separated from letters",
            };
            let msg = Level::Error.title(title).snippet(
                Snippet::source(&source_code)
                    .fold(true)
                    .origin(&conf.input_name)
                    .annotation(Level::Error.span(span).label("This one right here")),
            );
            eprintln!("{}", error_renderer.render(msg));

            return Err(NslError::Lex);
        }
    };

    let ast = match ast::Parser::new(&tokens).parse() {
        Ok(a) => a,
        Err(ast::Error { kind, span }) => {
            let title = match kind {
                ast::ErrorKind::UnexpectedEOF => "Expected some more tokens",
                ast::ErrorKind::UnexpectedToken { got, expected } => {
                    &format!("Expected: {expected:?}, got: {got:?}")
                }
            };
            let msg = Level::Error.title(title).snippet(
                Snippet::source(&source_code)
                    .fold(true)
                    .origin(&conf.input_name)
                    .annotation(Level::Error.span(span).label("Here gang")),
            );
            eprintln!("{}", error_renderer.render(msg));
            return Err(NslError::Ast);
        }
    };
    let type_checker = type_check::TypeChecker::new();
    let type_errors = type_checker.check(&ast);

    if !type_errors.is_empty() {
        for error in type_errors {
            let title = match &error.kind {
                type_check::TypeErrorKind::Mismatch { expected, got } => {
                    format!("Type mismatch: expected {expected}, got {got}")
                }
                type_check::TypeErrorKind::Undefined(name) => {
                    format!("Undefined variable: {name}")
                }
                type_check::TypeErrorKind::InvalidBinaryOp { left, op, right } => {
                    format!("Invalid binary operation: {left} {op:?} {right}")
                }
                type_check::TypeErrorKind::InvalidCondition(ty) => {
                    format!("Condition must be boolean, got {ty}")
                }
            };

            let msg = Level::Error.title(&title).snippet(
                Snippet::source(&source_code)
                    .fold(true)
                    .origin(&conf.input_name)
                    .annotation(Level::Error.span(error.span).label("Type error")),
            );
            eprintln!("{}", error_renderer.render(msg));
        }
        return Err(NslError::Type);
    }

    let ir = codegen::generate_ir(&ast);

    let ir_file_name = format!("{}.ssa", conf.output_name);
    let asm_file_name = format!("{}.s", conf.output_name);

    std::fs::write(&ir_file_name, &ir).unwrap();

    if conf.dump_ir {
        println!("{ir}");
    }

    std::process::Command::new("qbe")
        .arg(&ir_file_name)
        .arg("-o")
        .arg(&asm_file_name)
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    std::process::Command::new("gcc")
        .arg(&asm_file_name)
        .arg("-o")
        .arg(conf.output_name)
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    std::process::Command::new("rm")
        .arg(ir_file_name)
        .arg(asm_file_name)
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    Ok(())
}

mod codegen {
    use crate::ast::{Expr, ExprKind, Statement, StatementKind};
    use crate::lex::Operator;
    use qbe::{Cmp, Function, Instr, Linkage, Module, Type, Value};
    use std::collections::HashMap;

    pub fn generate_ir(ast: &[Statement]) -> String {
        let mut module = Module::new();
        let mut main_func = Function::new(Linkage::public(), "main", vec![], Some(Type::Word));

        let mut temp_count = 0;

        let mut variables: HashMap<String, Value> = HashMap::new();
        main_func.add_block("start");

        for st in ast {
            generate_statement(&mut main_func, st, &mut temp_count, &mut variables);
        }

        module.add_function(main_func).to_string()
    }

    fn generate_statement(
        func: &mut Function,
        st: &Statement,
        temp_count: &mut usize,
        vars: &mut HashMap<String, Value>,
    ) {
        match st {
            Statement {
                kind: StatementKind::Return(expr),
                ..
            } => {
                let result = eval_expr(func, expr, temp_count, vars);
                func.add_instr(Instr::Ret(Some(result)));
            }
            Statement {
                kind: StatementKind::DefineVar { name, value },
                ..
            } => {
                let value = eval_expr(func, value, temp_count, vars);
                vars.insert((*name).to_string(), value);
            }
            Statement {
                kind: StatementKind::AssignVar { name, value },
                ..
            } => {
                let value = eval_expr(func, value, temp_count, vars);
                *vars.get_mut(&(*name).to_string()).unwrap() = value;
            }
            Statement {
                kind: StatementKind::If { cond, block },
                ..
            } => {
                let cond = eval_expr(func, cond, temp_count, vars);

                let condition = Value::Temporary(format!("condition_{temp_count}"));

                let then_label_name = format!("then_{temp_count}");
                let merge_label_name = format!("merge_{temp_count}");

                func.assign_instr(
                    condition.clone(),
                    Type::Word,
                    Instr::Cmp(Type::Word, Cmp::Ne, cond, Value::Const(0)),
                );

                func.add_instr(Instr::Jnz(
                    condition,
                    then_label_name.clone(),
                    merge_label_name.clone(),
                ));

                func.add_block(then_label_name);
                for st in block {
                    generate_statement(func, st, temp_count, vars);
                }
                func.add_block(merge_label_name);
            }

            _ => todo!("Handle other statement types"),
        }
    }

    fn eval_expr(
        func: &mut Function,
        expr: &Expr,
        idx: &mut usize,
        vars: &HashMap<String, Value>,
    ) -> Value {
        match &expr.kind {
            ExprKind::Number(v) => Value::Const(*v),
            ExprKind::VariableName(name) => vars.get(&name.to_string()).unwrap().clone(),
            ExprKind::Binary { left, op, right } => {
                let left = eval_expr(func, left, idx, vars);
                let right = eval_expr(func, right, idx, vars);

                *idx += 1;
                let result_temporary = Value::Temporary(format!("t{idx}"));

                let operation_instruction = match op {
                    Operator::Plus => Instr::Add(left, right),
                    Operator::Minus => Instr::Sub(left, right),
                    Operator::Star => Instr::Mul(left, right),
                    Operator::Slash => Instr::Div(left, right),
                    Operator::More => Instr::Cmp(Type::Word, Cmp::Sgt, left, right),
                    Operator::Less => Instr::Cmp(Type::Word, Cmp::Slt, left, right),
                    _ => unreachable!(),
                };
                func.assign_instr(
                    Value::Temporary(format!("t{idx}")),
                    Type::Word,
                    operation_instruction,
                );
                result_temporary
            }
        }
    }
}

#[derive(Debug, Error)]
enum NslError {
    #[error("Couldn't find or something else happened when trying to read input file: {0}")]
    IO(#[from] std::io::Error),
    #[error("Lexer error")]
    Lex,
    #[error("Ast error")]
    Ast,
    #[error("Type error")]
    Type,
}

/// nsl Language compiler maybe????
#[derive(Parser)]
struct Config {
    #[arg()]
    input_name: String,

    #[arg(short)]
    output_name: String,

    /// Dump the generated QBE IR
    #[arg(long)]
    dump_ir: bool,

    /// For serious developers only.
    #[arg(long)]
    office: bool,
}
