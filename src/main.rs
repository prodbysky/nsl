use annotate_snippets::{Level, Renderer, Snippet};
mod ast;
mod lex;

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
    use qbe::{Function, Instr, Linkage, Module, Type, Value};

    pub fn generate_ir(ast: &[Statement]) -> String {
        let mut module = Module::new();
        let mut main_func = Function::new(Linkage::public(), "main", vec![], Some(Type::Word));

        let mut temp_count = 0;

        main_func.add_block("start");

        for st in ast {
            match st {
                Statement {
                    kind: StatementKind::Return(expr),
                    ..
                } => {
                    let result = eval_expr(&mut main_func, expr, &mut temp_count);
                    main_func.add_instr(Instr::Ret(Some(result)));
                    return module.add_function(main_func).to_string();
                }
                _ => todo!("Handle other statement types"),
            }
        }

        main_func.add_instr(Instr::Ret(Some(Value::Const(0))));
        module.add_function(main_func).to_string()
    }

    fn eval_expr(func: &mut Function, expr: &Expr, idx: &mut usize) -> Value {
        match &expr.kind {
            ExprKind::Number(v) => Value::Const(*v),
            ExprKind::Binary { left, op, right } => {
                let left = eval_expr(func, left, idx);
                let right = eval_expr(func, right, idx);

                *idx += 1;
                let result_temporary = Value::Temporary(format!("t{idx}"));

                let operation_instruction = match op {
                    Operator::Plus => Instr::Add(left, right),
                    Operator::Minus => Instr::Sub(left, right),
                    Operator::Star => Instr::Mul(left, right),
                    Operator::Slash => Instr::Div(left, right),
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
