use annotate_snippets::{Level, Renderer, Snippet};
mod ast;
mod codegen;
mod lex;
mod type_check;

use clap::Parser;
use thiserror::Error;

fn main() -> Result<(), NslError> {
    let conf = Config::parse();
    if conf.office {
        do_funny();
        return Ok(());
    }
    let source_code = std::fs::read_to_string(&conf.input_name)?;
    let error_renderer = Renderer::styled();
    let tokens = lex(&conf.input_name, &error_renderer, &source_code)?;
    let ast = parse_ast(&conf.input_name, &error_renderer, &tokens, &source_code)?;
    check_types(&conf.input_name, &error_renderer, &ast, &source_code)?;
    compile(&ast, &conf.output_name, conf.dump_ir);

    Ok(())
}

fn lex<'source>(
    origin: &str,
    error_renderer: &Renderer,
    source_code: &'source str,
) -> Result<Vec<lex::Token<'source>>, NslError> {
    let lexer = lex::Lexer::new(source_code);

    let before = std::time::Instant::now();
    let tokens = lexer.lex();
    println!("Tokenization took: {:.2?}", before.elapsed());
    let tokens = match tokens {
        Ok(t) => t,
        Err(lex::Error { kind, span }) => {
            let title = match kind {
                lex::ErrorKind::SquishedNumber => "Number literal not separated from letters",
            };
            let msg = Level::Error.title(title).snippet(
                Snippet::source(source_code)
                    .fold(true)
                    .origin(origin)
                    .annotation(Level::Error.span(span).label("This one right here")),
            );
            eprintln!("{}", error_renderer.render(msg));

            return Err(NslError::Lex);
        }
    };
    Ok(tokens)
}

fn parse_ast<'source>(
    origin: &str,
    error_renderer: &Renderer,
    tokens: &'source [lex::Token<'source>],
    source_code: &'source str,
) -> Result<Vec<ast::Statement<'source>>, NslError> {
    let parser = ast::Parser::new(tokens);
    let before = std::time::Instant::now();
    let ast = parser.parse();
    println!("Parsing the AST took: {:.2?}", before.elapsed());
    let ast = match ast {
        Ok(a) => a,
        Err(ast::Error { kind, span }) => {
            let title = match kind {
                ast::ErrorKind::UnexpectedEOF => "Expected some more tokens",
                ast::ErrorKind::UnexpectedToken { got, expected } => {
                    &format!("Expected: {expected:?}, got: {got:?}")
                }
            };
            let msg = Level::Error.title(title).snippet(
                Snippet::source(source_code)
                    .fold(true)
                    .origin(origin)
                    .annotation(Level::Error.span(span).label("Here gang")),
            );
            eprintln!("{}", error_renderer.render(msg));
            return Err(NslError::Ast);
        }
    };
    Ok(ast)
}

fn check_types<'source>(
    origin: &str,
    error_renderer: &Renderer,
    ast: &[ast::Statement<'source>],
    source_code: &'source str,
) -> Result<(), NslError> {
    let type_checker = type_check::TypeChecker::new();
    let before = std::time::Instant::now();
    let type_errors = type_checker.check(ast);
    println!("Type check pass took: {:.2?}", before.elapsed());

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
                Snippet::source(source_code)
                    .fold(true)
                    .origin(origin)
                    .annotation(Level::Error.span(error.span).label("Type error")),
            );
            eprintln!("{}", error_renderer.render(msg));
        }
        return Err(NslError::Type);
    }
    Ok(())
}

fn compile(ast: &[ast::Statement], output: &str, dump_ir: bool) {
    let before = std::time::Instant::now();
    let ir = codegen::generate_ir(ast);
    println!("IR generation took: {:.2?}", before.elapsed());

    let ir_file_name = format!("{output}.ssa");
    let asm_file_name = format!("{output}.s");

    std::fs::write(&ir_file_name, &ir).unwrap();

    if dump_ir {
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
        .arg(output)
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
}
fn do_funny() {
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
