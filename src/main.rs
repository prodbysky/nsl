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

    dbg!(&tokens);

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
    Ok(())
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

    /// For serious developers only.
    #[arg(long)]
    office: bool,
}
