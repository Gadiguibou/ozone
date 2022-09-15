mod ast;
mod interpreter;
mod parser;
#[cfg(test)]
mod tests;
mod typed_ast;

use anyhow::anyhow;
use parser::Parser;
use parser::Rule;
use pest::Parser as PestParser;
use std::io::Write;
use std::io::{self, BufRead};

pub fn run_file(path: &str) -> anyhow::Result<()> {
    let contents = std::fs::read_to_string(path)?;
    run(&contents)
}

pub fn run_prompt() -> anyhow::Result<()> {
    let stdin = io::stdin();
    loop {
        print!(">>> ");
        io::stdout().flush().ok();
        match stdin.lock().lines().next() {
            Some(Ok(l)) => match run(&l) {
                Ok(_) => {}
                Err(e) => println!("{}", e),
            },
            Some(Err(e)) => break Err(anyhow!(e)),
            None => break Ok(()),
        }
    }
}

pub fn run(contents: &str) -> anyhow::Result<()> {
    let parse = Parser::parse(Rule::file, contents);
    // println!("{parse:#?}");

    let expression = unsafe {
        parse?
            // Get file
            .next()
            .unwrap_unchecked()
            // Get expression
            .into_inner()
            .next()
            .unwrap_unchecked()
    };
    // println!("{expression:#?}");

    let ast = ast::Expr::from_pair(expression)?;
    println!("{ast}");

    let mut bindings = zero_copy_stack::ZeroCopyStack::new();
    let result = interpreter::eval(&ast, &mut bindings.handle())?;
    println!("{result}");

    Ok(())
}
