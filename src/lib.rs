mod ast;
mod interpreter;
mod parser;

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
            Some(Ok(l)) => run(&l)?,
            Some(Err(e)) => break Err(anyhow!(e)),
            None => break Ok(()),
        }
    }
}

pub fn run(contents: &str) -> anyhow::Result<()> {
    let parse = Parser::parse(Rule::file, contents);
    // println!("{parse:#?}");

    let arithmetic_expression = unsafe {
        parse?
            // Get file
            .next()
            .unwrap_unchecked()
            // Get arithmetic_expression
            .into_inner()
            .next()
            .unwrap_unchecked()
    };
    let ast = ast::ArithmeticExpr::from_pair(arithmetic_expression)?;
    // println!("{ast:#?}");

    let result = interpreter::eval(&ast);
    println!("{result}");

    Ok(())
}
