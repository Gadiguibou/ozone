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
use std::convert::TryFrom;
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
    let parse = Parser::parse(Rule::file, contents)?;
    // println!("{parse:#?}");

    let ast = ast::Ast::try_from(parse)?;
    // println!("{ast}");

    let typed_ast = typed_ast::TypedAst::from(ast.clone())?;
    println!("{}", typed_ast.root);

    let mut bindings = zero_copy_stack::ZeroCopyStack::new();
    let result = interpreter::eval(&ast.root, &mut bindings.handle())?;
    println!("{result}");

    Ok(())
}
