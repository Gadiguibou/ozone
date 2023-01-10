mod ast;
mod interpreter;
mod parser;
#[cfg(test)]
mod tests;
mod typed_ast;
mod types;

use anyhow::anyhow;
use directories::BaseDirs;
use parser::Parser;
use parser::Rule;
use pest::Parser as PestParser;
use rustyline::error::ReadlineError;
use rustyline::Config;

pub fn run_file(path: &str) -> anyhow::Result<()> {
    let contents = std::fs::read_to_string(path)?;
    run(&contents)
}

pub fn run_prompt() -> anyhow::Result<()> {
    let mut editor =
        rustyline::Editor::<()>::with_config(Config::builder().auto_add_history(true).build())?;

    let history_file =
        BaseDirs::new().map(|base_dirs| base_dirs.data_dir().join("ozone/history.txt"));

    if let Some(history_file) = &history_file {
        if !history_file.exists() {
            std::fs::create_dir_all(history_file.parent().unwrap())?;
        }
        let _ = editor.load_history(history_file);
    }

    loop {
        match editor.readline(">>> ") {
            Ok(l) => {
                if l.trim().is_empty() {
                    // Skip empty lines
                    continue;
                } else if l.trim() == "exit" {
                    // Exit the repl
                    break Ok(());
                }

                // Run the line
                match run(&l) {
                    Ok(_) => {}
                    Err(e) => eprintln!("{}", e),
                };

                // Save history
                if let Some(history_file) = &history_file {
                    let _ = editor.append_history(history_file);
                }
            }
            Err(ReadlineError::Interrupted) => {}
            Err(ReadlineError::Eof) => break Ok(()),
            Err(e) => break Err(anyhow!(e)),
        }
    }
}

pub fn run(contents: &str) -> anyhow::Result<()> {
    let parse = Parser::parse(Rule::file, contents)?;
    // println!("{parse:#?}");

    let ast = ast::Ast::parse(parse)?;
    // println!("{ast}");

    let typed_ast = typed_ast::TypedAst::from(ast)?;
    println!("{:#?}", typed_ast.root);

    // let mut bindings = zero_copy_stack::ZeroCopyStack::new();
    // let result = interpreter::eval(ast.root, bindings.handle())?;
    // println!("{result}");

    Ok(())
}
