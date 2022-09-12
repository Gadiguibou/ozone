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

#[cfg(test)]
mod tests {
    use super::*;

    use ast::Expr;
    use interpreter::DynValue;

    fn run(contents: &str) -> anyhow::Result<(Expr, DynValue)> {
        let parse = Parser::parse(Rule::file, contents);

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

        let ast = Expr::from_pair(expression)?;

        let mut bindings = zero_copy_stack::ZeroCopyStack::new();
        let result = interpreter::eval(&ast, &mut bindings.handle())?;
        Ok((ast, result))
    }

    #[test]
    fn test_arithmetic_expression() {
        let program = "3 - (2 + 2) + 4 * 2 ^ 2 ^ 3";

        let run = run(program);
        assert!(run.is_ok());

        let (ast, result) = run.unwrap();

        assert_eq!(format!("{ast}"), "((3 - (2 + 2)) + (4 * (2 ^ (2 ^ 3))))");
        assert_eq!(result, DynValue::Integer(1023));
    }

    #[test]
    fn test_binding_expression_works() {
        let program = "
            let test =
                1
            in
                (let test = 0 in 0 /* Should not shadow the top-level binding */) + test
        ";

        let run = run(program);
        assert!(run.is_ok());

        let (ast, result) = run.unwrap();

        assert_eq!(
            format!("{ast}"),
            "(let test = 1 in ((let test = 0 in 0) + test))"
        );

        assert_eq!(result, DynValue::Integer(1));
    }

    #[test]
    fn test_binding_expression_fails_on_unbound_variables() {
        let program = "
            let test =
                let test2 = 2 in 1
            in
                test2
        ";

        let run = run(program);
        assert!(run.is_err());
        assert_eq!(run.unwrap_err().to_string(), "Undefined variable: test2");
    }

    #[test]
    fn test_conditionals() {
        let program = "
        if 1 > 0 and (1 <= 0 or false) then
            false
        else if /* Nested if's */ if 0 == 1 or 0 != 1 then true else false then
            not false /* Should evaluate to this branch */
        else
            false
        ";

        let run = run(program);
        assert!(run.is_ok());

        let (ast, result) = run.unwrap();

        assert_eq!(
            format!("{ast}"),
            "(if ((1 > 0) and ((1 <= 0) or false)) then false else (if (if ((0 == 1) or (0 != 1)) then true else false) then (not false) else false))"
        );

        assert_eq!(result, DynValue::Boolean(true));
    }

    #[test]
    fn test_nested_expressions() {
        let program = "1 + if false then 2 else 3 + 4 + let test = 5 in test + 6";

        let run = run(program);
        assert!(run.is_ok());

        let (ast, result) = run.unwrap();

        assert_eq!(
            format!("{ast}"),
            "(1 + (if false then 2 else ((3 + 4) + (let test = 5 in (test + 6)))))"
        );

        assert_eq!(result, DynValue::Integer(19));
    }

    #[test]
    fn test_function_expression() {
        let program = "let add = (a, b) => a + b in add";

        let run = run(program);
        assert!(run.is_ok());

        let (ast, _) = run.unwrap();

        assert_eq!(format!("{ast}"), "(let add = (a, b) => ((a + b)) in add)");
    }

    #[test]
    fn test_function_application() {
        let program = "let add = (a, b) => a + b in add(1, 2)";

        let run = run(program);
        assert!(run.is_ok());

        let (ast, result) = run.unwrap();

        assert_eq!(
            format!("{ast}"),
            "(let add = (a, b) => ((a + b)) in (add(1, 2)))"
        );

        assert_eq!(result, DynValue::Integer(3));
    }

    #[test]
    fn test_function_application_has_highest_precedence() {
        let program = "
            let fn1 = () => (
                () => 1
            ) in
            -fn1()()
        ";

        let run = run(program);
        assert!(run.is_ok());

        let (ast, result) = run.unwrap();

        assert_eq!(
            format!("{ast}"),
            "(let fn1 = () => (() => (1)) in (-((fn1())())))"
        );

        assert_eq!(result, DynValue::Integer(-1));
    }

    #[test]
    fn test_function_application_has_static_scoping() {
        let program = "
            let fn1 = (
                let x = 1 in
                () => x
            ) in
            let x = 2 in
            fn1()
        ";

        let run = run(program);
        assert!(run.is_ok());

        let (_, result) = run.unwrap();

        assert_eq!(result, DynValue::Integer(1));
    }

    #[test]
    fn test_recursive_binding_works() {
        let program = "
            let rec fib = (n) =>
                if n <= 1 then
                    n
                else
                    fib(n - 1) + fib(n - 2)
            in
                fib(10)
        ";

        let run = run(program);
        assert!(run.is_ok());

        let (ast, result) = run.unwrap();

        assert_eq!(
            format!("{ast}"),
            "(let rec fib = (n) => ((if (n <= 1) then n else ((fib((n - 1))) + (fib((n - 2)))))) in (fib(10)))"
        );

        assert_eq!(result, DynValue::Integer(55));
    }
}
