// use super::*;

// use ast::Ast;
// use interpreter::DynValue;
// // use typed_ast::Type;

// fn build_ast(contents: &str) -> anyhow::Result<Ast> {
//     let parse = Parser::parse(Rule::file, contents)?;

//     Ast::parse(parse)
// }

// fn eval<'a>(ast: &Ast) -> anyhow::Result<DynValue<'a>> {
//     let mut bindings = zero_copy_stack::ZeroCopyStack::new();
//     let mut handle = bindings.handle();
//     interpreter::eval(&ast.root, &mut handle)
// }

// // fn typecheck(ast: &Ast) -> anyhow::Result<typed_ast::TypedAst> {
// //     typed_ast::TypedAst::from(ast.clone())
// // }

// #[test]
// fn test_arithmetic_expression() {
//     let program = "3 - (2 + 2) + 4 * 2 ^ 2 ^ 3";

//     let ast = build_ast(program);
//     assert!(ast.is_ok());
//     let ast = ast.unwrap();

//     let result = eval(&ast);
//     assert!(result.is_ok());
//     let result = result.unwrap();

//     assert_eq!(
//         ast,
//         build_ast("((3 - (2 + 2)) + (4 * (2 ^ (2 ^ 3))))").unwrap()
//     );
//     assert_eq!(result, DynValue::Integer(1023));
// }

// #[test]
// fn test_binding_expression() {
//     let program = "
//         let test = 1 in
//         (let test = 0 in 0 /* Should not shadow the top-level binding */) + test
//         ";

//     let ast = build_ast(program);
//     assert!(ast.is_ok());
//     let ast = ast.unwrap();

//     let result = eval(&ast);
//     assert!(result.is_ok());
//     let result = result.unwrap();

//     assert_eq!(
//         ast,
//         build_ast("let test = 1 in ((let test = 0 in 0) + test)").unwrap()
//     );

//     assert_eq!(result, DynValue::Integer(1));
// }

// #[test]
// fn test_binding_expression_fails_on_unbound_variables() {
//     let program = "
//         let test =
//             let test2 = 2 in 1
//         in
//             test2
//         ";

//     let ast = build_ast(program);
//     assert!(ast.is_ok());
//     let ast = ast.unwrap();

//     let result = eval(&ast);
//     assert!(result.is_err());

//     assert_eq!(result.unwrap_err().to_string(), "Undefined variable: test2");
// }

// #[test]
// fn test_conditionals() {
//     let program = "
//         if 1 > 0 and (1 <= 0 or false) {
//             false
//         } else if /* Nested if's */ if 0 == 1 or 0 != 1 { true } else { false } {
//             not false /* Should evaluate to this branch */
//         } else {
//             false
//         }
//         ";

//     let ast = build_ast(program);
//     assert!(ast.is_ok());
//     let ast = ast.unwrap();

//     let result = eval(&ast);
//     assert!(result.is_ok());
//     let result = result.unwrap();

//     assert_eq!(
//         ast,
//         build_ast(
//             "
//             if ((1 > 0) and ((1 <= 0) or false)) {
//                 false
//             }
//             else if (if ((0 == 1) or (0 != 1)) { true } else { false }) {
//                 (not false)
//             }
//             else {
//                 false
//             }
//             "
//         )
//         .unwrap()
//     );

//     assert_eq!(result, DynValue::Boolean(true));
// }

// #[test]
// fn test_nested_expressions() {
//     let program = "1 + if false { 2 } else { 3 + 4 + let test = 5 in test + 6 }";

//     let ast = build_ast(program);
//     assert!(ast.is_ok());
//     let ast = ast.unwrap();

//     let result = eval(&ast);
//     assert!(result.is_ok());
//     let result = result.unwrap();

//     assert_eq!(
//         ast,
//         build_ast(
//             "1 + (
//                 if false { 2 } else { (3 + 4) + (
//                     let test = 5 in (test + 6)
//                 )}
//             )"
//         )
//         .unwrap()
//     );

//     assert_eq!(result, DynValue::Integer(19));
// }

// #[test]
// fn test_function_expression() {
//     let program = "let add = (a, b) => a + b in add";

//     let ast = build_ast(program);
//     assert!(ast.is_ok());
//     let ast = ast.unwrap();

//     let result = eval(&ast);
//     assert!(result.is_ok());

//     assert_eq!(
//         ast,
//         build_ast("(let add = (a, b) => (a + b) in add)").unwrap()
//     );
// }

// #[test]
// fn test_function_application() {
//     let program = "let add = (a, b) => a + b in add(1, 2)";

//     let ast = build_ast(program);
//     assert!(ast.is_ok());
//     let ast = ast.unwrap();

//     let result = eval(&ast);
//     assert!(result.is_ok());
//     let result = result.unwrap();

//     assert_eq!(
//         ast,
//         build_ast("(let add = (a, b) => ((a + b)) in (add(1, 2)))").unwrap()
//     );

//     assert_eq!(result, DynValue::Integer(3));
// }

// #[test]
// fn test_function_application_has_highest_precedence() {
//     let program = "
//         let fn1 = () => (
//             () => 1
//         ) in
//         -fn1()()
//         ";

//     let ast = build_ast(program);
//     assert!(ast.is_ok());
//     let ast = ast.unwrap();

//     let result = eval(&ast);
//     assert!(result.is_ok());
//     let result = result.unwrap();

//     assert_eq!(
//         ast,
//         build_ast("let fn1 = () => (() => (1)) in -((fn1())())").unwrap()
//     );

//     assert_eq!(result, DynValue::Integer(-1));
// }

// #[test]
// fn test_function_application_has_static_scoping() {
//     let program = "
//         let fn1 = (
//             let x = 1 in
//             () => x
//         ) in
//         let x = 2 in
//         fn1()
//         ";

//     let ast = build_ast(program);
//     assert!(ast.is_ok());
//     let ast = ast.unwrap();

//     let result = eval(&ast);
//     assert!(result.is_ok());
//     let result = result.unwrap();

//     assert_eq!(result, DynValue::Integer(1));
// }

// #[test]
// fn test_recursive_binding() {
//     let program = "
//         let rec fib = (n) =>
//             if n <= 1 {
//                 n
//             } else {
//                 fib(n - 1) + fib(n - 2)
//             }
//         in
//             fib(10)
//         ";

//     let ast = build_ast(program);
//     assert!(ast.is_ok());
//     let ast = ast.unwrap();

//     let result = eval(&ast);
//     assert!(result.is_ok());
//     let result = result.unwrap();

//     assert_eq!(
//         ast,
//         build_ast(
//             "let rec fib = (n) =>
//                     if (n <= 1) { n } else {
//                         fib(n - 1) + fib(n - 2)
//                     }
//             in fib(10)"
//         )
//         .unwrap()
//     );

//     assert_eq!(result, DynValue::Integer(55));
// }

// // #[test]
// // fn test_type_inference_unconstrained_binding() {
// //     let program = "
// //         let rec any = any in any
// //         ";

// //     let ast = build_ast(program);
// //     assert!(ast.is_ok());
// //     let ast = ast.unwrap();

// //     let typed_ast = typecheck(&ast);
// //     assert!(typed_ast.is_ok());
// //     let typed_ast = typed_ast.unwrap();
// //     assert!(matches!(typed_ast.root.ty.resolve(), Type::TypeVariable(_)))
// // }

// // #[test]
// // fn test_type_inference_function() {
// //     let program = "
// //         let rec fact = (n) => n*fact(n-1) in fact
// //         ";

// //     let ast = build_ast(program);
// //     assert!(ast.is_ok());
// //     let ast = ast.unwrap();

// //     let typed_ast = typecheck(&ast);
// //     assert!(typed_ast.is_ok());
// //     let typed_ast = typed_ast.unwrap();

// //     assert_eq!(
// //         typed_ast.root.ty,
// //         Type::Function {
// //             parameter_types: vec![Type::Integer],
// //             return_type: Box::new(Type::Integer)
// //         }
// //     );
// // }

// // #[test]
// // fn test_type_inference_function_application() {
// //     let program = "
// //         let rec fact = (n) => n*fact(n-1) in fact(10)
// //         ";

// //     let ast = build_ast(program);
// //     assert!(ast.is_ok());
// //     let ast = ast.unwrap();

// //     let typed_ast = typecheck(&ast);
// //     assert!(typed_ast.is_ok());
// //     let typed_ast = typed_ast.unwrap();

// //     assert_eq!(typed_ast.root.ty, Type::Integer);
// // }

// // #[test]
// // fn test_type_inference_conditional() {
// //     let program = "
// //         let rec x = x in if x then x else x
// //         ";

// //     let ast = build_ast(program);
// //     assert!(ast.is_ok());
// //     let ast = ast.unwrap();

// //     let typed_ast = typecheck(&ast);
// //     assert!(typed_ast.is_ok());
// //     let typed_ast = typed_ast.unwrap();

// //     assert_eq!(typed_ast.root.ty, Type::Boolean);
// // }

// // #[test]
// // fn test_type_inference_generic_function() {
// //     let program = "
// //         let eq = (x, y) => x == y in eq(1, 2) or eq(true, false)
// //         ";

// //     let ast = build_ast(program);
// //     assert!(ast.is_ok());
// //     let ast = ast.unwrap();

// //     let typed_ast = typecheck(&ast);
// //     assert!(typed_ast.is_ok());
// //     let typed_ast = typed_ast.unwrap();

// //     assert_eq!(typed_ast.root.ty, Type::Boolean);
// // }
