// use std::fmt::Display;

// use anyhow::bail;
// use zero_copy_stack::{ZeroCopyStack, ZeroCopyStackHandle};

// use crate::ast::{BinaryOp, IfClause, Node, NodeKind, UnaryOp};

// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
// pub enum DynValue<'a> {
//     Unit,
//     Integer(i64),
//     Boolean(bool),
//     Function {
//         parameters: Vec<String>,
//         captured_bindings: ZeroCopyStack<Binding<'a>>,
//         body: Box<Node<'a>>,
//     },
//     /// Used in recursive bindings to allow referencing the binding itself.
//     Lazy(Box<Node<'a>>),
// }

// impl Display for DynValue<'_> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             DynValue::Unit => write!(f, "()"),
//             DynValue::Integer(i) => write!(f, "{}", i),
//             DynValue::Boolean(b) => write!(f, "{}", b),
//             DynValue::Function { parameters, .. } => {
//                 write!(f, "({}) => (<body>)", parameters.join(", "))
//             }
//             DynValue::Lazy(_) => write!(f, "<lazy>"),
//         }
//     }
// }

// pub type Bindings<'a> = ZeroCopyStackHandle<'a, Binding<'a>>;

// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
// pub struct Binding<'a> {
//     name: String,
//     value: DynValue<'a>,
//     mutable: bool,
// }

// /// Evaluates an [ast::Node].
// pub fn eval<'a>(expr: Node<'a>, bindings: &mut Bindings<'a>) -> anyhow::Result<DynValue> {
//     use DynValue::*;

//     match expr.kind() {
//         NodeKind::Conditional {
//             if_clauses,
//             else_clause,
//         } => {
//             for IfClause { condition, body } in if_clauses {
//                 let condition = eval(condition, bindings)?;
//                 if let Boolean(true) = condition {
//                     return eval(body, bindings);
//                 }
//             }
//             if let Some(else_clause) = else_clause {
//                 eval(else_clause, bindings)
//             } else {
//                 Ok(Unit)
//             }
//         }
//         NodeKind::Binding {
//             name,
//             recursive,
//             value,
//             body,
//         } => {
//             let value = if *recursive {
//                 let mut bindings = bindings.handle();
//                 bindings.push(Binding {
//                     name: name.clone(),
//                     value: Lazy(value.clone()),
//                     mutable: false,
//                 });
//                 eval(value, &mut bindings)?
//             } else {
//                 eval(value, bindings)?
//             };
//             let mut bindings = bindings.handle();
//             bindings.push(Binding {
//                 name: name.clone(),
//                 value,
//                 mutable: false,
//             });
//             if let Some(body) = body {
//                 Ok(eval(body, &mut bindings)?)
//             } else {
//                 Ok(Unit)
//             }
//         }
//         NodeKind::BinaryOp { op, lhs, rhs } => {
//             let lhs = eval(lhs, bindings)?;
//             let rhs = eval(rhs, bindings)?;

//             use BinaryOp::*;
//             match (lhs, op, rhs) {
//                 // Arithmetic
//                 (Integer(lhs), Plus, Integer(rhs)) => Ok(Integer(lhs + rhs)),
//                 (Integer(lhs), Minus, Integer(rhs)) => Ok(Integer(lhs - rhs)),
//                 (Integer(lhs), Times, Integer(rhs)) => Ok(Integer(lhs * rhs)),
//                 (Integer(lhs), Divide, Integer(rhs)) => Ok(Integer(lhs / rhs)),
//                 (Integer(lhs), Modulo, Integer(rhs)) => Ok(Integer(lhs % rhs)),
//                 (Integer(lhs), Power, Integer(rhs)) => {
//                     if rhs < 0 {
//                         bail!("Negative exponent: {rhs}")
//                     }
//                     if rhs > u32::MAX as i64 {
//                         bail!("Exponent is {rhs} but must be less than {}", u32::MAX)
//                     }
//                     Ok(Integer(lhs.pow(rhs as u32)))
//                 }
//                 // Equality
//                 // TODO: This is missing equality for custom types.
//                 (Integer(lhs), Equal, Integer(rhs)) => Ok(Boolean(lhs == rhs)),
//                 (Integer(lhs), NotEqual, Integer(rhs)) => Ok(Boolean(lhs != rhs)),
//                 (Boolean(lhs), Equal, Boolean(rhs)) => Ok(Boolean(lhs == rhs)),
//                 (Boolean(lhs), NotEqual, Boolean(rhs)) => Ok(Boolean(lhs != rhs)),
//                 // Ordering
//                 (Integer(lhs), LessThan, Integer(rhs)) => Ok(Boolean(lhs < rhs)),
//                 (Integer(lhs), LessThanOrEqual, Integer(rhs)) => Ok(Boolean(lhs <= rhs)),
//                 (Integer(lhs), GreaterThan, Integer(rhs)) => Ok(Boolean(lhs > rhs)),
//                 (Integer(lhs), GreaterThanOrEqual, Integer(rhs)) => Ok(Boolean(lhs >= rhs)),
//                 // Logical
//                 (Boolean(lhs), And, Boolean(rhs)) => Ok(Boolean(lhs && rhs)),
//                 (Boolean(lhs), Or, Boolean(rhs)) => Ok(Boolean(lhs || rhs)),
//                 // Type error
//                 (lhs, op, rhs) => Err(anyhow::anyhow!(
//                     "Cannot apply {:?} to {:?} and {:?}",
//                     op,
//                     lhs,
//                     rhs
//                 )),
//             }
//         }
//         NodeKind::UnaryOp { op, expr } => {
//             let expr = eval(expr, bindings)?;

//             use UnaryOp::*;
//             match (op, expr) {
//                 (Negate, Integer(expr)) => Ok(Integer(-expr)),
//                 (Not, Boolean(expr)) => Ok(Boolean(!expr)),
//                 (op, expr) => Err(anyhow::anyhow!("Cannot apply {:?} to {:?}", op, expr)),
//             }
//         }
//         NodeKind::FunctionApplication {
//             function,
//             arguments,
//         } => {
//             let function = eval(function, bindings)?;

//             if !matches!(&function, DynValue::Function { .. }) {
//                 bail!("`{}` is not a function", function)
//             }

//             let arguments = arguments
//                 .iter()
//                 .map(|arg| eval(arg, bindings))
//                 .collect::<anyhow::Result<Vec<_>>>()?;

//             match function {
//                 DynValue::Function {
//                     parameters,
//                     mut captured_bindings,
//                     body,
//                 } => {
//                     if parameters.len() != arguments.len() {
//                         bail!(
//                             "Expected {} arguments but got {}",
//                             parameters.len(),
//                             arguments.len()
//                         )
//                     }

//                     let mut bindings = captured_bindings.handle();
//                     for (parameter, argument) in parameters.into_iter().zip(arguments) {
//                         bindings.push(Binding {
//                             name: parameter,
//                             value: argument,
//                             mutable: false,
//                         });
//                     }

//                     eval(&*body, &mut bindings)
//                 }
//                 _ => unreachable!(),
//             }
//         }
//         NodeKind::Function { parameters, body } => {
//             // TODO: Capture only the bindings which are used in the function body.
//             let captured_bindings = bindings.iter().map(Clone::clone).collect();
//             Ok(Function {
//                 parameters: parameters.clone(),
//                 captured_bindings,
//                 body: body.clone(),
//             })
//         }
//         NodeKind::Identifier(identifier) => {
//             let binding = bindings
//                 .find(|binding| binding.name == *identifier)
//                 .ok_or_else(|| anyhow::anyhow!("Undefined variable: {}", identifier))?;

//             let value = binding.value.clone();

//             match value {
//                 Lazy(expr) => eval(&*expr, bindings),
//                 _ => Ok(value),
//             }
//         }
//         NodeKind::Integer(integer) => Ok(Integer(*integer)),
//         NodeKind::Boolean(boolean) => Ok(Boolean(*boolean)),
//     }
// }
