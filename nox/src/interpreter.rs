use anyhow::bail;
use zero_copy_stack::ZeroCopyStackHandle;

use crate::ast::{BinaryOp, Expr, UnaryOp};

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum DynValue {
    Integer(i64),
    Boolean(bool),
    Function {
        parameters: Vec<String>,
        body: Box<Expr>,
    },
}

pub type Bindings<'a> = ZeroCopyStackHandle<'a, Binding>;

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct Binding {
    name: String,
    value: DynValue,
    mutable: bool,
}

/// Evaluates an [Expr].
pub fn eval(expr: &Expr, bindings: &mut Bindings) -> anyhow::Result<DynValue> {
    use DynValue::*;

    match expr {
        Expr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => {
            let condition = eval(condition, bindings)?;
            let condition = match condition {
                Boolean(b) => b,
                _ => bail!("Condition must be a boolean"),
            };
            if condition {
                eval(then_expr, bindings)
            } else {
                eval(else_expr, bindings)
            }
        }
        Expr::Binding { name, value, body } => {
            let value = eval(value, bindings)?;
            let mut bindings = bindings.handle();
            bindings.push(Binding {
                name: name.clone(),
                value,
                mutable: false,
            });
            let body = eval(body, &mut bindings)?;
            Ok(body)
        }
        Expr::BinaryOp { op, lhs, rhs } => {
            let lhs = eval(lhs, bindings)?;
            let rhs = eval(rhs, bindings)?;

            use BinaryOp::*;
            match (lhs, op, rhs) {
                // Arithmetic
                (Integer(lhs), Plus, Integer(rhs)) => Ok(Integer(lhs + rhs)),
                (Integer(lhs), Minus, Integer(rhs)) => Ok(Integer(lhs - rhs)),
                (Integer(lhs), Times, Integer(rhs)) => Ok(Integer(lhs * rhs)),
                (Integer(lhs), Divide, Integer(rhs)) => Ok(Integer(lhs / rhs)),
                (Integer(lhs), Modulo, Integer(rhs)) => Ok(Integer(lhs % rhs)),
                (Integer(lhs), Power, Integer(rhs)) => {
                    if rhs < 0 {
                        bail!("Negative exponent: {rhs}")
                    }
                    if rhs > u32::MAX as i64 {
                        bail!("Exponent is {rhs} but must be less than {}", u32::MAX)
                    }
                    Ok(Integer(lhs.pow(rhs as u32)))
                }
                // Equality
                (Integer(lhs), Equal, Integer(rhs)) => Ok(Boolean(lhs == rhs)),
                (Integer(lhs), NotEqual, Integer(rhs)) => Ok(Boolean(lhs != rhs)),
                // Ordering
                (Integer(lhs), LessThan, Integer(rhs)) => Ok(Boolean(lhs < rhs)),
                (Integer(lhs), LessThanOrEqual, Integer(rhs)) => Ok(Boolean(lhs <= rhs)),
                (Integer(lhs), GreaterThan, Integer(rhs)) => Ok(Boolean(lhs > rhs)),
                (Integer(lhs), GreaterThanOrEqual, Integer(rhs)) => Ok(Boolean(lhs >= rhs)),
                // Logical
                (Boolean(lhs), And, Boolean(rhs)) => Ok(Boolean(lhs && rhs)),
                (Boolean(lhs), Or, Boolean(rhs)) => Ok(Boolean(lhs || rhs)),
                // Type error
                (lhs, op, rhs) => Err(anyhow::anyhow!(
                    "Cannot apply {:?} to {:?} and {:?}",
                    op,
                    lhs,
                    rhs
                )),
            }
        }
        Expr::UnaryOp { op, expr } => {
            let expr = eval(expr, bindings)?;

            use UnaryOp::*;
            match (op, expr) {
                (Negate, Integer(expr)) => Ok(Integer(-expr)),
                (Not, Boolean(expr)) => Ok(Boolean(!expr)),
                (op, expr) => Err(anyhow::anyhow!("Cannot apply {:?} to {:?}", op, expr)),
            }
        }
        Expr::Function { parameters, body } => Ok(Function {
            parameters: parameters.clone(),
            body: body.clone(),
        }),
        Expr::Identifier(identifier) => {
            let binding = bindings
                .find(|binding| binding.name == *identifier)
                .ok_or_else(|| anyhow::anyhow!("Undefined variable: {}", identifier))?;
            Ok(binding.value.clone())
        }
        Expr::Integer(integer) => Ok(Integer(*integer)),
        Expr::Boolean(boolean) => Ok(Boolean(*boolean)),
    }
}
