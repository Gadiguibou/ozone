use anyhow::bail;

use crate::ast::{BinaryOp, Expr, UnaryOp};

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum DynValue {
    Integer(i64),
    Boolean(bool),
}

/// Evaluates an [ArithmeticExpr].
pub fn eval(expr: &Expr) -> anyhow::Result<DynValue> {
    use DynValue::*;

    match expr {
        Expr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => {
            let condition = eval(condition)?;
            let condition = match condition {
                Boolean(b) => b,
                Integer(_) => bail!("Condition must be a boolean"),
            };
            if condition {
                eval(then_expr)
            } else {
                eval(else_expr)
            }
        }
        Expr::BinaryOp { op, lhs, rhs } => {
            let lhs = eval(lhs)?;
            let rhs = eval(rhs)?;

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
            let expr = eval(expr)?;

            use UnaryOp::*;
            match (op, expr) {
                (Negate, Integer(expr)) => Ok(Integer(-expr)),
                (Not, Boolean(expr)) => Ok(Boolean(!expr)),
                (op, expr) => Err(anyhow::anyhow!("Cannot apply {:?} to {:?}", op, expr)),
            }
        }
        Expr::Integer(integer) => Ok(Integer(*integer)),
        Expr::Boolean(boolean) => Ok(Boolean(*boolean)),
    }
}
