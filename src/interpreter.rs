use crate::ast::{ArithmeticExpr, BinaryOp, UnaryOp};

/// Evaluates an [ArithmeticExpr].
pub fn eval(expr: &ArithmeticExpr) -> i64 {
    match expr {
        ArithmeticExpr::BinaryOp { op, lhs, rhs } => {
            let lhs = eval(lhs);
            let rhs = eval(rhs);
            match op {
                BinaryOp::Plus => lhs + rhs,
                BinaryOp::Minus => lhs - rhs,
                BinaryOp::Times => lhs * rhs,
                BinaryOp::Divide => lhs / rhs,
                BinaryOp::Modulo => lhs % rhs,
                BinaryOp::Power => {
                    if rhs < 0 {
                        panic!("negative exponent: {rhs}")
                    }
                    if rhs > u32::MAX as i64 {
                        panic!("exponent is {rhs} but must be less than {}", u32::MAX)
                    }
                    lhs.pow(rhs as u32)
                }
            }
        }
        ArithmeticExpr::UnaryOp { op, expr } => {
            let expr = eval(expr);
            match op {
                UnaryOp::Negate => -expr,
            }
        }
        ArithmeticExpr::Literal(integer) => *integer,
    }
}
