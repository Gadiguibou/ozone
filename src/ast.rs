use crate::parser::Rule;
use lazy_static::lazy_static;
use pest::iterators::Pair;
use pest::prec_climber::{Assoc, Operator, PrecClimber};

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum ArithmeticExpr {
    BinaryOp {
        op: BinaryOp,
        lhs: Box<ArithmeticExpr>,
        rhs: Box<ArithmeticExpr>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<ArithmeticExpr>,
    },
    Literal(i64),
}

impl ArithmeticExpr {
    /// Constructs an [ArithmeticExpr] while respecting operator precedence and associativity.
    pub fn from_pair(pair: Pair<Rule>) -> anyhow::Result<ArithmeticExpr> {
        if pair.as_rule() != Rule::arithmetic_expression {
            panic!("Did not pass in an arithmetic expression")
        }

        lazy_static! {
            // Precedence increases with the index in the Vec
            static ref PREC_CLIMBER: PrecClimber<Rule> = PrecClimber::new(vec![
                Operator::new(Rule::plus, Assoc::Left) | Operator::new(Rule::minus, Assoc::Left),
                Operator::new(Rule::times, Assoc::Left)
                    | Operator::new(Rule::divide, Assoc::Left)
                    | Operator::new(Rule::modulo, Assoc::Left),
                Operator::new(Rule::power, Assoc::Right),
            ]);
        }

        /// Parses a [Rule::negation_expression].
        fn primary(pair: Pair<Rule>) -> anyhow::Result<ArithmeticExpr> {
            // In the case where there is a negation, a negation_expression contains another negation_expression.
            // Otherwise, it only contains an integer literal.

            // Recursively evaluate the negation expression to find its final negation status.
            let mut inner = unsafe { pair.into_inner().next().unwrap_unchecked() };
            let mut negative = false;
            loop {
                match inner.as_rule() {
                    Rule::integer => {
                        let integer = parse_integer(inner.as_str());
                        break if negative {
                            Ok(ArithmeticExpr::UnaryOp {
                                op: UnaryOp::Negate,
                                expr: Box::new(ArithmeticExpr::Literal(integer)),
                            })
                        } else {
                            Ok(ArithmeticExpr::Literal(integer))
                        };
                    }
                    Rule::negation_expression => {
                        inner = unsafe { inner.into_inner().next().unwrap_unchecked() };
                        negative = !negative;
                    }
                    _ => unreachable!(),
                }
            }
        }

        fn infix(
            lhs: anyhow::Result<ArithmeticExpr>,
            op: Pair<Rule>,
            rhs: anyhow::Result<ArithmeticExpr>,
        ) -> anyhow::Result<ArithmeticExpr> {
            let lhs = lhs?;
            let rhs = rhs?;

            let op = match op.as_rule() {
                Rule::plus => BinaryOp::Plus,
                Rule::minus => BinaryOp::Minus,
                Rule::times => BinaryOp::Times,
                Rule::divide => BinaryOp::Divide,
                Rule::modulo => BinaryOp::Modulo,
                Rule::power => BinaryOp::Power,
                _ => unreachable!(),
            };

            Ok(ArithmeticExpr::BinaryOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
        }

        PREC_CLIMBER.climb(pair.into_inner(), primary, infix)
    }
}

/// Parses a string from a [Rule::integer].
fn parse_integer(s: &str) -> i64 {
    let result = s.chars().filter(|c| c != &'_').collect::<String>().parse();
    // All expressions composed of only ascii digits and underscores are valid integers (except for overflow)
    // TODO: Handle overflows gracefully
    result.unwrap()
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum BinaryOp {
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
    Power,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum UnaryOp {
    Negate,
}
