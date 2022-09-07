use crate::parser::Rule;
use once_cell::sync::Lazy;
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

        // Precedence increases with the index in the Vec
        static PREC_CLIMBER: Lazy<PrecClimber<Rule>> = Lazy::new(|| {
            PrecClimber::new(vec![
                Operator::new(Rule::plus, Assoc::Left) | Operator::new(Rule::minus, Assoc::Left),
                Operator::new(Rule::times, Assoc::Left)
                    | Operator::new(Rule::divide, Assoc::Left)
                    | Operator::new(Rule::modulo, Assoc::Left),
                Operator::new(Rule::power, Assoc::Right),
            ])
        });

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

        PREC_CLIMBER.climb(pair.into_inner(), parse_parenthesized_expression, infix)
    }
}

/// Parses a [Rule::parenthesized_expression].
fn parse_parenthesized_expression(pair: Pair<Rule>) -> anyhow::Result<ArithmeticExpr> {
    let inner = unsafe { pair.into_inner().next().unwrap_unchecked() };
    match inner.as_rule() {
        Rule::arithmetic_expression => ArithmeticExpr::from_pair(inner),
        Rule::negation_expression => parse_negation_expression(inner),
        _ => unreachable!(),
    }
}

/// Parses a [Rule::negation_expression].
fn parse_negation_expression(pair: Pair<Rule>) -> anyhow::Result<ArithmeticExpr> {
    let inner = unsafe { pair.into_inner().next().unwrap_unchecked() };
    match inner.as_rule() {
        Rule::parenthesized_expression => {
            let parenthesized_expression = parse_parenthesized_expression(inner)?;
            Ok(ArithmeticExpr::UnaryOp {
                op: UnaryOp::Negate,
                expr: Box::new(parenthesized_expression),
            })
        }
        Rule::integer => parse_integer(inner),
        _ => unreachable!(),
    }
}

/// Parses a string from a [Rule::integer].
fn parse_integer(pair: Pair<Rule>) -> anyhow::Result<ArithmeticExpr> {
    let s = pair.as_str();
    let integer: Result<i64, _> = s.chars().filter(|c| c != &'_').collect::<String>().parse();
    // All expressions composed of only ascii digits and underscores are valid integers (except for overflow)
    // TODO: Handle overflows gracefully
    Ok(ArithmeticExpr::Literal(integer.unwrap()))
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
