use std::fmt::Display;

use crate::parser::Rule;
use once_cell::sync::Lazy;
use pest::iterators::Pair;
use pest::prec_climber::{Assoc, Operator, PrecClimber};

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum Expr {
    Conditional {
        condition: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    Binding {
        name: String,
        value: Box<Expr>,
        body: Box<Expr>,
    },
    BinaryOp {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Integer(i64),
    Boolean(bool),
    Identifier(String),
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum BinaryOp {
    // Logical
    Or,
    And,
    // Equality
    Equal,
    NotEqual,
    // Ordering
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    // Arithmetic
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
    Not,
}

impl Expr {
    /// Constructs an [Rule::expression] while respecting operator precedence and associativity.
    pub fn from_pair(pair: Pair<Rule>) -> anyhow::Result<Expr> {
        if pair.as_rule() != Rule::expression {
            panic!("Did not pass in an expression")
        }

        let inner = pair.into_inner().next().unwrap();

        match inner.as_rule() {
            Rule::prefix_expression => parse_prefix_expression(inner),
            Rule::infix_expression => parse_infix_expression(inner),
            _ => unreachable!(),
        }
    }
}

/// Parses a [Rule::subexpression].
fn parse_subexpression(pair: Pair<Rule>) -> anyhow::Result<Expr> {
    let inner = pair.into_inner().next().unwrap();

    match inner.as_rule() {
        Rule::infix_expression => parse_infix_expression(inner),
        Rule::conditional_expression => parse_conditional_expression(inner),
        Rule::binding_expression => parse_binding_expression(inner),
        Rule::parenthesized_expression => parse_parenthesized_expression(inner),
        Rule::prefix_expression => parse_prefix_expression(inner),
        Rule::identifier => parse_identifier(inner),
        Rule::literal => parse_literal(inner),
        _ => unreachable!(),
    }
}

/// Parses a [Rule::conditional_expression].
fn parse_conditional_expression(pair: Pair<Rule>) -> anyhow::Result<Expr> {
    let mut inner = pair.into_inner();

    let condition = inner.next().unwrap();
    let then_expr = inner.next().unwrap();
    let else_expr = inner.next().unwrap();

    Ok(Expr::Conditional {
        condition: Box::new(Expr::from_pair(condition)?),
        then_expr: Box::new(Expr::from_pair(then_expr)?),
        else_expr: Box::new(Expr::from_pair(else_expr)?),
    })
}

/// Parses a [Rule::binding_expression].
fn parse_binding_expression(pair: Pair<Rule>) -> anyhow::Result<Expr> {
    let mut inner = pair.into_inner();

    let name = inner.next().unwrap();
    let value = inner.next().unwrap();
    let body = inner.next().unwrap();

    Ok(Expr::Binding {
        name: name.as_str().to_string(),
        value: Box::new(Expr::from_pair(value)?),
        body: Box::new(Expr::from_pair(body)?),
    })
}

/// Parses a [Rule::infix_expression].
fn parse_infix_expression(pair: Pair<Rule>) -> anyhow::Result<Expr> {
    // Precedence increases with the index in the Vec
    static PREC_CLIMBER: Lazy<PrecClimber<Rule>> = Lazy::new(|| {
        PrecClimber::new(vec![
            Operator::new(Rule::or, Assoc::Left),
            Operator::new(Rule::and, Assoc::Left),
            Operator::new(Rule::equal, Assoc::Left)
                | Operator::new(Rule::not_equal, Assoc::Left)
                | Operator::new(Rule::less_than, Assoc::Left)
                | Operator::new(Rule::less_than_or_equal, Assoc::Left)
                | Operator::new(Rule::greater_than, Assoc::Left)
                | Operator::new(Rule::greater_than_or_equal, Assoc::Left),
            Operator::new(Rule::plus, Assoc::Left) | Operator::new(Rule::minus, Assoc::Left),
            Operator::new(Rule::times, Assoc::Left)
                | Operator::new(Rule::divide, Assoc::Left)
                | Operator::new(Rule::modulo, Assoc::Left),
            Operator::new(Rule::power, Assoc::Right),
        ])
    });

    fn infix(
        lhs: anyhow::Result<Expr>,
        op: Pair<Rule>,
        rhs: anyhow::Result<Expr>,
    ) -> anyhow::Result<Expr> {
        let lhs = lhs?;
        let rhs = rhs?;

        let op = match op.as_rule() {
            // Logical
            Rule::or => BinaryOp::Or,
            Rule::and => BinaryOp::And,
            // Equality
            Rule::equal => BinaryOp::Equal,
            Rule::not_equal => BinaryOp::NotEqual,
            // Ordering
            Rule::less_than => BinaryOp::LessThan,
            Rule::less_than_or_equal => BinaryOp::LessThanOrEqual,
            Rule::greater_than => BinaryOp::GreaterThan,
            Rule::greater_than_or_equal => BinaryOp::GreaterThanOrEqual,
            // Arithmetic
            Rule::plus => BinaryOp::Plus,
            Rule::minus => BinaryOp::Minus,
            Rule::times => BinaryOp::Times,
            Rule::divide => BinaryOp::Divide,
            Rule::modulo => BinaryOp::Modulo,
            Rule::power => BinaryOp::Power,
            _ => unreachable!(),
        };

        Ok(Expr::BinaryOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }

    PREC_CLIMBER.climb(pair.into_inner(), parse_subexpression, infix)
}

/// Parses a [Rule::parenthesized_expression].
fn parse_parenthesized_expression(pair: Pair<Rule>) -> anyhow::Result<Expr> {
    let inner = unsafe { pair.into_inner().next().unwrap_unchecked() };
    match inner.as_rule() {
        Rule::expression => Expr::from_pair(inner),
        _ => unreachable!(),
    }
}

/// Parses a [Rule::prefix_expression].
fn parse_prefix_expression(pair: Pair<Rule>) -> anyhow::Result<Expr> {
    let mut inner = pair.into_inner();
    let (first, second) = (inner.next(), inner.next());
    // first is always present
    let first = first.unwrap();
    match first.as_rule() {
        Rule::minus => {
            let subexpression = parse_subexpression(second.unwrap())?;
            Ok(Expr::UnaryOp {
                op: UnaryOp::Negate,
                expr: Box::new(subexpression),
            })
        }
        Rule::not => {
            let subexpression = parse_subexpression(second.unwrap())?;
            Ok(Expr::UnaryOp {
                op: UnaryOp::Not,
                expr: Box::new(subexpression),
            })
        }
        Rule::literal => parse_literal(first),
        Rule::identifier => parse_identifier(first),
        _ => unreachable!(),
    }
}

/// Parses a [Rule::literal].
fn parse_literal(pair: Pair<Rule>) -> anyhow::Result<Expr> {
    let mut inner = pair.into_inner();
    let first = inner.next().unwrap();
    match first.as_rule() {
        Rule::integer => parse_integer(first),
        Rule::boolean => parse_boolean(first),
        _ => unreachable!(),
    }
}

/// Parses a [Rule::integer].
fn parse_integer(pair: Pair<Rule>) -> anyhow::Result<Expr> {
    let s = pair.as_str();
    let integer: Result<i64, _> = s.chars().filter(|c| c != &'_').collect::<String>().parse();
    // All expressions composed of only ascii digits and underscores are valid integers (except for overflow)
    // TODO: Handle overflows gracefully
    Ok(Expr::Integer(integer.unwrap()))
}

/// Parses a [Rule::boolean].
fn parse_boolean(pair: Pair<Rule>) -> anyhow::Result<Expr> {
    let s = pair.as_str();
    let boolean = match s {
        "true" => true,
        "false" => false,
        _ => unreachable!(),
    };
    Ok(Expr::Boolean(boolean))
}

/// Parses a [Rule::identifier].
fn parse_identifier(pair: Pair<Rule>) -> anyhow::Result<Expr> {
    let s = pair.as_str();
    Ok(Expr::Identifier(s.to_string()))
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Conditional {
                condition,
                then_expr,
                else_expr,
            } => {
                write!(
                    f,
                    "(if {} then {} else {})",
                    condition, then_expr, else_expr
                )
            }
            Expr::Binding { name, value, body } => {
                write!(f, "(let {} = {} in {})", name, value, body)
            }
            Expr::BinaryOp { lhs, op, rhs } => write!(f, "({} {} {})", lhs, op, rhs),
            Expr::UnaryOp { op, expr } => write!(f, "({} {})", op, expr),
            Expr::Identifier(s) => write!(f, "{}", s),
            Expr::Integer(i) => write!(f, "{}", i),
            Expr::Boolean(b) => write!(f, "{}", b),
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Or => write!(f, "or"),
            BinaryOp::And => write!(f, "and"),
            BinaryOp::Equal => write!(f, "=="),
            BinaryOp::NotEqual => write!(f, "!="),
            BinaryOp::LessThan => write!(f, "<"),
            BinaryOp::LessThanOrEqual => write!(f, "<="),
            BinaryOp::GreaterThan => write!(f, ">"),
            BinaryOp::GreaterThanOrEqual => write!(f, ">="),
            BinaryOp::Plus => write!(f, "+"),
            BinaryOp::Minus => write!(f, "-"),
            BinaryOp::Times => write!(f, "*"),
            BinaryOp::Divide => write!(f, "/"),
            BinaryOp::Modulo => write!(f, "%"),
            BinaryOp::Power => write!(f, "^"),
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Negate => write!(f, "-"),
            UnaryOp::Not => write!(f, "not"),
        }
    }
}
