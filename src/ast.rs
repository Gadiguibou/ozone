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
    /// Constructs an [ArithmeticExpr] while respecting operator precedence and associativity.
    pub fn from_pair(pair: Pair<Rule>) -> anyhow::Result<Expr> {
        if pair.as_rule() != Rule::expression {
            panic!("Did not pass in an arithmetic expression")
        }

        let inner = pair.into_inner().next().unwrap();

        match inner.as_rule() {
            Rule::conditional_expression => parse_conditional_expression(inner),
            Rule::infix_expression => parse_infix_expression(inner),
            _ => unreachable!(),
        }
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

    PREC_CLIMBER.climb(pair.into_inner(), parse_parenthesized_expression, infix)
}

/// Parses a [Rule::parenthesized_expression].
fn parse_parenthesized_expression(pair: Pair<Rule>) -> anyhow::Result<Expr> {
    let inner = unsafe { pair.into_inner().next().unwrap_unchecked() };
    match inner.as_rule() {
        Rule::expression => Expr::from_pair(inner),
        Rule::negation_expression => parse_negation_expression(inner),
        _ => unreachable!(),
    }
}

/// Parses a [Rule::negation_expression].
fn parse_negation_expression(pair: Pair<Rule>) -> anyhow::Result<Expr> {
    let mut inner = pair.into_inner();
    let (first, second) = (inner.next(), inner.next());
    // first is always present
    let first = first.unwrap();
    match first.as_rule() {
        Rule::minus => {
            let parenthesized_expression = parse_parenthesized_expression(second.unwrap())?;
            Ok(Expr::UnaryOp {
                op: UnaryOp::Negate,
                expr: Box::new(parenthesized_expression),
            })
        }
        Rule::not => {
            let parenthesized_expression = parse_parenthesized_expression(second.unwrap())?;
            Ok(Expr::UnaryOp {
                op: UnaryOp::Not,
                expr: Box::new(parenthesized_expression),
            })
        }
        Rule::literal => parse_literal(first),
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
