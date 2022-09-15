use std::fmt::Display;

use crate::parser::Rule;
use anyhow::anyhow;
use once_cell::sync::Lazy;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct Ast {
    pub root: Node,
}

impl TryFrom<Pairs<'_, Rule>> for Ast {
    type Error = anyhow::Error;

    fn try_from(mut pairs: Pairs<Rule>) -> anyhow::Result<Self> {
        let file = pairs
            .next()
            .ok_or_else(|| anyhow!("Passed in an empty iterator"))?;

        if file.as_rule() != Rule::file {
            panic!("Did not pass in a Rule::file")
        }

        let expression = unsafe {
            pairs
                // Get file
                .next()
                .unwrap_unchecked()
                // Get expression
                .into_inner()
                .next()
                .unwrap_unchecked()
        };

        Ok(Self {
            root: Node::try_from(expression)?,
        })
    }
}

// TODO: Add error reporting capabilities here by keeping the span associated with each node
#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum Node {
    Conditional {
        condition: Box<Node>,
        then_expr: Box<Node>,
        else_expr: Box<Node>,
    },
    Binding {
        name: String,
        recursive: bool,
        value: Box<Node>,
        body: Box<Node>,
    },
    BinaryOp {
        op: BinaryOp,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Node>,
    },
    FunctionApplication {
        function: Box<Node>,
        arguments: Vec<Node>,
    },
    Function {
        parameters: Vec<String>,
        body: Box<Node>,
    },
    Identifier(String),
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

impl TryFrom<Pair<'_, Rule>> for Node {
    type Error = anyhow::Error;

    /// Constructs a [Rule::expression] while respecting operator precedence and associativity.
    fn try_from(pair: Pair<Rule>) -> anyhow::Result<Node> {
        let inner = pair.into_inner().next().unwrap();

        match inner.as_rule() {
            Rule::prefix_expression => parse_prefix_expression(inner),
            Rule::infix_expression => parse_infix_expression(inner),
            _ => unreachable!(),
        }
    }
}

/// Parses a [Rule::prefix_expression].
fn parse_prefix_expression(pair: Pair<Rule>) -> anyhow::Result<Node> {
    let mut inner = pair.into_inner();
    let (first, second) = (inner.next(), inner.next());
    // first is always present
    let first = first.unwrap();
    match first.as_rule() {
        Rule::minus => {
            let subexpression = parse_postfix_expression(second.unwrap())?;
            Ok(Node::UnaryOp {
                op: UnaryOp::Negate,
                expr: Box::new(subexpression),
            })
        }
        Rule::not => {
            let subexpression = parse_postfix_expression(second.unwrap())?;
            Ok(Node::UnaryOp {
                op: UnaryOp::Not,
                expr: Box::new(subexpression),
            })
        }
        Rule::literal => parse_literal(first),
        Rule::identifier => parse_identifier(first),
        _ => unreachable!(),
    }
}

/// Parses a [Rule::infix_expression].
fn parse_infix_expression(pair: Pair<Rule>) -> anyhow::Result<Node> {
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
        lhs: anyhow::Result<Node>,
        op: Pair<Rule>,
        rhs: anyhow::Result<Node>,
    ) -> anyhow::Result<Node> {
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

        Ok(Node::BinaryOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }

    PREC_CLIMBER.climb(pair.into_inner(), parse_postfix_expression, infix)
}

/// Parses a [Rule::postfix_expression].
fn parse_postfix_expression(pair: Pair<Rule>) -> anyhow::Result<Node> {
    let inner = pair.into_inner().next().unwrap();

    match inner.as_rule() {
        Rule::function_application => parse_function_application(inner),
        Rule::subexpression => parse_subexpression(inner),
        _ => unreachable!(),
    }
}

/// Parses a [Rule::subexpression].
fn parse_subexpression(pair: Pair<Rule>) -> anyhow::Result<Node> {
    let inner = pair.into_inner().next().unwrap();

    match inner.as_rule() {
        Rule::infix_expression => parse_infix_expression(inner),
        Rule::conditional_expression => parse_conditional_expression(inner),
        Rule::binding_expression => parse_binding_expression(inner),
        Rule::parenthesized_expression => parse_parenthesized_expression(inner),
        Rule::function_expression => parse_function_expression(inner),
        Rule::identifier => parse_identifier(inner),
        Rule::literal => parse_literal(inner),
        _ => unreachable!(),
    }
}

/// Parses a [Rule::conditional_expression].
fn parse_conditional_expression(pair: Pair<Rule>) -> anyhow::Result<Node> {
    let mut inner = pair.into_inner();

    let condition = inner.next().unwrap();
    let then_expr = inner.next().unwrap();
    let else_expr = inner.next().unwrap();

    Ok(Node::Conditional {
        condition: Box::new(Node::try_from(condition)?),
        then_expr: Box::new(Node::try_from(then_expr)?),
        else_expr: Box::new(Node::try_from(else_expr)?),
    })
}

/// Parses a [Rule::binding_expression].
fn parse_binding_expression(pair: Pair<Rule>) -> anyhow::Result<Node> {
    let mut inner = pair.into_inner();

    let (recursive, name) = {
        let next = inner.next().unwrap();
        match next.as_rule() {
            Rule::rec => (true, inner.next().unwrap()),
            _ => (false, next),
        }
    };
    let value = inner.next().unwrap();
    let body = inner.next().unwrap();

    Ok(Node::Binding {
        name: name.as_str().to_string(),
        recursive,
        value: Box::new(Node::try_from(value)?),
        body: Box::new(Node::try_from(body)?),
    })
}

/// Parses a [Rule::parenthesized_expression].
fn parse_parenthesized_expression(pair: Pair<Rule>) -> anyhow::Result<Node> {
    let inner = unsafe { pair.into_inner().next().unwrap_unchecked() };
    match inner.as_rule() {
        Rule::expression => Node::try_from(inner),
        _ => unreachable!(),
    }
}

/// Parses a [Rule::function_expression].
fn parse_function_expression(pair: Pair<Rule>) -> anyhow::Result<Node> {
    let mut inner = pair.into_inner();

    let parameters = parse_parameter_list(inner.next().unwrap())?;
    let body = Node::try_from(inner.next().unwrap())?;

    Ok(Node::Function {
        parameters,
        body: Box::new(body),
    })
}

/// Parses a [Rule::function_application].
fn parse_function_application(pair: Pair<Rule>) -> anyhow::Result<Node> {
    let mut inner = pair.into_inner();

    let function = parse_subexpression(inner.next().unwrap())?;
    let argument_lists = inner
        .map(parse_argument_list)
        .collect::<anyhow::Result<Vec<_>>>()?;

    // "Left-associate" function applications.
    let function_application = argument_lists
        .into_iter()
        .fold(function, |function, arguments| Node::FunctionApplication {
            function: Box::new(function),
            arguments,
        });

    Ok(function_application)
}

/// Parses a [Rule::argument_list].
fn parse_argument_list(pair: Pair<Rule>) -> anyhow::Result<Vec<Node>> {
    pair.into_inner()
        .map(Node::try_from)
        .collect::<anyhow::Result<_>>()
}

/// Parses a [Rule::parameter_list].
fn parse_parameter_list(pair: Pair<Rule>) -> anyhow::Result<Vec<String>> {
    Ok(pair.into_inner().map(|p| p.as_str().to_string()).collect())
}

/// Parses a [Rule::literal].
fn parse_literal(pair: Pair<Rule>) -> anyhow::Result<Node> {
    let mut inner = pair.into_inner();
    let first = inner.next().unwrap();
    match first.as_rule() {
        Rule::integer => parse_integer(first),
        Rule::boolean => parse_boolean(first),
        _ => unreachable!(),
    }
}

/// Parses a [Rule::integer].
fn parse_integer(pair: Pair<Rule>) -> anyhow::Result<Node> {
    let s = pair.as_str();
    let integer: Result<i64, _> = s.chars().filter(|c| c != &'_').collect::<String>().parse();
    // All expressions composed of only ascii digits and underscores are valid integers (except for overflow)
    // TODO: Handle overflows gracefully
    Ok(Node::Integer(integer.unwrap()))
}

/// Parses a [Rule::boolean].
fn parse_boolean(pair: Pair<Rule>) -> anyhow::Result<Node> {
    let s = pair.as_str();
    let boolean = match s {
        "true" => true,
        "false" => false,
        _ => unreachable!(),
    };
    Ok(Node::Boolean(boolean))
}

/// Parses a [Rule::identifier].
fn parse_identifier(pair: Pair<Rule>) -> anyhow::Result<Node> {
    let s = pair.as_str();
    Ok(Node::Identifier(s.to_string()))
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Conditional {
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
            Node::Binding {
                name,
                recursive,
                value,
                body,
            } => {
                write!(
                    f,
                    "(let {}{} = {} in {})",
                    if *recursive { "rec " } else { "" },
                    name,
                    value,
                    body
                )
            }
            Node::BinaryOp { lhs, op, rhs } => write!(f, "({} {} {})", lhs, op, rhs),
            Node::UnaryOp { op, expr } => write!(f, "({}{})", op, expr),
            Node::FunctionApplication {
                function,
                arguments,
            } => {
                write!(
                    f,
                    "({}({}))",
                    function,
                    arguments
                        .iter()
                        .map(|s| format!("{s}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Node::Function { parameters, body } => {
                write!(f, "({}) => ({})", parameters.join(", "), body)
            }
            Node::Identifier(s) => write!(f, "{}", s),
            Node::Integer(i) => write!(f, "{}", i),
            Node::Boolean(b) => write!(f, "{}", b),
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
            UnaryOp::Not => write!(f, "not "),
        }
    }
}
