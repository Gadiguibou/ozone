use std::cell::RefCell;
use std::rc::Rc;

use crate::parser::Rule;
use anyhow::{bail, Result};
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::{Assoc, Op, PrattParser};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ast<'a> {
    pub root: Node<'a>,
}

type Bindings<'a> = Rc<RefCell<Vec<&'a str>>>;

impl<'a> Ast<'a> {
    pub fn parse(mut pairs: Pairs<'a, Rule>) -> Result<Ast<'a>> {
        let file = pairs
            .next()
            .unwrap_or_else(|| panic!("Passed in an empty iterator"));

        debug_assert_eq!(file.as_rule(), Rule::file);

        let expression = file
            // Get expression
            .into_inner()
            .next()
            .unwrap();

        let bindings: Bindings<'a> = Rc::new(RefCell::new(Vec::new()));

        Ok(Self {
            root: parse_expression(expression, bindings)?,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node<'a> {
    pub kind: NodeKind<'a, Node<'a>>,
    pub span: pest::Span<'a>,
}

// TODO: Add error reporting capabilities here by keeping the span associated with each node
// NodeKind is generic over the concrete Node being used since this type is used in both the untyped and the typed AST.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeKind<'a, T>
where
    T: std::fmt::Debug + Clone + Eq,
{
    Conditional {
        if_clauses: Vec<IfClause<T>>,
        else_clause: Option<Box<T>>,
    },
    Binding {
        name: &'a str,
        recursive: bool,
        value: Box<T>,
        body: Option<Box<T>>,
    },
    BinaryOp {
        op: BinaryOp,
        lhs: Box<T>,
        rhs: Box<T>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<T>,
    },
    FunctionApplication {
        function: Box<T>,
        arguments: Vec<T>,
    },
    Function {
        parameters: Vec<&'a str>,
        body: Box<T>,
    },
    Identifier(&'a str, usize),
    Integer(i64),
    Boolean(bool),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfClause<T>
where
    T: std::fmt::Debug + Clone + Eq,
{
    pub condition: T,
    pub body: T,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Negate,
    Not,
}

fn parse_expression<'a>(pair: Pair<'a, Rule>, bindings: Bindings<'a>) -> Result<Node<'a>> {
    debug_assert_eq!(pair.as_rule(), Rule::expression);

    thread_local! {
        static PRATT_PARSER: PrattParser<Rule> =
            PrattParser::new()
                .op(Op::infix(Rule::or, Assoc::Right))
                .op(Op::infix(Rule::and, Assoc::Right))
                .op(
                    Op::infix(Rule::equal, Assoc::Left)
                    | Op::infix(Rule::not_equal, Assoc::Left)
                    | Op::infix(Rule::less_than, Assoc::Left)
                    | Op::infix(Rule::less_than_or_equal, Assoc::Left)
                    | Op::infix(Rule::greater_than, Assoc::Left)
                    | Op::infix(Rule::greater_than_or_equal, Assoc::Left)
                )
                .op(Op::infix(Rule::plus, Assoc::Left) | Op::infix(Rule::minus, Assoc::Left))
                .op(
                    Op::infix(Rule::times, Assoc::Left)
                        | Op::infix(Rule::divide, Assoc::Left)
                        | Op::infix(Rule::modulo, Assoc::Left)
                )
                .op(Op::infix(Rule::power, Assoc::Right))
                .op(Op::prefix(Rule::unary_minus) | Op::prefix(Rule::not))
                .op(Op::postfix(Rule::function_application))
    }

    let bindings_starting_len = bindings.borrow().len();

    let expression = PRATT_PARSER.with(|parser| {
        let bindings_clone_1 = bindings.clone();
        let bindings_clone_2 = bindings.clone();

        parser
            .map_primary(move |primary| parse_primary(primary, bindings_clone_1.clone()))
            .map_prefix(parse_prefix)
            .map_postfix(move |lhs, op| parse_postfix(lhs, op, bindings_clone_2.clone()))
            .map_infix(parse_infix)
            .parse(pair.into_inner())
    });

    bindings.borrow_mut().truncate(bindings_starting_len);

    expression
}

fn parse_primary<'a>(primary: Pair<'a, Rule>, bindings: Bindings<'a>) -> Result<Node<'a>> {
    match primary.as_rule() {
        Rule::conditional_expression => parse_conditional(primary, bindings),
        Rule::binding_expression => parse_binding(primary, bindings),
        Rule::function_expression => parse_function_expression(primary, bindings),
        Rule::parenthesized_expression => {
            let inner = primary.into_inner().next().unwrap();
            parse_expression(inner, bindings)
        }
        Rule::identifier => parse_identifier(primary, bindings),
        Rule::literal => parse_literal(primary),
        _ => unreachable!(),
    }
}

fn parse_conditional<'a>(conditional: Pair<'a, Rule>, bindings: Bindings<'a>) -> Result<Node<'a>> {
    debug_assert_eq!(conditional.as_rule(), Rule::conditional_expression);
    let span = conditional.as_span();
    let pairs = conditional.into_inner();
    let mut if_clauses = Vec::new();

    for pair in pairs {
        match pair.as_rule() {
            Rule::if_clause => if_clauses.push(parse_if_clause(pair, bindings.clone())?),
            Rule::else_clause => {
                let else_clause = parse_expression(pair.into_inner().next().unwrap(), bindings)?;
                return Ok(Node {
                    kind: NodeKind::Conditional {
                        if_clauses,
                        else_clause: Some(Box::new(else_clause)),
                    },
                    span,
                });
            }
            _ => unreachable!(),
        }
    }

    Ok(Node {
        kind: NodeKind::Conditional {
            if_clauses,
            else_clause: None,
        },
        span,
    })
}

fn parse_if_clause<'a>(
    if_clause: Pair<'a, Rule>,
    bindings: Bindings<'a>,
) -> Result<IfClause<Node<'a>>> {
    debug_assert_eq!(if_clause.as_rule(), Rule::if_clause);
    let mut pairs = if_clause.into_inner();
    let condition = parse_expression(pairs.next().unwrap(), bindings.clone())?;
    let body = parse_expression(pairs.next().unwrap(), bindings)?;
    Ok(IfClause { condition, body })
}

fn parse_binding<'a>(binding: Pair<'a, Rule>, bindings: Bindings<'a>) -> Result<Node<'a>> {
    debug_assert_eq!(binding.as_rule(), Rule::binding_expression);
    let span = binding.as_span();
    let mut pairs = binding.into_inner();
    let recursive = pairs.next().unwrap().as_rule();
    let recursive = match recursive {
        Rule::recursive => true,
        Rule::non_recursive => false,
        _ => unreachable!(),
    };
    let name = pairs.next().unwrap().as_str();
    let value = parse_expression(pairs.next().unwrap(), bindings.clone())?;
    let body = match pairs.next() {
        Some(pair) => Some(Box::new(parse_expression(pair, bindings.clone())?)),
        None => None,
    };

    bindings.borrow_mut().push(name);

    Ok(Node {
        kind: NodeKind::Binding {
            recursive,
            name,
            value: Box::new(value),
            body,
        },
        span,
    })
}

fn parse_function_expression<'a>(
    function: Pair<'a, Rule>,
    bindings: Bindings<'a>,
) -> Result<Node<'a>> {
    debug_assert_eq!(function.as_rule(), Rule::function_expression);
    let span = function.as_span();
    let mut inner = function.into_inner();

    let parameters = parse_parameter_list(inner.next().unwrap());

    let body = parse_expression(inner.next().unwrap(), bindings)?;

    Ok(Node {
        kind: NodeKind::Function {
            parameters,
            body: Box::new(body),
        },
        span,
    })
}

fn parse_parameter_list(parameter_list: Pair<Rule>) -> Vec<&str> {
    debug_assert_eq!(parameter_list.as_rule(), Rule::parameter_list);
    parameter_list
        .into_inner()
        .map(|pair| pair.as_str())
        .collect()
}

fn parse_prefix<'a>(op: Pair<'a, Rule>, rhs: Result<Node<'a>>) -> Result<Node<'a>> {
    let rhs = rhs?;
    let span = op.as_span().start_pos().span(&rhs.span.end_pos());
    let kind = match op.as_rule() {
        Rule::unary_minus => NodeKind::UnaryOp {
            op: UnaryOp::Negate,
            expr: Box::new(rhs),
        },
        Rule::not => NodeKind::UnaryOp {
            op: UnaryOp::Not,
            expr: Box::new(rhs),
        },
        _ => unreachable!(),
    };
    Ok(Node { kind, span })
}

fn parse_postfix<'a>(
    lhs: Result<Node<'a>>,
    op: Pair<'a, Rule>,
    bindings: Bindings<'a>,
) -> Result<Node<'a>> {
    debug_assert_eq!(op.as_rule(), Rule::function_application);
    let lhs = lhs?;
    let span = lhs.span.start_pos().span(&op.as_span().end_pos());

    let arguments = parse_argument_list(op.into_inner().next().unwrap(), bindings)?;

    Ok(Node {
        kind: NodeKind::FunctionApplication {
            function: Box::new(lhs),
            arguments,
        },
        span,
    })
}

fn parse_argument_list<'a>(
    argument_list: Pair<'a, Rule>,
    bindings: Bindings<'a>,
) -> Result<Vec<Node<'a>>> {
    debug_assert_eq!(argument_list.as_rule(), Rule::argument_list);
    argument_list
        .into_inner()
        .map(|arg| parse_expression(arg, bindings.clone()))
        .collect()
}

fn parse_literal(pair: Pair<'_, Rule>) -> Result<Node<'_>> {
    debug_assert_eq!(pair.as_rule(), Rule::literal);
    let mut inner = pair.into_inner();
    let first = inner.next().unwrap();
    match first.as_rule() {
        Rule::integer => parse_integer(first),
        Rule::boolean => parse_boolean(first),
        _ => unreachable!(),
    }
}

fn parse_integer(integer: Pair<'_, Rule>) -> Result<Node<'_>> {
    debug_assert_eq!(integer.as_rule(), Rule::integer);
    let s = integer.as_str();
    let i: Result<i64, _> = s.chars().filter(|c| c != &'_').collect::<String>().parse();
    // All expressions composed of only ascii digits and underscores are valid integers (except for overflow)
    // TODO: Handle overflows gracefully
    Ok(Node {
        kind: NodeKind::Integer(i.unwrap()),
        span: integer.as_span(),
    })
}

fn parse_boolean(boolean: Pair<'_, Rule>) -> Result<Node<'_>> {
    debug_assert_eq!(boolean.as_rule(), Rule::boolean);
    let s = boolean.as_str();
    let b = match s {
        "true" => true,
        "false" => false,
        _ => unreachable!(),
    };
    Ok(Node {
        kind: NodeKind::Boolean(b),
        span: boolean.as_span(),
    })
}

fn parse_identifier<'a>(identifier: Pair<'a, Rule>, bindings: Bindings<'a>) -> Result<Node<'a>> {
    debug_assert_eq!(identifier.as_rule(), Rule::identifier);
    let s = identifier.as_str();
    let Some(pos) = bindings.borrow().iter().rposition(|&x| x == s) else {
        bail!("Unknown identifier: {}", s);
    };

    Ok(Node {
        kind: NodeKind::Identifier(s, pos),
        span: identifier.as_span(),
    })
}

fn parse_infix<'a>(
    lhs: Result<Node<'a>>,
    infix_op: Pair<Rule>,
    rhs: Result<Node<'a>>,
) -> Result<Node<'a>> {
    let lhs = lhs?;
    let rhs = rhs?;
    let span = lhs.span.start_pos().span(&rhs.span.end_pos());

    let op = match infix_op.as_rule() {
        Rule::plus => BinaryOp::Plus,
        Rule::minus => BinaryOp::Minus,
        Rule::times => BinaryOp::Times,
        Rule::divide => BinaryOp::Divide,
        Rule::modulo => BinaryOp::Modulo,
        Rule::power => BinaryOp::Power,
        Rule::equal => BinaryOp::Equal,
        Rule::not_equal => BinaryOp::NotEqual,
        Rule::less_than => BinaryOp::LessThan,
        Rule::less_than_or_equal => BinaryOp::LessThanOrEqual,
        Rule::greater_than => BinaryOp::GreaterThan,
        Rule::greater_than_or_equal => BinaryOp::GreaterThanOrEqual,
        Rule::and => BinaryOp::And,
        Rule::or => BinaryOp::Or,
        _ => unreachable!(),
    };

    Ok(Node {
        kind: NodeKind::BinaryOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
        span,
    })
}
