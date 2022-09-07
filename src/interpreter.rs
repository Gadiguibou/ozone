use crate::parser::Rule;
use lazy_static::lazy_static;
use pest::iterators::Pair;
use pest::prec_climber::{Assoc, Operator, PrecClimber};

/// Evaluates a [Rule::arithmetic_expression] while respecting operator precedence and associativity.
pub fn eval(ast: Pair<Rule>) -> i64 {
    if !matches!(ast.as_rule(), Rule::arithmetic_expression) {
        panic!("Did not pass in an arithmetic expression to eval")
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
    fn primary(pair: Pair<Rule>) -> i64 {
        // In the case where there is a negation, a negation_expression contains another negation_expression.
        // Otherwise, it only contains an integer literal.

        // Recursively evaluate the negation expression to find its final negation status.
        let mut inner = unsafe { pair.into_inner().next().unwrap_unchecked() };
        let mut negative = false;
        loop {
            match inner.as_rule() {
                Rule::integer => {
                    let integer = parse_integer(inner.as_str());
                    break if negative { -integer } else { integer };
                }
                Rule::negation_expression => {
                    inner = unsafe { inner.into_inner().next().unwrap_unchecked() };
                    negative = !negative;
                }
                _ => unreachable!(),
            }
        }
    }

    fn infix(lhs: i64, op: Pair<Rule>, rhs: i64) -> i64 {
        match op.as_rule() {
            Rule::plus => lhs + rhs,
            Rule::minus => lhs - rhs,
            Rule::times => lhs * rhs,
            Rule::divide => lhs / rhs,
            Rule::modulo => lhs % rhs,
            // TODO: Handle the case where an exponent is negative
            Rule::power => lhs.pow(rhs as u32),
            _ => unreachable!(),
        }
    }

    PREC_CLIMBER.climb(ast.into_inner(), primary, infix)
}

/// Parses a [Rule::integer].
///
/// Should only be called with strings composed of ASCII digits ('0'..'9') and underscores ('_').
fn parse_integer(s: &str) -> i64 {
    let result = s.chars().filter(|c| c != &'_').collect::<String>().parse();
    // All expressions composed of only ascii digits and underscores are valid integers (except for overflow)
    // TODO: Handle overflows gracefully
    result.unwrap()
}
