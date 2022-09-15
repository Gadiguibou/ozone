use std::fmt::Display;

use crate::ast;
use crate::ast::Node as UntypedNode;
use crate::ast::{BinaryOp, UnaryOp};
use anyhow::{anyhow, bail};
use zero_copy_stack::{ZeroCopyStack, ZeroCopyStackHandle};

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct TypedAst {
    pub root: Node,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum Type {
    Integer,
    Boolean,
    Function {
        parameter_types: Vec<Type>,
        return_type: Box<Type>,
    },
    // Type variables carry the index of their content in the `TYPE_VARIABLES` `Vec`.
    TypeVariable(usize),
}

// TODO: Stop doing this
static mut TYPE_VARIABLES: Vec<Option<Type>> = Vec::new();

impl Type {
    pub fn resolve(&self) -> Type {
        if let Type::TypeVariable(id) = self {
            let mut i = *id;
            let mut origin = unsafe { &TYPE_VARIABLES[i] };
            while let Some(Type::TypeVariable(j)) = origin {
                i = *j;
                origin = unsafe { &TYPE_VARIABLES[i] };
            }

            let ty = match origin {
                Some(t) => t.clone(),
                None => Type::TypeVariable(i),
            };
            // Compress
            unsafe {
                // If i == id then we already are at the origin and we should not add a pointer to ourselves
                // If we did we would end up in an infinite loop when resolving next time.
                if &i != id {
                    TYPE_VARIABLES[*id] = Some(ty.clone());
                }
            }
            ty
        } else {
            self.clone()
        }
    }

    fn constrain(&self, using: &Type) -> anyhow::Result<()> {
        let current = self.resolve();
        let using = using.resolve();

        match (&current, &using) {
            (Type::TypeVariable(i), Type::TypeVariable(j)) => match i.cmp(j) {
                std::cmp::Ordering::Less => unsafe {
                    TYPE_VARIABLES[*j] = Some(Type::TypeVariable(*i));
                },
                std::cmp::Ordering::Greater => unsafe {
                    TYPE_VARIABLES[*i] = Some(Type::TypeVariable(*j));
                },
                std::cmp::Ordering::Equal => {}
            },
            (Type::TypeVariable(i), _) => unsafe {
                TYPE_VARIABLES[*i] = Some(using);
            },
            (_, Type::TypeVariable(j)) => unsafe {
                TYPE_VARIABLES[*j] = Some(current);
            },
            (
                Type::Function {
                    parameter_types: pt1,
                    return_type: rt1,
                },
                Type::Function {
                    parameter_types: pt2,
                    return_type: rt2,
                },
            ) => {
                if pt1.len() != pt2.len() {
                    bail!("Cannot constrain {current} with {using}")
                }

                for (t1, t2) in pt1.iter().zip(pt2.iter()) {
                    t1.constrain(t2)?
                }

                rt1.constrain(rt2)?
            }
            (ty1, ty2) if ty1 == ty2 => {}
            (ty1, ty2) => bail!("Cannot constrain {ty1} with {ty2}"),
        }
        Ok(())
    }

    fn new_type_variable() -> Type {
        let id = unsafe { TYPE_VARIABLES.len() };
        unsafe {
            TYPE_VARIABLES.push(None);
        }
        Type::TypeVariable(id)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.resolve() {
            Type::Integer => write!(f, "Integer"),
            Type::Boolean => write!(f, "Boolean"),
            Type::Function {
                parameter_types: parameters,
                return_type,
            } => {
                write!(
                    f,
                    "({}) -> ({})",
                    parameters
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                    return_type
                )
            }
            Type::TypeVariable(i) => {
                // Unconstrained type variable: print 'a -> 'b -> ... -> 'aa -> 'ab -> ... based on index
                let mut s = String::new();
                let mut n = i;
                loop {
                    s.push((b'a' + (n % 26) as u8) as char);
                    n /= 26;
                    if n == 0 {
                        break;
                    }
                }
                s.push('\'');
                let s = s.chars().rev().collect::<String>();
                write!(f, "{}", s)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
struct Bindings {
    stack: ZeroCopyStack<Binding>,
}

type ScopedBindings<'input> = ZeroCopyStackHandle<'input, Binding>;

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
struct Binding {
    name: String,
    ty: Type,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct Node {
    pub kind: NodeKind,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum NodeKind {
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

impl TypedAst {
    pub fn from(ast: ast::Ast) -> anyhow::Result<Self> {
        let mut bindings = Bindings::new();
        let root = Node::from(ast.root, &mut bindings.scoped())?;
        Ok(Self { root })
    }
}

impl Bindings {
    fn new() -> Self {
        Self {
            stack: ZeroCopyStack::new(),
        }
    }

    fn scoped(&mut self) -> ScopedBindings {
        self.stack.handle()
    }
}

impl Node {
    fn from(ast: UntypedNode, bindings: &mut ScopedBindings) -> anyhow::Result<Self> {
        match ast {
            UntypedNode::Conditional {
                condition,
                then_expr,
                else_expr,
            } => Self::from_conditional(bindings, *condition, *then_expr, *else_expr),
            UntypedNode::Binding {
                name,
                recursive,
                value,
                body,
            } => Self::from_binding(bindings, name, recursive, *value, *body),
            UntypedNode::BinaryOp { op, lhs, rhs } => Self::from_binary_op(bindings, *lhs, op, *rhs),
            UntypedNode::UnaryOp { op, expr } => Self::from_unary_op(bindings, op, *expr),
            UntypedNode::FunctionApplication {
                function,
                arguments,
            } => Self::from_function_application(bindings, *function, arguments),
            UntypedNode::Function { parameters, body } => {
                Self::from_function(bindings, parameters, *body)
            }
            UntypedNode::Identifier(name) => Self::from_identifier(bindings, name),
            UntypedNode::Integer(value) => Ok(Self {
                kind: NodeKind::Integer(value),
                ty: Type::Integer,
            }),
            UntypedNode::Boolean(value) => Ok(Self {
                kind: NodeKind::Boolean(value),
                ty: Type::Boolean,
            }),
        }
    }

    fn from_conditional(
        bindings: &mut ScopedBindings,
        condition: UntypedNode,
        then_expr: UntypedNode,
        else_expr: UntypedNode,
    ) -> anyhow::Result<Node> {
        let condition = Box::new(Node::from(condition, bindings)?);

        if condition.ty.constrain(&Type::Boolean).is_err() {
            bail!("Condition must be a boolean. Found: {}", condition.ty)
        }

        let then_expr = Box::new(Node::from(then_expr, bindings)?);
        let else_expr = Box::new(Node::from(else_expr, bindings)?);

        let ty = match then_expr.ty.constrain(&else_expr.ty) {
            Ok(()) => then_expr.ty.resolve(),
            Err(_) => bail!(
                "Branches of conditional must have the same type. Found: {} and {}",
                then_expr.ty.resolve(),
                else_expr.ty.resolve()
            ),
        };

        Ok(Self {
            kind: NodeKind::Conditional {
                condition,
                then_expr,
                else_expr,
            },
            ty,
        })
    }

    fn from_binding(
        bindings: &mut ScopedBindings,
        name: String,
        recursive: bool,
        value: UntypedNode,
        body: UntypedNode,
    ) -> anyhow::Result<Node> {
        let value = if recursive {
            let mut bindings = bindings.handle();
            bindings.push(Binding {
                name: name.clone(),
                ty: Type::new_type_variable(),
            });
            Box::new(Node::from(value, &mut bindings)?)
        } else {
            Box::new(Node::from(value, bindings)?)
        };
        let mut bindings = bindings.handle();
        bindings.push(Binding {
            name: name.clone(),
            ty: value.ty.resolve(),
        });
        let body = Box::new(Node::from(body, &mut bindings)?);
        Ok(Self {
            ty: body.ty.clone(),
            kind: NodeKind::Binding {
                name,
                recursive,
                value,
                body,
            },
        })
    }

    fn from_binary_op(
        bindings: &mut ScopedBindings,
        lhs: UntypedNode,
        op: BinaryOp,
        rhs: UntypedNode,
    ) -> anyhow::Result<Node> {
        use BinaryOp::*;
        match op {
            Plus | Minus | Times | Divide | Modulo | Power => {
                Self::from_arithmetic_op(bindings, lhs, op, rhs)
            }
            LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual => {
                Self::from_ordering_op(bindings, lhs, op, rhs)
            }
            Equal | NotEqual => Self::from_equality_op(bindings, lhs, op, rhs),
            And | Or => Self::from_logical_op(bindings, lhs, op, rhs),
        }
    }

    fn from_arithmetic_op(
        bindings: &mut ZeroCopyStackHandle<Binding>,
        lhs: UntypedNode,
        op: BinaryOp,
        rhs: UntypedNode,
    ) -> anyhow::Result<Node> {
        let lhs = Box::new(Node::from(lhs, bindings)?);
        let rhs = Box::new(Node::from(rhs, bindings)?);

        if lhs
            .ty
            .constrain(&Type::Integer)
            .and(rhs.ty.constrain(&Type::Integer))
            .is_err()
        {
            bail!(
                "Operands of arithmetic operations must be integers. Found: {} and {}",
                lhs.ty.resolve(),
                rhs.ty.resolve()
            )
        }

        Ok(Self {
            ty: Type::Integer,
            kind: NodeKind::BinaryOp { op, lhs, rhs },
        })
    }

    fn from_ordering_op(
        bindings: &mut ZeroCopyStackHandle<Binding>,
        lhs: UntypedNode,
        op: BinaryOp,
        rhs: UntypedNode,
    ) -> anyhow::Result<Node> {
        let lhs = Box::new(Node::from(lhs, bindings)?);
        let rhs = Box::new(Node::from(rhs, bindings)?);

        if lhs
            .ty
            .constrain(&Type::Integer)
            .and(rhs.ty.constrain(&Type::Integer))
            .is_err()
        {
            bail!(
                "Operands of comparison operations must be integers. Found: {} and {}",
                lhs.ty.resolve(),
                rhs.ty.resolve()
            )
        }

        Ok(Self {
            ty: Type::Boolean,
            kind: NodeKind::BinaryOp { op, lhs, rhs },
        })
    }

    fn from_equality_op(
        bindings: &mut ZeroCopyStackHandle<Binding>,
        lhs: UntypedNode,
        op: BinaryOp,
        rhs: UntypedNode,
    ) -> anyhow::Result<Node> {
        let lhs = Box::new(Node::from(lhs, bindings)?);
        let rhs = Box::new(Node::from(rhs, bindings)?);

        if lhs.ty.constrain(&rhs.ty).is_err() {
            bail!(
                "Operands of equality operations must be of the same type. Found: {} and {}",
                lhs.ty.resolve(),
                rhs.ty.resolve()
            )
        }

        let ty = lhs.ty.resolve(); // == rhs.resolve()

        // TODO: Remove this check once equality is implemented on other types
        if ty != Type::Integer && ty != Type::Boolean {
            bail!(
                "Equality operations are only implemented for integers and booleans. Found: {}",
                ty
            )
        }

        Ok(Self {
            ty: Type::Boolean,
            kind: NodeKind::BinaryOp { op, lhs, rhs },
        })
    }

    fn from_logical_op(
        bindings: &mut ZeroCopyStackHandle<Binding>,
        lhs: UntypedNode,
        op: BinaryOp,
        rhs: UntypedNode,
    ) -> anyhow::Result<Node> {
        let lhs = Box::new(Node::from(lhs, bindings)?);
        let rhs = Box::new(Node::from(rhs, bindings)?);

        if lhs
            .ty
            .constrain(&Type::Boolean)
            .and(rhs.ty.constrain(&Type::Boolean))
            .is_err()
        {
            bail!(
                "Operands of logical operations must be booleans. Found: {} and {}",
                lhs.ty.resolve(),
                rhs.ty.resolve()
            )
        }

        Ok(Self {
            ty: Type::Boolean,
            kind: NodeKind::BinaryOp { op, lhs, rhs },
        })
    }

    fn from_unary_op(
        bindings: &mut ZeroCopyStackHandle<Binding>,
        op: UnaryOp,
        expr: UntypedNode,
    ) -> anyhow::Result<Node> {
        let expr = Box::new(Node::from(expr, bindings)?);

        match (&op, &expr.ty) {
            (UnaryOp::Negate, ty) => {
                if ty.constrain(&Type::Integer).is_err() {
                    bail!("Operand of unary minus must be an integer. Found: {}", ty)
                }
            }
            (UnaryOp::Not, ty) => {
                if ty.constrain(&Type::Boolean).is_err() {
                    bail!("Operand of logical not must be a boolean. Found: {}", ty)
                }
            }
        };

        Ok(Self {
            ty: expr.ty.clone(),
            kind: NodeKind::UnaryOp { op, expr },
        })
    }

    fn from_function_application(
        bindings: &mut ZeroCopyStackHandle<Binding>,
        function: UntypedNode,
        arguments: Vec<UntypedNode>,
    ) -> anyhow::Result<Node> {
        let function = Box::new(Node::from(function, bindings)?);

        let arguments = arguments
            .into_iter()
            .map(|arg| Node::from(arg, bindings))
            .collect::<anyhow::Result<Vec<_>>>()?;

        let parameter_types = arguments.iter().map(|arg| arg.ty.clone()).collect();
        let return_type = Box::new(Type::new_type_variable());

        let expected_type = Type::Function {
            parameter_types,
            return_type: return_type.clone(),
        };

        match function.ty.constrain(&expected_type) {
            Ok(_) => {}
            Err(_) => bail!(
                "Mismatched types for function application. Expected: {} but found: {}",
                expected_type,
                function.ty
            ),
        }

        Ok(Self {
            ty: return_type.resolve(),
            kind: NodeKind::FunctionApplication {
                function,
                arguments,
            },
        })
    }

    fn from_function(
        bindings: &mut ZeroCopyStackHandle<Binding>,
        parameters: Vec<String>,
        body: UntypedNode,
    ) -> anyhow::Result<Node> {
        let mut bindings = bindings.handle();
        for parameter in &parameters {
            bindings.push(Binding {
                name: parameter.clone(),
                ty: Type::new_type_variable(),
            });
        }
        let body = Box::new(Node::from(body, &mut bindings)?);
        let parameter_types = parameters
            .iter()
            .map(|p| bindings.find(|b| &b.name == p).unwrap().ty.resolve())
            .collect();
        let return_type = body.ty.resolve();
        Ok(Self {
            ty: Type::Function {
                parameter_types,
                return_type: Box::new(return_type),
            },
            kind: NodeKind::Function { parameters, body },
        })
    }

    fn from_identifier(
        bindings: &mut ZeroCopyStackHandle<Binding>,
        name: String,
    ) -> anyhow::Result<Node> {
        let binding = bindings
            .find(|binding| binding.name == name)
            .ok_or_else(|| anyhow!("Unknown identifier: {}", name))?;

        Ok(Self {
            ty: binding.ty.resolve(),
            kind: NodeKind::Identifier(name),
        })
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use NodeKind::*;
        match &self.kind {
            Conditional {
                condition,
                then_expr,
                else_expr,
            } => {
                write!(f, "if {} then {} else {}", condition, then_expr, else_expr)
            }
            Binding {
                name,
                recursive,
                value,
                body,
            } => write!(
                f,
                "let {}{}: {} = {} in {}",
                if *recursive { "rec " } else { "" },
                name,
                value.ty,
                value,
                body
            ),
            BinaryOp { op, lhs, rhs } => write!(f, "{} {} {}", lhs, op, rhs),
            UnaryOp { op, expr } => write!(f, "{}{}", op, expr),
            FunctionApplication {
                function,
                arguments,
            } => {
                write!(
                    f,
                    "{}({})",
                    function,
                    arguments
                        .iter()
                        .map(|a| { format!("{}", a) })
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Function { parameters, body } => {
                write!(
                    f,
                    "({}) => {}",
                    parameters
                        .iter()
                        .map(|p| { p.to_string() })
                        .collect::<Vec<String>>()
                        .join(", "),
                    body,
                )
            }
            Identifier(name) => write!(f, "{}", name),
            Integer(value) => write!(f, "{}", value),
            Boolean(value) => write!(f, "{}", value),
        }
    }
}
