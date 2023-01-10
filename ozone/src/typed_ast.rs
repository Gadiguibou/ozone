use crate::ast::Node as UntypedNode;
use crate::ast::{self, IfClause, NodeKind};
use crate::ast::{BinaryOp, UnaryOp};
use crate::types::{Type, TypeEnvironment};
use anyhow::bail;
use zero_copy_stack::{ZeroCopyStack, ZeroCopyStackHandle};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedAst<'a> {
    pub root: Node<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Bindings<'a> {
    stack: ZeroCopyStack<Binding<'a>>,
}

type ScopedBindings<'parent, 'stack> = ZeroCopyStackHandle<'parent, Binding<'stack>>;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Binding<'a> {
    name: &'a str,
    ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node<'a> {
    pub kind: NodeKind<'a, Node<'a>>,
    pub span: pest::Span<'a>,
    pub ty: Type,
}

impl<'a> TypedAst<'a> {
    pub fn from(ast: ast::Ast<'a>) -> anyhow::Result<Self> {
        let mut bindings = Bindings::new();
        let mut type_env = TypeEnvironment::new();
        let root = Node::from(ast.root, &mut bindings.scoped(), &mut type_env)?;
        Ok(Self { root })
    }
}

impl<'a> Bindings<'a> {
    fn new() -> Self {
        Self {
            stack: ZeroCopyStack::new(),
        }
    }

    fn scoped(&mut self) -> ScopedBindings<'_, 'a> {
        self.stack.handle()
    }
}

impl<'a> Node<'a> {
    fn from(
        node: UntypedNode<'a>,
        bindings: &mut ScopedBindings<'_, 'a>,
        type_env: &mut TypeEnvironment,
    ) -> anyhow::Result<Self> {
        match node.kind {
            NodeKind::Conditional {
                if_clauses,
                else_clause,
            } => Self::from_conditional(bindings, node.span, if_clauses, else_clause, type_env),
            NodeKind::Binding {
                name,
                recursive,
                value,
                body,
            } => Self::from_binding(bindings, node.span, name, recursive, *value, body, type_env),
            NodeKind::BinaryOp { op, lhs, rhs } => {
                Self::from_binary_op(bindings, node.span, *lhs, op, *rhs, type_env)
            }
            NodeKind::UnaryOp { op, expr } => {
                Self::from_unary_op(bindings, node.span, op, *expr, type_env)
            }
            NodeKind::FunctionApplication {
                function,
                arguments,
            } => {
                Self::from_function_application(bindings, node.span, *function, arguments, type_env)
            }
            NodeKind::Function { parameters, body } => {
                Self::from_function(bindings, node.span, parameters, *body, type_env)
            }
            NodeKind::Identifier(name, pos) => {
                Self::from_identifier(bindings, node.span, name, pos, type_env)
            }
            NodeKind::Integer(value) => Ok(Self {
                kind: NodeKind::Integer(value),
                ty: Type::Integer,
                span: node.span,
            }),
            NodeKind::Boolean(value) => Ok(Self {
                kind: NodeKind::Boolean(value),
                ty: Type::Boolean,
                span: node.span,
            }),
        }
    }

    fn from_conditional(
        bindings: &mut ScopedBindings<'_, 'a>,
        span: pest::Span<'a>,
        if_clauses: Vec<IfClause<UntypedNode<'a>>>,
        else_clause: Option<Box<UntypedNode<'a>>>,
        type_env: &mut TypeEnvironment,
    ) -> anyhow::Result<Node<'a>> {
        let if_clauses = if_clauses
            .into_iter()
            .map(|clause| {
                let condition = Node::from(clause.condition, bindings, type_env)?;
                let body = Node::from(clause.body, bindings, type_env)?;
                Ok(IfClause { condition, body })
            })
            .collect::<anyhow::Result<Vec<_>>>()?;
        let else_clause = else_clause
            .map(|c| Node::from(*c, bindings, type_env))
            .transpose()?;

        for if_clause in if_clauses.iter() {
            if type_env
                .unify(&if_clause.condition.ty, &Type::Boolean)
                .is_err()
            {
                bail!(
                    "Condition must be a boolean. Found: {}",
                    if_clause.condition.ty
                )
            }
        }

        let mut ty = type_env.new_type_variable();

        for if_clause in if_clauses.iter() {
            match type_env.unify(&if_clause.body.ty, &ty) {
                Ok(clause_type) => ty = clause_type,
                Err(_) => bail!(
                    "Branches of conditional must have the same type. Found: {} and {}",
                    ty,
                    type_env.resolve(&if_clause.body.ty)
                ),
            }
        }

        if type_env
            .unify(
                else_clause.as_ref().map(|c| &c.ty).unwrap_or(&Type::Unit),
                &ty,
            )
            .is_err()
        {
            bail!(
                "Branches of conditional must have the same type. Found: {} and {}",
                ty,
                else_clause.map(|c| c.ty).unwrap_or(Type::Unit)
            )
        }

        Ok(Self {
            kind: NodeKind::Conditional {
                if_clauses,
                else_clause: else_clause.map(Box::new),
            },
            ty,
            span,
        })
    }

    fn from_binding(
        bindings: &'_ mut ScopedBindings<'_, 'a>,
        span: pest::Span<'a>,
        name: &'a str,
        recursive: bool,
        value: UntypedNode<'a>,
        body: Option<Box<UntypedNode<'a>>>,
        type_env: &mut TypeEnvironment,
    ) -> anyhow::Result<Node<'a>> {
        let value = if recursive {
            let mut bindings = bindings.handle();
            bindings.push(Binding {
                name,
                ty: type_env.new_type_variable(),
            });
            Box::new(Node::from(value, &mut bindings, type_env)?)
        } else {
            Box::new(Node::from(value, bindings, type_env)?)
        };

        let mut bindings = bindings.handle();

        bindings.push(Binding {
            name,
            ty: type_env.resolve(&value.ty),
        });

        let body = match body {
            Some(body) => Some(Box::new(Node::from(*body, &mut bindings, type_env)?)),
            None => None,
        };

        Ok(Self {
            ty: body
                .as_ref()
                .map(|body| &body.ty)
                .unwrap_or(&Type::Unit)
                .clone(),
            kind: NodeKind::Binding {
                name,
                recursive,
                value,
                body,
            },
            span,
        })
    }

    fn from_binary_op(
        bindings: &mut ScopedBindings<'_, 'a>,
        span: pest::Span<'a>,
        lhs: UntypedNode<'a>,
        op: BinaryOp,
        rhs: UntypedNode<'a>,
        type_env: &mut TypeEnvironment,
    ) -> anyhow::Result<Node<'a>> {
        use BinaryOp::*;
        match op {
            Plus | Minus | Times | Divide | Modulo | Power => {
                Self::from_arithmetic_op(bindings, span, lhs, op, rhs, type_env)
            }
            LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual => {
                Self::from_ordering_op(bindings, span, lhs, op, rhs, type_env)
            }
            Equal | NotEqual => Self::from_equality_op(bindings, span, lhs, op, rhs, type_env),
            And | Or => Self::from_logical_op(bindings, span, lhs, op, rhs, type_env),
        }
    }

    fn from_arithmetic_op(
        bindings: &mut ZeroCopyStackHandle<Binding<'a>>,
        span: pest::Span<'a>,
        lhs: UntypedNode<'a>,
        op: BinaryOp,
        rhs: UntypedNode<'a>,
        type_env: &mut TypeEnvironment,
    ) -> anyhow::Result<Node<'a>> {
        let lhs = Box::new(Node::from(lhs, bindings, type_env)?);
        let rhs = Box::new(Node::from(rhs, bindings, type_env)?);

        if type_env
            .unify(&lhs.ty, &Type::Integer)
            .and(type_env.unify(&rhs.ty, &Type::Integer))
            .is_err()
        {
            bail!(
                "Operands of arithmetic operations must be integers. Found: {} and {}",
                type_env.resolve(&lhs.ty),
                type_env.resolve(&rhs.ty)
            )
        }

        Ok(Self {
            ty: Type::Integer,
            kind: NodeKind::BinaryOp { op, lhs, rhs },
            span,
        })
    }

    fn from_ordering_op(
        bindings: &mut ZeroCopyStackHandle<Binding<'a>>,
        span: pest::Span<'a>,
        lhs: UntypedNode<'a>,
        op: BinaryOp,
        rhs: UntypedNode<'a>,
        type_env: &mut TypeEnvironment,
    ) -> anyhow::Result<Node<'a>> {
        let lhs = Box::new(Node::from(lhs, bindings, type_env)?);
        let rhs = Box::new(Node::from(rhs, bindings, type_env)?);

        if type_env
            .unify(&lhs.ty, &Type::Integer)
            .and(type_env.unify(&rhs.ty, &Type::Integer))
            .is_err()
        {
            bail!(
                "Operands of comparison operations must be integers. Found: {} and {}",
                type_env.resolve(&lhs.ty),
                type_env.resolve(&rhs.ty)
            )
        }

        Ok(Self {
            ty: Type::Boolean,
            kind: NodeKind::BinaryOp { op, lhs, rhs },
            span,
        })
    }

    fn from_equality_op(
        bindings: &mut ZeroCopyStackHandle<Binding<'a>>,
        span: pest::Span<'a>,
        lhs: UntypedNode<'a>,
        op: BinaryOp,
        rhs: UntypedNode<'a>,
        type_env: &mut TypeEnvironment,
    ) -> anyhow::Result<Node<'a>> {
        let lhs = Box::new(Node::from(lhs, bindings, type_env)?);
        let rhs = Box::new(Node::from(rhs, bindings, type_env)?);

        if type_env.unify(&lhs.ty, &rhs.ty).is_err() {
            bail!(
                "Operands of equality operations must be of the same type. Found: {} and {}",
                type_env.resolve(&lhs.ty),
                type_env.resolve(&rhs.ty)
            )
        }

        let ty = type_env.resolve(&lhs.ty); // == rhs.resolve()

        // TODO: Remove this check once equality is implemented on other types
        if type_env.unifiable(&ty, &Type::Integer).is_ok()
            && type_env.unifiable(&ty, &Type::Boolean).is_ok()
        {
            bail!(
                "Equality operations are only implemented for integers and booleans. Found: {}",
                ty
            )
        }

        Ok(Self {
            ty: Type::Boolean,
            kind: NodeKind::BinaryOp { op, lhs, rhs },
            span,
        })
    }

    fn from_logical_op(
        bindings: &mut ZeroCopyStackHandle<Binding<'a>>,
        span: pest::Span<'a>,
        lhs: UntypedNode<'a>,
        op: BinaryOp,
        rhs: UntypedNode<'a>,
        type_env: &mut TypeEnvironment,
    ) -> anyhow::Result<Node<'a>> {
        let lhs = Box::new(Node::from(lhs, bindings, type_env)?);
        let rhs = Box::new(Node::from(rhs, bindings, type_env)?);

        if type_env
            .unify(&lhs.ty, &Type::Boolean)
            .and(type_env.unify(&rhs.ty, &Type::Boolean))
            .is_err()
        {
            bail!(
                "Operands of logical operations must be booleans. Found: {} and {}",
                type_env.resolve(&lhs.ty),
                type_env.resolve(&rhs.ty)
            )
        }

        Ok(Self {
            ty: Type::Boolean,
            kind: NodeKind::BinaryOp { op, lhs, rhs },
            span,
        })
    }

    fn from_unary_op(
        bindings: &mut ZeroCopyStackHandle<Binding<'a>>,
        span: pest::Span<'a>,
        op: UnaryOp,
        expr: UntypedNode<'a>,
        type_env: &mut TypeEnvironment,
    ) -> anyhow::Result<Node<'a>> {
        let expr = Box::new(Node::from(expr, bindings, type_env)?);

        match (&op, &expr.ty) {
            (UnaryOp::Negate, ty) => {
                if type_env.unify(ty, &Type::Integer).is_err() {
                    bail!("Operand of unary minus must be an integer. Found: {}", ty)
                }
            }
            (UnaryOp::Not, ty) => {
                if type_env.unify(ty, &Type::Boolean).is_err() {
                    bail!("Operand of logical not must be a boolean. Found: {}", ty)
                }
            }
        };

        Ok(Self {
            ty: expr.ty.clone(),
            kind: NodeKind::UnaryOp { op, expr },
            span,
        })
    }

    fn from_function_application(
        bindings: &mut ZeroCopyStackHandle<Binding<'a>>,
        span: pest::Span<'a>,
        function: UntypedNode<'a>,
        arguments: Vec<UntypedNode<'a>>,
        type_env: &mut TypeEnvironment,
    ) -> anyhow::Result<Node<'a>> {
        let function = Box::new(Node::from(function, bindings, type_env)?);

        let arguments = arguments
            .into_iter()
            .map(|arg| Node::from(arg, bindings, type_env))
            .collect::<anyhow::Result<Vec<_>>>()?;

        let parameter_types = arguments.iter().map(|arg| arg.ty.clone()).collect();
        let return_type = Box::new(type_env.new_type_variable());

        let expected_type = Type::Function {
            parameter_types,
            return_type,
        };

        let actual_return_type = match type_env.unifiable(&function.ty, &expected_type) {
            Ok(t) => t,
            Err(_) => bail!(
                "Mismatched types for function application. Expected: {} but found: {}",
                expected_type,
                function.ty
            ),
        };

        Ok(Self {
            ty: actual_return_type,
            kind: NodeKind::FunctionApplication {
                function,
                arguments,
            },
            span,
        })
    }

    fn from_function(
        bindings: &mut ZeroCopyStackHandle<Binding<'a>>,
        span: pest::Span<'a>,
        parameters: Vec<&'a str>,
        body: UntypedNode<'a>,
        type_env: &mut TypeEnvironment,
    ) -> anyhow::Result<Node<'a>> {
        let mut bindings = bindings.handle();
        for parameter in parameters.iter() {
            bindings.push(Binding {
                name: parameter,
                ty: type_env.new_type_variable(),
            });
        }
        let body = Box::new(Node::from(body, &mut bindings, type_env)?);
        let parameter_types = parameters
            .iter()
            .map(|p| type_env.resolve(&bindings.find(|b| &b.name == p).unwrap().ty))
            .collect();
        let return_type = type_env.resolve(&body.ty);
        Ok(Self {
            ty: Type::Function {
                parameter_types,
                return_type: Box::new(return_type),
            },
            kind: NodeKind::Function { parameters, body },
            span,
        })
    }

    fn from_identifier(
        bindings: &mut ZeroCopyStackHandle<Binding>,
        span: pest::Span<'a>,
        name: &'a str,
        pos: usize,
        type_env: &mut TypeEnvironment,
    ) -> anyhow::Result<Node<'a>> {
        let binding = bindings.get(pos).unwrap();

        Ok(Self {
            ty: type_env.resolve(&binding.ty),
            kind: NodeKind::Identifier(name, pos),
            span,
        })
    }
}
