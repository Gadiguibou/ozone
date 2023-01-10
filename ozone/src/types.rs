use std::fmt::Display;

use anyhow::bail;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unit,
    Integer,
    Boolean,
    Function {
        parameter_types: Vec<Type>,
        return_type: Box<Type>,
    },
    // Type variables carry the index of their content in the `TYPE_VARIABLES` `Vec`.
    TypeVariable {
        id: usize,
    },
}

impl Display for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Type::Unit => write!(f, "()"),
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
            Type::TypeVariable { id, .. } => fmt_type_variable(*id, f),
        }
    }
}

fn fmt_type_variable(i: usize, f: &mut std::fmt::Formatter) -> std::fmt::Result {
	// Unconstrained type variable: print 'a -> 'b -> ... -> 'aa -> 'ab -> ... based on index
    let mut s = String::new();
    let mut n = i;
    loop {
		s.push((b'a' + (n % 26) as u8) as char);
        n /= 26;
        if n == 0 {
			break;
        }
        n -= 1;
    }
    s.push('\'');
    let s = s.chars().rev().collect::<String>();
    write!(f, "{}", s)
}


// TODO: Add type bindings here
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeEnvironment {
	pub type_variables: Vec<Option<Type>>,
}

impl TypeEnvironment {
	pub fn new() -> Self {
		Self {
			type_variables: Vec::new(),
		}
	}
}

impl Default for TypeEnvironment {
	fn default() -> Self {
		Self::new()
	}
}

// TODO: Solve the issue for: `let f = (x) -> x in let test = f(1) in f(true)`
impl TypeEnvironment {
	pub fn resolve(&mut self, ty: &Type) -> Type {
		if let Type::TypeVariable { id: original_id } = ty {
			let mut id = *original_id;
			let mut next_type = &self.type_variables[id];
			while let Some(Type::TypeVariable { id: new_id }) = next_type {
				id = *new_id;
				next_type = &self.type_variables[id];
			}

			let ty = match next_type {
				Some(t) => t.clone(),
				None => Type::TypeVariable { id },
			};

			// Compress
			// If i == id then we already are at the origin and we should not add a pointer to ourselves
			// If we did we would end up in an infinite loop when resolving next time.
			if &id != original_id {
				self.type_variables[id] = Some(ty.clone());
			}
			ty
		} else {
			ty.clone()
		}
	}

	pub fn unify(&mut self, first: &Type, second: &Type) -> anyhow::Result<Type> {
		let first = self.resolve(first);
		let second = self.resolve(second);

		match (&first, &second) {
			(Type::TypeVariable { id: i }, Type::TypeVariable { id: j }) => match i.cmp(j) {
				std::cmp::Ordering::Less => {
					self.type_variables[*j] = Some(Type::TypeVariable { id: *i });
					Ok(first)
				}
				std::cmp::Ordering::Greater => {
					self.type_variables[*i] = Some(Type::TypeVariable { id: *j });
					Ok(second)
				}
				std::cmp::Ordering::Equal => Ok(first),
			},
			(Type::TypeVariable { id }, _) => {
				self.type_variables[*id] = Some(second.clone());
				Ok(second)
			}
			(_, Type::TypeVariable { id }) => {
				self.type_variables[*id] = Some(first.clone());
				Ok(first)
			}
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
					bail!("Cannot unify {first} with {second}")
				}

				for (t1, t2) in pt1.iter().zip(pt2.iter()) {
					self.unify(t1, t2)?;
				}

				Ok(self.unify(rt1, rt2)?)
			}
			(ty1, ty2) if ty1 == ty2 => Ok(ty1.clone()),
			(ty1, ty2) => bail!("Cannot unify {ty1} with {ty2}"),
		}
	}

	pub fn unifiable(&mut self, first: &Type, second: &Type) -> anyhow::Result<Type> {
		let first = self.resolve(first);
		let second = self.resolve(second);

		match (&first, &second) {
			(Type::TypeVariable { .. }, _) => Ok(second),
			(_, Type::TypeVariable { .. }) => Ok(first),
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
					bail!("Cannot unify {first} with {second}")
				}

				let mut new_type_env = self.clone();

				for (t1, t2) in pt1.iter().zip(pt2.iter()) {
					new_type_env.unify(t1, t2)?;
				}

				new_type_env.unify(rt1, rt2)
			}
			(ty1, ty2) if ty1 == ty2 => Ok(first),
			(ty1, ty2) => bail!("Cannot unify {ty1} with {ty2}"),
		}
	}

	// TODO: Use a better data structure
	pub fn new_type_variable(&mut self) -> Type {
		let id = self.type_variables.len();
		self.type_variables.push(None);
		Type::TypeVariable { id }
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_fmt_type_variable() {
		let type_env = TypeEnvironment::new();
		let type_env = Box::leak(Box::new(type_env));
		let a = type_env.new_type_variable();
		assert_eq!(a.to_string(), "'a");

		let aa = loop {
			if let Type::TypeVariable { id: 26, .. } = type_env.new_type_variable() {
				break Type::TypeVariable { id: 26 };
			}
		};
		assert_eq!(aa.to_string(), "'aa");

		let ab = type_env.new_type_variable();
		assert_eq!(ab.to_string(), "'ab");

		let ba = loop {
			if let Type::TypeVariable { id: 52, .. } = type_env.new_type_variable() {
				break Type::TypeVariable { id: 52 };
			}
		};
		assert_eq!(ba.to_string(), "'ba");
	}
}
