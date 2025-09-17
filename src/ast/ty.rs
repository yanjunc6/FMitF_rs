//! ast/ty.rs
//!
//! Type-related AST nodes and resolved type representations.

use super::common::Identifier;
use super::item::{GenericParamId, TableId, TypeDeclId};
use crate::util::Span;
use id_arena::Id;
use std::collections::HashMap;

// ============================================================================
// --- Syntactic Types (from source)
// ============================================================================

pub type AstTypeId = Id<AstType>;

/// Syntactic representation of types from source code. Renamed to AstType to avoid confusion.
#[derive(Debug, Clone)]
pub enum AstType {
    /// Simple type name: `int`, `MyTable`, `T`
    Named {
        name: Identifier,
        resolved_type: Option<TypeDeclId>, // Filled by name resolver
    },

    /// Generic instantiation: `List<int>`, `Map<K, V>`
    Generic {
        base: Identifier,
        args: Vec<AstTypeId>,
        resolved_base_type: Option<TypeDeclId>, // Filled by name resolver
        span: Option<Span>,
    },

    /// Function type: `(int, bool) -> string`
    Function {
        params: Vec<AstTypeId>,
        return_type: AstTypeId,
        span: Option<Span>,
    },
}

// ============================================================================
// --- Resolved Types (after type checking)
// ============================================================================

/// A unique identifier for type variables during type inference
pub type TypeVarId = u32;

/// Resolved type information after type checking and inference
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedType {
    /// Primitive types with potential type arguments (e.g., List<int>, Row<User>)
    Primitive {
        type_id: TypeDeclId,
        type_args: Vec<ResolvedType>,
        /// Generic type variables that are bound within this type scope
        /// For List<T>, this contains the type variable for T
        bound_vars: Vec<TypeVarId>,
    },
    /// Table reference types (e.g., the table Activity has type Table<Activity>)
    Table {
        table_id: TableId,
    },
    /// Type variables during inference - can be bounded or unbounded
    TypeVariable {
        var_id: TypeVarId,
        name: String,
        /// If Some, this type variable is bound to a specific generic parameter
        /// This ensures all instances of T in get<T> refer to the same type
        bound_to: Option<GenericParamId>,
    },
    /// Function types with potential generic type variables
    Function {
        param_types: Vec<ResolvedType>,
        return_type: Box<ResolvedType>,
        /// Generic type variables that are bound within this function scope
        bound_vars: Vec<TypeVarId>,
    },
    Void,
    Unknown,
    /// A type that has not yet been resolved.
    Unresolved(AstTypeId),
}

impl ResolvedType {
    /// Check if this type contains any unbound type variables
    pub fn has_unbound_vars(&self) -> bool {
        match self {
            ResolvedType::TypeVariable { bound_to, .. } => bound_to.is_none(),
            ResolvedType::Primitive { type_args, .. } => {
                type_args.iter().any(|t| t.has_unbound_vars())
            }
            ResolvedType::Function {
                param_types,
                return_type,
                ..
            } => param_types.iter().any(|t| t.has_unbound_vars()) || return_type.has_unbound_vars(),
            _ => false, // Table, Void, Unknown, Unresolved don't contain unbound variables
        }
    }

    /// Get all type variables (bound and unbound) contained in this type
    pub fn get_type_vars(&self) -> Vec<TypeVarId> {
        let mut vars = Vec::new();
        self.collect_type_vars(&mut vars);
        vars
    }

    fn collect_type_vars(&self, vars: &mut Vec<TypeVarId>) {
        match self {
            ResolvedType::TypeVariable { var_id, .. } => {
                if !vars.contains(var_id) {
                    vars.push(*var_id);
                }
            }
            ResolvedType::Primitive { type_args, .. } => {
                for arg in type_args {
                    arg.collect_type_vars(vars);
                }
            }
            ResolvedType::Function {
                param_types,
                return_type,
                ..
            } => {
                for param in param_types {
                    param.collect_type_vars(vars);
                }
                return_type.collect_type_vars(vars);
            }
            _ => {}
        }
    }

    /// Check if this type can be unified with another type
    pub fn can_unify_with(&self, other: &ResolvedType) -> bool {
        match (self, other) {
            // Type variables can unify with anything
            (ResolvedType::TypeVariable { .. }, _) | (_, ResolvedType::TypeVariable { .. }) => true,

            // Same primitives can unify if their type args can unify
            (
                ResolvedType::Primitive {
                    type_id: id1,
                    type_args: args1,
                    ..
                },
                ResolvedType::Primitive {
                    type_id: id2,
                    type_args: args2,
                    ..
                },
            ) => {
                id1 == id2
                    && args1.len() == args2.len()
                    && args1
                        .iter()
                        .zip(args2)
                        .all(|(a1, a2)| a1.can_unify_with(a2))
            }

            // Tables can only unify with same table
            (ResolvedType::Table { table_id: id1 }, ResolvedType::Table { table_id: id2 }) => {
                id1 == id2
            }

            // Functions can unify if params and return types can unify
            (
                ResolvedType::Function {
                    param_types: p1,
                    return_type: r1,
                    ..
                },
                ResolvedType::Function {
                    param_types: p2,
                    return_type: r2,
                    ..
                },
            ) => {
                p1.len() == p2.len()
                    && p1.iter().zip(p2).all(|(a1, a2)| a1.can_unify_with(a2))
                    && r1.can_unify_with(r2)
            }

            // Unknown can unify with anything
            (ResolvedType::Unknown, _) | (_, ResolvedType::Unknown) => true,

            _ => false,
        }
    }
}

/// Type substitution map for generic instantiation
pub type TypeSubstitution = std::collections::HashMap<TypeVarId, ResolvedType>;

// ============================================================================
// --- Type Resolution Helpers
// ============================================================================

#[derive(Debug, Clone, Default)]
pub struct Substitution {
    substitutions: HashMap<TypeVarId, ResolvedType>,
}

impl Substitution {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn extend(&mut self, var: TypeVarId, ty: ResolvedType) {
        self.substitutions.insert(var, ty);
    }

    pub fn apply(&self, ty: &ResolvedType) -> ResolvedType {
        match ty {
            ResolvedType::TypeVariable { var_id, .. } => {
                if let Some(t) = self.substitutions.get(var_id) {
                    self.apply(t)
                } else {
                    ty.clone()
                }
            }
            ResolvedType::Primitive {
                type_id,
                type_args,
                bound_vars,
            } => ResolvedType::Primitive {
                type_id: *type_id,
                type_args: type_args.iter().map(|t| self.apply(t)).collect(),
                bound_vars: bound_vars.clone(),
            },
            ResolvedType::Function {
                param_types,
                return_type,
                bound_vars,
            } => ResolvedType::Function {
                param_types: param_types.iter().map(|t| self.apply(t)).collect(),
                return_type: Box::new(self.apply(return_type)),
                bound_vars: bound_vars.clone(),
            },
            _ => ty.clone(),
        }
    }
}
