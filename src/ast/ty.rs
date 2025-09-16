//! ast/ty.rs
//!
//! Type-related AST nodes and resolved type representations.

use super::common::Identifier;
use super::item::{TableId, TypeDeclId};
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
    Primitive {
        type_id: TypeDeclId,
        type_args: Vec<ResolvedType>,
    },
    Table {
        table_id: TableId,
    },
    TypeVariable {
        var_id: TypeVarId,
        name: String,
    },
    Function {
        param_types: Vec<ResolvedType>,
        return_type: Box<ResolvedType>,
    },
    Void,
    Unknown,
    /// A type that has not yet been resolved.
    Unresolved(AstTypeId),
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
            ResolvedType::Primitive { type_id, type_args } => ResolvedType::Primitive {
                type_id: *type_id,
                type_args: type_args.iter().map(|t| self.apply(t)).collect(),
            },
            ResolvedType::Function {
                param_types,
                return_type,
            } => ResolvedType::Function {
                param_types: param_types.iter().map(|t| self.apply(t)).collect(),
                return_type: Box::new(self.apply(return_type)),
            },
            _ => ty.clone(),
        }
    }
}
