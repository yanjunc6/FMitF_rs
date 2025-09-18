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

/// A unique identifier for an inference variable.
/// This corresponds to the α, β, etc. in our discussion.
pub type InferVarId = u32;

/// Represents a monotype in the type system.
/// This is the primary structure that the unification algorithm operates on.
/// It contains no "forall" quantifiers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedType {
    /// A declared type constructor, like `int`, `bool`, `List`, or `Row`.
    /// For `List<int>`, `decl_id` is `List` and `args` is `[Type::Int]`.
    Declared {
        decl_id: TypeDeclId,
        args: Vec<ResolvedType>,
    },

    /// A specific table type. This could be a special case of `Declared`,
    /// but keeping it separate can be convenient.
    Table {
        table_id: TableId,
    },

    /// An unsolved inference variable. This is a placeholder that the
    /// type checker will attempt to solve during unification.
    InferVar(InferVarId),

    /// A rigid generic type parameter, bound by an enclosing function's
    /// `TypeScheme`. For a function `f<T>(a: T)`, the type of `a`
    /// within the function body is `GenericParam(id_of_T)`.
    GenericParam(GenericParamId),

    /// A function type.
    Function {
        param_types: Vec<ResolvedType>,
        return_type: Box<ResolvedType>,
    },

    Void, // Represents the absence of a value, e.g., from a procedure.
}

/// Represents a polytype, i.e., a type scheme with "forall" quantifiers.
/// This is what is stored in the environment for generic functions.
/// Example: `∀T, U. (T, U) -> T`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeScheme {
    /// The list of generic parameters that are quantified over.
    /// For `∀T, U. ...`, this would contain the `GenericParamId`s for T and U.
    pub quantified_params: Vec<GenericParamId>,

    /// The underlying monotype.
    pub ty: ResolvedType,
}

impl std::fmt::Display for ResolvedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedType::Declared { decl_id, args } => {
                write!(f, "Declared({:?}", decl_id)?;
                if !args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", arg)?;
                    }
                    write!(f, ">")?;
                }
                write!(f, ")")
            }
            ResolvedType::Table { table_id } => write!(f, "Table({:?})", table_id),
            ResolvedType::InferVar(id) => write!(f, "'{}", id),
            ResolvedType::GenericParam(id) => write!(f, "GenericParam({:?})", id),
            ResolvedType::Function {
                param_types,
                return_type,
            } => {
                write!(f, "(")?;
                for (i, param) in param_types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ") -> {}", return_type)
            }
            ResolvedType::Void => write!(f, "void"),
        }
    }
}
