//! ast/common.rs
//!
//! Common, reusable data structures for the AST like identifiers and spans.

use super::item::{ConstId, FunctionId, GenericParamId, ParamId, TableId, TypeDeclId, VarId};
use crate::util::Span;

/// An identifier - just a name and location
/// operator names are also represented as Identifiers
#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
    pub span: Option<Span>,
}

/// What an identifier can resolve to - used in Identifier expressions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IdentifierResolution {
    Function(FunctionId),
    Type(TypeDeclId),
    Table(TableId),
    Const(ConstId),
    Var(VarId),
    Param(ParamId),
    GenericParam(GenericParamId),
}

/// Represents a decorator like `@hop`.
#[derive(Debug, Clone)]
pub struct Decorator {
    pub name: Identifier,
    pub span: Option<Span>,
}

/// Wraps any value with its source location.
#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub value: T,
    /// The location of this value in the source code. `None` for built-ins or generated code.
    pub span: Option<Span>,
}
