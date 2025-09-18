//! ast/item.rs
//!
//! Top-level declarations (items) like functions, type definitions, and tables.

use super::common::{Decorator, Identifier};
use super::expr::ExprId;
use super::stmt::BlockId;
use super::ty::{AstTypeId, ResolvedType};
use crate::util::Span;
use id_arena::Id;

// Arena IDs for items
pub type FunctionId = Id<CallableDecl>;
pub type TypeDeclId = Id<TypeDecl>;
pub type ConstId = Id<ConstDecl>;
pub type TableId = Id<TableDecl>;
pub type VarId = Id<VarDecl>; // Technically a statement-level decl, but ID is needed here
pub type ParamId = Id<Parameter>;
pub type GenericParamId = Id<GenericParam>;

/// A top-level declaration in a file.
#[derive(Debug, Clone)]
pub enum Item {
    Callable(FunctionId),
    Type(TypeDeclId),
    Const(ConstId),
    Table(TableId),
}

/// Unified representation for functions, operators, partitions, and transactions.
#[derive(Debug, Clone)]
pub struct CallableDecl {
    pub decorators: Vec<Decorator>,
    pub kind: CallableKind,
    pub name: Identifier,
    pub generic_params: Vec<GenericParamId>,
    pub params: Vec<ParamId>,
    pub return_type: Option<AstTypeId>,
    pub assumptions: Vec<ExprId>,
    pub body: Option<BlockId>, // None if no implementation
    pub resolved_param_types: Option<Vec<ResolvedType>>,
    pub resolved_return_type: Option<ResolvedType>,
    pub resolved_function_type: Option<ResolvedType>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallableKind {
    Function,
    Operator,
    Partition,
    Transaction,
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub decorators: Vec<Decorator>,
    pub name: Identifier,
    pub generic_params: Vec<GenericParamId>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct ConstDecl {
    pub name: Identifier,
    pub ty: AstTypeId,
    pub value: ExprId,
    pub resolved_type: Option<ResolvedType>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct TableDecl {
    pub name: Identifier,
    pub elements: Vec<TableElement>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub enum TableElement {
    Field(TableField),
    Node(TableNode),
    Invariant(ExprId),
}

#[derive(Debug, Clone)]
pub struct TableField {
    pub is_primary: bool,
    pub name: Identifier,
    pub ty: AstTypeId,
    pub resolved_type: Option<ResolvedType>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct TableNode {
    pub name: Identifier,
    pub args: Vec<ExprId>,
    pub resolved_partitions: Vec<FunctionId>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct GenericParam {
    pub name: Identifier,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: Identifier,
    pub ty: AstTypeId,
    pub resolved_type: Option<ResolvedType>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: Identifier,
    pub ty: Option<AstTypeId>,
    pub init: Option<ExprId>,
    pub resolved_type: Option<ResolvedType>,
    pub span: Option<Span>,
}
