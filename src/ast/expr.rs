//! ast/expr.rs
//!
//! Expression AST nodes, representing constructs that evaluate to a value.

use super::common::{Identifier, IdentifierResolution};
use super::item::{FunctionId, ParamId, TableField, TableId};
use super::stmt::BlockId;
use super::ty::{AstTypeId, ResolvedType};
use crate::util::Span;
use id_arena::Id;

pub type ExprId = Id<Expression>;

#[derive(Debug, Clone)]
pub enum Expression {
    Literal {
        value: Literal,
        resolved_type: Option<ResolvedType>,
        span: Option<Span>,
    },
    Identifier {
        name: Identifier,
        resolved_declaration: Option<IdentifierResolution>,
        resolved_type: Option<ResolvedType>,
        span: Option<Span>,
    },
    Binary {
        left: ExprId,
        op: Identifier,
        right: ExprId,
        resolved_callable: Option<FunctionId>,
        resolved_type: Option<ResolvedType>,
        span: Option<Span>,
    },
    Unary {
        op: Identifier,
        expr: ExprId,
        resolved_callable: Option<FunctionId>,
        resolved_type: Option<ResolvedType>,
        span: Option<Span>,
    },
    Assignment {
        lhs: ExprId,
        rhs: ExprId,
        resolved_type: Option<ResolvedType>,
        span: Option<Span>,
    },
    Call {
        callee: ExprId,
        args: Vec<ExprId>,
        resolved_callable: Option<FunctionId>,
        resolved_type: Option<ResolvedType>,
        span: Option<Span>,
    },
    MemberAccess {
        object: ExprId,
        member: Identifier,
        resolved_table: Option<TableId>,
        resolved_field: Option<TableField>,
        resolved_type: Option<ResolvedType>,
        span: Option<Span>,
    },
    TableRowAccess {
        table: ExprId,
        key_values: Vec<KeyValue>,
        resolved_table: Option<TableId>,
        resolved_type: Option<ResolvedType>,
        span: Option<Span>,
    },
    Grouped {
        expr: ExprId,
        resolved_type: Option<ResolvedType>,
        span: Option<Span>,
    },
    Lambda {
        params: Vec<ParamId>,
        return_type: AstTypeId,
        body: BlockId,
        resolved_type: Option<ResolvedType>,
        span: Option<Span>,
    },
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(String),
    Float(String),
    String(String),
    Bool(bool),
    List(Vec<ExprId>),
    RowLiteral(Vec<KeyValue>),
}

#[derive(Debug, Clone)]
pub struct KeyValue {
    pub key: Identifier,
    pub value: ExprId,
    pub resolved_table: Option<TableId>,
    pub resolved_field: Option<TableField>,
    pub span: Option<Span>,
}
