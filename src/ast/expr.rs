//! ast/expr.rs
//!
//! Expression AST nodes, representing constructs that evaluate to a value.

use super::common::{Identifier, IdentifierResolution};
use super::item::{FunctionId, ParamId, TableId};
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
        resolved_declarations: Vec<IdentifierResolution>,
        resolved_type: Option<ResolvedType>,
        span: Option<Span>,
    },
    Binary {
        left: ExprId,
        op: Identifier,
        right: ExprId,
        resolved_callables: Vec<FunctionId>,
        resolved_type: Option<ResolvedType>,
        span: Option<Span>,
    },
    Unary {
        op: Identifier,
        expr: ExprId,
        resolved_callables: Vec<FunctionId>,
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
        resolved_callables: Vec<FunctionId>,
        resolved_type: Option<ResolvedType>,
        span: Option<Span>,
    },
    MemberAccess {
        object: ExprId,
        member: Identifier,
        resolved_fields: Vec<IdentifierResolution>,
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
    pub resolved_field: Option<IdentifierResolution>,
    pub span: Option<Span>,
}

impl Expression {
    pub fn span(&self) -> Option<Span> {
        match self {
            Expression::Literal { span, .. }
            | Expression::Identifier { span, .. }
            | Expression::Binary { span, .. }
            | Expression::Unary { span, .. }
            | Expression::Assignment { span, .. }
            | Expression::Call { span, .. }
            | Expression::MemberAccess { span, .. }
            | Expression::TableRowAccess { span, .. }
            | Expression::Grouped { span, .. }
            | Expression::Lambda { span, .. } => *span,
        }
    }

    /// Get the resolved type of this expression
    pub fn resolved_type(&self) -> Option<&ResolvedType> {
        match self {
            Expression::Literal { resolved_type, .. }
            | Expression::Identifier { resolved_type, .. }
            | Expression::Binary { resolved_type, .. }
            | Expression::Unary { resolved_type, .. }
            | Expression::Assignment { resolved_type, .. }
            | Expression::Call { resolved_type, .. }
            | Expression::MemberAccess { resolved_type, .. }
            | Expression::TableRowAccess { resolved_type, .. }
            | Expression::Grouped { resolved_type, .. }
            | Expression::Lambda { resolved_type, .. } => resolved_type.as_ref(),
        }
    }
}
