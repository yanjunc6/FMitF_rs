//! ast/stmt.rs
//!
//! Statement AST nodes, like `let`, `if`, `return`, etc.

use super::common::{Decorator};
use super::expr::ExprId;
use super::item::VarId;
use crate::util::Span;
use id_arena::Id;

pub type StmtId = Id<Statement>;
pub type BlockId = Id<Block>;

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<StmtId>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    VarDecl(VarId),
    If {
        condition: ExprId,
        then_block: BlockId,
        else_block: Option<BlockId>,
        span: Option<Span>,
    },
    For {
        init: Option<ForInit>,
        condition: Option<ExprId>,
        update: Option<ExprId>,
        body: BlockId,
        span: Option<Span>,
    },
    Return {
        value: Option<ExprId>,
        span: Option<Span>,
    },
    Assert {
        expr: ExprId,
        span: Option<Span>,
    },
    Hop {
        decorators: Vec<Decorator>,
        body: BlockId,
        span: Option<Span>,
    },
    HopsFor {
        decorators: Vec<Decorator>,
        var: VarId, // Loop variable declaration
        start: ExprId,
        end: ExprId,
        body: BlockId,
        span: Option<Span>,
    },
    Expression {
        expr: ExprId,
        span: Option<Span>,
    },
    Block(BlockId),
}

#[derive(Debug, Clone)]
pub enum ForInit {
    VarDecl(VarId),
    Expression(ExprId),
}