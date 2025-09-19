//! ast/mod.rs
//!
//! The root of the Abstract Syntax Tree module.
//!
//! This module organizes the AST into sub-modules for items, statements,
//! expressions, and types. It also defines the top-level `Program` struct
//! which contains the arenas for all AST nodes.

use id_arena::Arena;

// Re-export sub-modules for a clean public API
pub mod common;
pub mod expr;
pub mod fold;
pub mod item;
pub mod stmt;
pub mod ty;
pub mod visit;
pub mod visit_mut;

// Publicly re-export all the important types from submodules
pub use common::*;
pub use expr::*;
pub use item::*;
pub use stmt::*;
pub use ty::*;

// ============================================================================
// --- Program Root
// ============================================================================

/// The root AST node containing all declarations and arenas.
#[derive(Debug, Default)]
pub struct Program {
    /// Top-level declarations in order of appearance.
    pub declarations: Vec<Item>,

    // Declaration arenas
    pub functions: Arena<CallableDecl>,
    pub type_decls: Arena<TypeDecl>,
    pub const_decls: Arena<ConstDecl>,
    pub table_decls: Arena<TableDecl>,
    pub var_decls: Arena<VarDecl>,
    pub params: Arena<Parameter>,
    pub generic_params: Arena<GenericParam>,
    pub fields: Arena<TableField>,

    // Structure arenas
    pub types: Arena<AstType>,
    pub statements: Arena<Statement>,
    pub expressions: Arena<Expression>,
    pub blocks: Arena<Block>,
}
