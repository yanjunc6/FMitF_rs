//! ast/mod.rs
//!
//! Abstract Syntax Tree (AST) for the distributed data processing DSL.
//!
//! This module defines the data structures representing parsed code. It uses an
//! arena-based approach where all nodes are stored in arenas and referenced by IDs.
//! The AST is designed to be annotated in multiple passes:
//! - Name resolution: fills in `resolved` fields to link uses to declarations
//! - Type resolution: fills in `resolved_type` fields with concrete types

use crate::util::Span;
use id_arena::{Arena, Id};

// ============================================================================
// --- Arena-based Node IDs
// ============================================================================

// Declaration IDs
pub type FunctionId = Id<CallableDecl>;
pub type TypeDeclId = Id<TypeDecl>;
pub type ConstId = Id<ConstDecl>;
pub type TableId = Id<TableDecl>;
pub type VarId = Id<VarDecl>;
pub type ParamId = Id<Parameter>;
pub type GenericParamId = Id<GenericParam>;

// Structure IDs
pub type TypeId = Id<Type>;
pub type StmtId = Id<Statement>;
pub type ExprId = Id<Expression>;
pub type BlockId = Id<Block>;

// ============================================================================
// --- Identifiers and References
// ============================================================================

/// An identifier with resolution information.
#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
    pub span: Option<Span>,
    /// Resolved declaration this identifier refers to (filled by name resolver).
    pub resolved: Option<DeclRef>,
    /// Resolved type of this identifier (filled by type resolver).
    pub resolved_type: Option<ResolvedType>,
}

/// Reference to any declaration that can be named.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DeclRef {
    Function(FunctionId),
    Type(TypeDeclId),
    Table(TableId),
    Const(ConstId),
    Var(VarId),
    Param(ParamId),
    GenericParam(GenericParamId),
}

/// Fully resolved type after type checking.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedType {
    /// A resolved type, like `int`, `string`, or a user-defined struct.
    Type(TypeDeclId),
    Table(TableId),
    TypeVar(GenericParamId),
    Generic {
        base: TypeDeclId,
        args: Vec<ResolvedType>,
    },
    Function {
        params: Vec<ResolvedType>,
        return_type: Box<ResolvedType>,
    },
    Void,
}

/// Wraps any value with its source location.
#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub value: T,
    /// The location of this value in the source code. `None` for built-ins or generated code.
    pub span: Option<Span>,
}

// ============================================================================
// --- Program Root
// ============================================================================

/// The root AST node containing all declarations and arenas.
#[derive(Debug)]
pub struct Program {
    /// Top-level declarations in order of appearance.
    pub declarations: Vec<Declaration>,

    // Declaration arenas
    pub functions: Arena<CallableDecl>,
    pub type_decls: Arena<TypeDecl>,
    pub const_decls: Arena<ConstDecl>,
    pub table_decls: Arena<TableDecl>,
    pub var_decls: Arena<VarDecl>,
    pub params: Arena<Parameter>,
    pub generic_params: Arena<GenericParam>,

    // Structure arenas
    pub types: Arena<Type>,
    pub statements: Arena<Statement>,
    pub expressions: Arena<Expression>,
    pub blocks: Arena<Block>,
}

// ============================================================================
// --- Top-Level Declarations
// ============================================================================

#[derive(Debug, Clone)]
pub enum Declaration {
    Callable(FunctionId),
    Type(TypeDeclId),
    Const(ConstId),
    Table(TableId),
}

// Represents a decorator
#[derive(Debug, Clone)]
pub struct Decorator {
    pub name: Identifier,
    pub span: Option<Span>,
}

// Represents the name of a callable, distinguishing between regular identifiers and operator symbols.
#[derive(Debug, Clone)]
pub enum CallableName {
    Identifier(Identifier),
    Operator(Spanned<String>),
}

/// Unified representation for functions, operators, partitions, and transactions.
#[derive(Debug, Clone)]
pub struct CallableDecl {
    pub decorators: Vec<Decorator>,
    pub kind: CallableKind,
    pub name: CallableName,
    pub generic_params: Vec<GenericParamId>,
    pub params: Vec<ParamId>,
    pub return_type: Option<TypeId>,
    pub assumptions: Vec<ExprId>,
    pub body: Option<BlockId>, // None for forward declarations
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
    pub ty: TypeId,
    pub value: ExprId,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub enum TableElement {
    Field(TableField),
    Node(TableNode),
    Invariant(ExprId),
}

#[derive(Debug, Clone)]
pub struct TableDecl {
    pub name: Identifier,
    // A single vector of `TableElement` preserves source order.
    pub elements: Vec<TableElement>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct TableField {
    pub is_primary: bool,
    pub name: Identifier,
    pub ty: TypeId,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct TableNode {
    pub name: Identifier,
    pub args: Vec<ExprId>,
    pub resolved_partition: Option<FunctionId>, // Filled by name resolver
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
    pub ty: TypeId,
    pub span: Option<Span>,
}

// ============================================================================
// --- Types
// ============================================================================

/// Syntactic representation of types from source code.
#[derive(Debug, Clone)]
pub enum Type {
    /// Simple type name: `int`, `MyTable`, `T`
    Named(Identifier),

    /// Generic instantiation: `List<int>`, `Map<K, V>`
    Generic {
        base: Identifier,
        args: Vec<TypeId>,
        span: Option<Span>,
    },

    /// Function type: `(int, bool) -> string`
    Function {
        params: Vec<TypeId>,
        return_type: TypeId,
        span: Option<Span>,
    },
}

// ============================================================================
// --- Statements
// ============================================================================

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

#[derive(Debug, Clone)]
pub struct VarDecl {
    // CHANGED: Use `Identifier` for the variable's name.
    pub name: Identifier,
    pub ty: Option<TypeId>,
    pub init: Option<ExprId>,
    pub span: Option<Span>,
}

// ============================================================================
// --- Expressions
// ============================================================================

#[derive(Debug, Clone)]
pub enum Expression {
    Literal {
        value: Literal,
        span: Option<Span>,
    },

    Identifier(Identifier),

    Binary {
        left: ExprId,
        op: Spanned<String>,
        right: ExprId,
        span: Option<Span>,
    },

    Unary {
        op: Spanned<String>,
        expr: ExprId,
        span: Option<Span>,
    },

    Assignment {
        lhs: ExprId,
        rhs: ExprId,
        span: Option<Span>,
    },

    Call {
        callee: ExprId,
        args: Vec<ExprId>,
        resolved_callable: Option<FunctionId>, // Filled by name resolver
        span: Option<Span>,
    },

    MemberAccess {
        object: ExprId,
        // CHANGED: Use `Identifier` for the member name, which matches the grammar.
        member: Identifier,
        // This is still useful for linking directly to a table field definition.
        resolved_field: Option<TableField>,
        span: Option<Span>,
    },

    TableRowAccess {
        table: ExprId,
        key_values: Vec<KeyValue>,
        span: Option<Span>,
    },

    Grouped {
        expr: ExprId,
        span: Option<Span>,
    },

    Lambda {
        params: Vec<ParamId>,
        return_type: TypeId,
        body: BlockId,
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
    RowLiteral(Vec<KeyValue>), // Distinguished from list
}

#[derive(Debug, Clone)]
pub struct KeyValue {
    // CHANGED: Use `Identifier` for the key, which aligns with the grammar.
    pub key: Identifier,
    pub value: ExprId,
    pub resolved_field: Option<TableField>, // Filled by name resolver
    pub span: Option<Span>,
}

// ============================================================================
// --- Main Parse Function
// ============================================================================

/// Parse source code into AST with complete processing including name resolution,
/// type checking, and semantic analysis
pub fn parse_and_analyze_program(source: &str) -> Result<Program, Vec<errors::AstError>> {
    // Stage 1: Basic AST parsing with prelude
    let mut program = match ast_builder::parse_program(source) {
        Ok(program) => program,
        Err(error) => return Err(error),
    };

    // Stage 2: Name resolution
    if let Err(errors) = name_resolver::resolve_names(&mut program) {
        return Err(errors);
    }

    // Stage 3: Type checking (disabled for now)
    // if let Err(errors) = type_checker::check_types(&mut program) {
    //     return Err(errors);
    // }

    // Stage 4: Semantic analysis (disabled for now)
    // if let Err(errors) = semantic_analyzer::analyze_semantics(&program) {
    //     return Err(errors);
    // }

    Ok(program)
}

// ============================================================================
// --- Module Declarations
// ============================================================================

pub mod ast_builder;
pub mod errors;
pub mod prelude;
pub mod name_resolver;
// pub mod type_checker;
// pub mod semantic_analyzer;
// Complex modules with legacy issues:
// pub mod constant_checker;
// pub mod ast_debug;

// ============================================================================
// --- Public Interface Re-exports
// ============================================================================

// Core types - using unified error system
pub use errors::{AstError, AstErrorKind};

// AST builder functions

// Analysis phases (disabled for now)
// pub use name_resolver::resolve_names;
// pub use type_resolver::resolve_types;
// pub use semantics_analysis::analyze_semantics;
// pub use constant_checker::{check_constants, evaluate_constant_expression, ConstantValue};

// Debug utilities (disabled for now)
// pub use ast_debug::{print_program, print_expression, print_statement, DebugConfig};
