//! The `ast` module defines the Abstract Syntax Tree (AST) structure for the TransAct language.
//! It provides the core data structures and utilities for parsing, analyzing, and representing
//! the language constructs. The AST is designed to be arena-based for efficient memory management
//! and supports various operations such as name resolution and semantic analysis.
//!
//! # Overview
//!
//! The AST is composed of several key components:
//!
//! - **Span**: Represents a span in the source code with start and end positions, line, and column.
//! - **Program**: The main structure representing the entire parsed and analyzed program.
//! - **NodeDef**: Represents a node definition in the AST.
//! - **TableDeclaration**: Represents a table declaration with fields and primary keys.
//! - **FieldDeclaration**: Represents a field in a table.
//! - **FunctionDeclaration**: Represents a function with parameters and hops.
//! - **StatementKind**: Represents various types of statements such as assignments, loops, and returns.
//! - **ExpressionKind**: Represents expressions including literals, identifiers, and operations.
//!
//! The module also includes utility functions for parsing and analyzing the source code.
//!
//! # Features
//!
//! - Arena-based memory management for efficient allocation and deallocation.
//! - Comprehensive error handling with spans for precise error reporting.
//! - Support for complex language constructs such as composite keys and cross-node access.
//!
//! # Usage
//!
//! To parse and analyze a source file, use the `parse_and_analyze` function:
//!
//! ```rust
//! use crate::ast::parse_and_analyze;
//!
//! let source = "..."; // TransAct source code
//! let program = parse_and_analyze(source).expect("Failed to parse and analyze");
//! ```

use id_arena::{Arena, Id};
use std::collections::HashMap;

pub mod analysis;
mod ast_builder;
pub mod errors;
mod name_resolver;

// Re-export only the essential types users need
pub use errors::{AstError, Results, SpannedError};

/// Represents a span in the source code with start and end positions, line, and column.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
}

impl Span {
    /// Converts a Pest span into a Span.
    pub fn from_pest(span: pest::Span) -> Self {
        let (line, column) = span.start_pos().line_col();
        Self {
            start: span.start(),
            end: span.end(),
            line,
            column,
        }
    }
}

impl Default for Span {
    /// Provides a default span value.
    fn default() -> Self {
        Self {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
        }
    }
}

// Arena-based IDs - keep these public for external use
pub type PartitionId = Id<PartitionDeclaration>;
pub type TableId = Id<TableDeclaration>;
pub type FieldId = Id<FieldDeclaration>;
pub type FunctionId = Id<FunctionDeclaration>;
pub type HopId = Id<HopBlock>;
pub type ParameterId = Id<ParameterDecl>;
pub type StatementId = Id<Statement>;
pub type ExpressionId = Id<Expression>;
pub type VarId = Id<VarDecl>;
pub type ConstId = Id<ConstDeclaration>;
pub type ScopeId = Id<Scope>;

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

pub type Expression = Spanned<ExpressionKind>;
pub type Statement = Spanned<StatementKind>;

/// Main Program structure - this is what users get after processing.
#[derive(Debug)]
pub struct Program {
    // Arena storage - keep public for read access
    pub partitions: Arena<PartitionDeclaration>,
    pub constants: Arena<ConstDeclaration>,
    pub tables: Arena<TableDeclaration>,
    pub fields: Arena<FieldDeclaration>,
    pub functions: Arena<FunctionDeclaration>,
    pub hops: Arena<HopBlock>,
    pub parameters: Arena<ParameterDecl>,
    pub statements: Arena<Statement>,
    pub expressions: Arena<Expression>,
    pub variables: Arena<VarDecl>,
    pub scopes: Arena<Scope>,

    // Root collections - public for iteration
    pub root_partitions: Vec<PartitionId>,
    pub root_constants: Vec<VarId>,
    pub root_tables: Vec<TableId>,
    pub root_functions: Vec<FunctionId>,

    // Lookup maps - public for convenience
    pub partition_map: HashMap<String, PartitionId>,
    pub constant_map: HashMap<String, VarId>,
    pub table_map: HashMap<String, TableId>,
    pub function_map: HashMap<String, FunctionId>,

    // Resolution results - public for type checking access
    pub resolutions: HashMap<ExpressionId, VarId>,
    pub var_types: HashMap<VarId, TypeName>,
    pub parameter_resolutions: HashMap<ParameterId, VarId>, // Maps parameters to their variable declarations

    // Unique ID mappings for CFG builder - all identifier references resolved to their declarations
    pub table_resolutions: HashMap<ExpressionId, TableId>, // Expression references to tables
    pub function_resolutions: HashMap<ExpressionId, FunctionId>, // Function call references
}

impl Default for Program {
    fn default() -> Self {
        Self {
            partitions: Arena::new(),
            constants: Arena::new(),
            tables: Arena::new(),
            fields: Arena::new(),
            functions: Arena::new(),
            hops: Arena::new(),
            parameters: Arena::new(),
            statements: Arena::new(),
            expressions: Arena::new(),
            variables: Arena::new(),
            scopes: Arena::new(),
            root_partitions: Vec::new(),
            root_constants: Vec::new(),
            root_tables: Vec::new(),
            root_functions: Vec::new(),
            partition_map: HashMap::new(),
            constant_map: HashMap::new(),
            table_map: HashMap::new(),
            function_map: HashMap::new(),
            resolutions: HashMap::new(),
            var_types: HashMap::new(),
            parameter_resolutions: HashMap::new(),
            table_resolutions: HashMap::new(),
            function_resolutions: HashMap::new(),
        }
    }
}

/// Represents a partition function declaration.
/// Partitions are function-like with typed parameters and always return int.
#[derive(Debug, Clone, PartialEq)]
pub struct PartitionDeclaration {
    pub name: String,
    pub parameters: Vec<ParameterId>, // Typed parameters like functions
    pub implementation: Option<ExpressionId>, // Optional implementation expression
    pub span: Span,
    // Note: Return type is always int (implicitly), no need to store it
}

/// Represents a constant declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct ConstDeclaration {
    pub const_type: TypeName,
    pub name: String,
    pub value: ExpressionId,
    pub span: Span,
}

/// Represents a table declaration in the AST.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TableDeclaration {
    pub name: String,
    pub fields: Vec<FieldId>,
    pub primary_keys: Vec<FieldId>,
    pub node_partition: Option<NodePartition>,
    pub span: Span,
}

/// Represents a node partition specification.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NodePartition {
    pub partition_name: String,
    pub arguments: Vec<String>, // Field names used as arguments
    pub resolved_partition: Option<PartitionId>,
}

/// Represents a field declaration in the AST.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldDeclaration {
    pub field_type: TypeName,
    pub field_name: String,
    pub is_primary: bool,
    pub span: Span,
}

/// Represents type names in the AST.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeName {
    Int,
    Float,
    String,
    Bool,
    Array {
        element_type: Box<TypeName>,
        size: Option<usize>, // None for dynamic arrays, Some(n) for fixed-size
        size_expr: Option<ExpressionId>, // The original size expression for semantic analysis
    },
    Table(String), // For table variable declarations like "Stock s"
}

/// Represents a function declaration in the AST.
#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub return_type: ReturnType,
    pub name: String,
    pub parameters: Vec<ParameterId>,
    pub hops: Vec<HopId>,
    pub span: Span,
}

/// Represents the return type of a function.
#[derive(Debug, Clone, PartialEq)]
pub enum ReturnType {
    Void,
    Type(TypeName),
}

/// Represents a parameter declaration in the AST.
#[derive(Debug, Clone)]
pub struct ParameterDecl {
    pub param_type: TypeName,
    pub param_name: String,
    pub span: Span,
}

/// Represents a hop block in the AST.
#[derive(Debug, Clone)]
pub struct HopBlock {
    pub statements: Vec<StatementId>,
    pub span: Span,
    pub hop_type: HopType,
}

/// Represents the type of hop block.
#[derive(Debug, Clone)]
pub enum HopType {
    Simple, // hop { ... }
    ForLoop {
        loop_var: String,
        loop_var_type: TypeName,
        start: ExpressionId,
        end: ExpressionId,
        // Optional pre-computed constant values for CFG expansion
        start_value: Option<i64>,
        end_value: Option<i64>,
    }, // hops for int var = start to end { ... }
}

/// Represents a statement in the AST.
#[derive(Debug, Clone)]
pub enum StatementKind {
    Assignment(AssignmentStatement),
    VarDecl(VarDeclStatement),
    IfStmt(IfStatement),
    ForStmt(ForStatement),
    WhileStmt(WhileStatement),
    Return(ReturnStatement),
    Abort(AbortStatement),
    Break(BreakStatement),
    Continue(ContinueStatement),
    Expression(ExpressionStatement),
    Empty,
}

/// Represents an assignment statement in the AST.
#[derive(Debug, Clone)]
pub struct AssignmentStatement {
    pub lvalue: LValue,
    pub operator: AssignmentOperator,
    pub rhs: ExpressionId,
}

/// Represents an assignment operator.
#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentOperator {
    Assign,    // =
    AddAssign, // +=
    SubAssign, // -=
    MulAssign, // *=
    DivAssign, // /=
}

/// Represents a left-hand side value that can be assigned to.
#[derive(Debug, Clone)]
pub enum LValue {
    Var {
        name: String,
        resolved_var: Option<VarId>,
    },
    TableField {
        table_name: String,
        pk_fields: Vec<String>,
        pk_exprs: Vec<ExpressionId>,
        field_name: String,
        resolved_table: Option<TableId>,
        resolved_pk_fields: Vec<Option<FieldId>>,
        resolved_field: Option<FieldId>,
    },
    TableRecord {
        table_name: String,
        pk_fields: Vec<String>,
        pk_exprs: Vec<ExpressionId>,
        resolved_table: Option<TableId>,
        resolved_pk_fields: Vec<Option<FieldId>>,
    },
    ArrayElement {
        array_name: String,
        index: ExpressionId,
        resolved_var: Option<VarId>,
    },
    FieldAccess {
        object_name: String,
        field_name: String,
        resolved_var: Option<VarId>,
        resolved_field: Option<FieldId>,
    },
}

#[derive(Debug, Clone)]
pub struct VarDeclStatement {
    pub var_type: TypeName,
    pub var_name: String,
    pub init_value: Option<ExpressionId>,
}

#[derive(Debug, Clone)]
pub struct ForStatement {
    pub loop_var: String,
    pub loop_var_type: TypeName,
    pub init: ExpressionId,
    pub condition: ExpressionId,
    pub increment: ExpressionId,
    pub body: Vec<StatementId>,
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub condition: ExpressionId,
    pub then_branch: Vec<StatementId>,
    pub else_branch: Option<Vec<StatementId>>,
}

#[derive(Debug, Clone)]
pub struct WhileStatement {
    pub condition: ExpressionId,
    pub body: Vec<StatementId>,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub value: Option<ExpressionId>,
}

#[derive(Debug, Clone)]
pub struct AbortStatement;

#[derive(Debug, Clone)]
pub struct BreakStatement;

#[derive(Debug, Clone)]
pub struct ContinueStatement;

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub expression: ExpressionId,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Ident(String),
    IntLit(i64),
    FloatLit(f64),
    StringLit(String),
    BoolLit(bool),
    TableFieldAccess {
        table_name: String,
        pk_fields: Vec<String>,
        pk_exprs: Vec<ExpressionId>,
        field_name: String,
        resolved_table: Option<TableId>,
        resolved_pk_fields: Vec<Option<FieldId>>,
        resolved_field: Option<FieldId>,
        resolved_type: Option<TypeName>,
    },
    TableAccess {
        table_name: String,
        pk_fields: Vec<String>,
        pk_exprs: Vec<ExpressionId>,
        resolved_table: Option<TableId>,
        resolved_pk_fields: Vec<Option<FieldId>>,
        resolved_type: Option<TypeName>,
    },
    ArrayAccess {
        array_name: String,
        index: ExpressionId,
        resolved_var: Option<VarId>,
        resolved_type: Option<TypeName>,
    },
    FieldAccess {
        object_name: String,
        field_name: String,
        resolved_var: Option<VarId>,
        resolved_field: Option<FieldId>,
        resolved_type: Option<TypeName>,
    },
    RecordLiteral {
        fields: Vec<FieldAssignment>,
        resolved_type: Option<TypeName>,
    },
    ArrayLiteral {
        elements: Vec<ExpressionId>,
        resolved_type: Option<TypeName>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: ExpressionId,
        resolved_type: Option<TypeName>,
    },
    BinaryOp {
        left: ExpressionId,
        op: BinaryOp,
        right: ExpressionId,
        resolved_type: Option<TypeName>,
    },
}

/// Represents a field assignment in a record literal.
#[derive(Debug, Clone)]
pub struct FieldAssignment {
    pub field_name: String,
    pub value: ExpressionId,
    pub resolved_field: Option<FieldId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Not,
    Neg,
    PreIncrement,
    PostIncrement,
    PreDecrement,
    PostDecrement,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Neq,
    And,
    Or,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: String,
    pub ty: TypeName,
    pub kind: VarKind,
    pub defined_at: Span,
    pub scope: ScopeId,
}

/// Represents the kind of a variable (parameter or local).
#[derive(Debug, Clone, PartialEq)]
pub enum VarKind {
    Parameter,
    Local,
    Global,
}

#[derive(Debug)]
pub struct Scope {
    pub parent: Option<ScopeId>,
    pub variables: HashMap<String, VarId>,
}

/// Parses and analyzes the source code to produce a `Program`.
pub fn parse_and_analyze(source: &str) -> Results<Program> {
    let mut program = ast_builder::parse_and_build(source)?;
    name_resolver::resolve_names(&mut program)?;
    analysis::analyze_program_with_types(&mut program)?;
    Ok(program)
}
