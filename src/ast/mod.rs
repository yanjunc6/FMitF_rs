use id_arena::{Arena, Id};
use std::collections::HashMap;

mod ast_builder;
pub mod errors;
mod name_resolver;
mod semantics_analysis;

// Re-export only the essential types users need
pub use errors::{AstError, Results, SpannedError};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
}

impl Span {
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
pub type NodeId = Id<NodeDef>;
pub type TableId = Id<TableDeclaration>;
pub type FieldId = Id<FieldDeclaration>;
pub type FunctionId = Id<FunctionDeclaration>;
pub type HopId = Id<HopBlock>;
pub type ParameterId = Id<ParameterDecl>;
pub type StatementId = Id<Statement>;
pub type ExpressionId = Id<Expression>;
pub type VarId = Id<VarDecl>;
pub type ScopeId = Id<Scope>;

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

pub type Expression = Spanned<ExpressionKind>;
pub type Statement = Spanned<StatementKind>;

/// Main Program structure - this is what users get after processing
#[derive(Debug)]
pub struct Program {
    // Arena storage - keep public for read access
    pub nodes: Arena<NodeDef>,
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
    pub root_nodes: Vec<NodeId>,
    pub root_tables: Vec<TableId>,
    pub root_functions: Vec<FunctionId>,

    // Lookup maps - public for convenience
    pub node_map: HashMap<String, NodeId>,
    pub table_map: HashMap<String, TableId>,
    pub function_map: HashMap<String, FunctionId>,

    // Resolution results - public for type checking access
    pub global_scope: Option<ScopeId>,
    pub resolutions: HashMap<ExpressionId, VarId>,
    pub var_types: HashMap<VarId, TypeName>,
}

// Essential AST node types - keep these public
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NodeDef {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TableDeclaration {
    pub name: String,
    pub node: NodeId,
    pub fields: Vec<FieldId>,
    pub primary_key: FieldId,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldDeclaration {
    pub field_type: TypeName,
    pub field_name: String,
    pub is_primary: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeName {
    Int,
    Float,
    String,
    Bool,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub return_type: ReturnType,
    pub name: String,
    pub parameters: Vec<ParameterId>,
    pub hops: Vec<HopId>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ReturnType {
    Void,
    Type(TypeName),
}

#[derive(Debug, Clone)]
pub struct ParameterDecl {
    pub param_type: TypeName,
    pub param_name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HopBlock {
    pub node_name: String,
    pub statements: Vec<StatementId>,
    pub span: Span,
    pub resolved_node: Option<NodeId>,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Assignment(AssignmentStatement),
    VarAssignment(VarAssignmentStatement),
    IfStmt(IfStatement),
    WhileStmt(WhileStatement),
    VarDecl(VarDeclStatement),
    Return(ReturnStatement),
    Abort(AbortStatement),
    Break(BreakStatement),
    Continue(ContinueStatement),
    Empty,
}

#[derive(Debug, Clone)]
pub struct VarAssignmentStatement {
    pub var_name: String,
    pub rhs: ExpressionId,
    pub resolved_var: Option<VarId>,
}

#[derive(Debug, Clone)]
pub struct AssignmentStatement {
    pub table_name: String,
    pub pk_field_name: String,
    pub pk_expr: ExpressionId,
    pub field_name: String,
    pub rhs: ExpressionId,
    pub resolved_table: Option<TableId>,
    pub resolved_pk_field: Option<FieldId>,
    pub resolved_field: Option<FieldId>,
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
pub struct VarDeclStatement {
    pub var_type: TypeName,
    pub var_name: String,
    pub init_value: ExpressionId,
    pub is_global: bool,
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
pub enum ExpressionKind {
    Ident(String),
    IntLit(i64),
    FloatLit(f64),
    StringLit(String),
    BoolLit(bool),
    TableFieldAccess {
        table_name: String,
        pk_field_name: String,
        pk_expr: ExpressionId,
        field_name: String,
        resolved_table: Option<TableId>,
        resolved_pk_field: Option<FieldId>,
        resolved_field: Option<FieldId>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: ExpressionId,
    },
    BinaryOp {
        left: ExpressionId,
        op: BinaryOp,
        right: ExpressionId,
    },
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum VarKind {
    Parameter,
    Local,
}

#[derive(Debug)]
pub struct Scope {
    pub parent: Option<ScopeId>,
    pub kind: ScopeKind,
    pub variables: HashMap<String, VarId>,
}

#[derive(Debug, Clone)]
pub enum ScopeKind {
    Global,
    Function(FunctionId),
    Block,
    Hop(HopId),
}

/// Main public API - this is the only function users need to call
pub fn parse_and_analyze(source: &str) -> Results<Program> {
    // Parse and build AST using ast_builder
    let mut program = ast_builder::parse_and_build(source)?;

    // Perform name resolution
    name_resolver::resolve_names(&mut program)?;

    // Perform semantic analysis
    semantics_analysis::analyze_program(&mut program)?;

    Ok(program)
}
