// ast.rs

/// The entire TransAct program, owning all data.
/// References inside these structs point back into these owned vectors.
pub struct Program<'ast> {
    /// All node definitions (NodeA, NodeB, etc.).
    pub nodes: Vec<NodeDef>,

    /// All table declarations, each referencing a node by &NodeDef.
    pub tables: Vec<TableDeclaration<'ast>>,

    /// All functions, each referencing zero or more nodes, etc.
    pub functions: Vec<FunctionDeclaration<'ast>>,
}

/// Each node has a name, e.g. "NodeA".
pub struct NodeDef {
    pub name: String,
}

/// A table definition references the NodeDef that stores it.
pub struct TableDeclaration<'ast> {
    /// Table name, e.g. "Customers"
    pub name: String,

    /// Direct reference to the node that holds this table (no leftover node_name).
    pub node: &'ast NodeDef,

    /// Table fields.
    pub fields: Vec<FieldDeclaration>,
}

/// Each field has a type and a name, e.g. "int customerID".
pub struct FieldDeclaration {
    pub field_type: TypeName,
    pub field_name: String,
}

#[derive(Debug, Clone)]
pub enum TypeName {
    Int,
    Float,
    String,
}

pub struct FunctionDeclaration<'ast> {
    pub return_type: ReturnType,
    pub name: String,
    pub parameters: Vec<ParameterDecl>,
    pub hops: Vec<HopBlock<'ast>>,
}

#[derive(Debug, Clone)]
pub enum ReturnType {
    Void,
    Type(TypeName),
}

pub struct ParameterDecl {
    pub param_type: TypeName,
    pub param_name: String,
}

pub struct HopBlock<'ast> {
    /// Which node this hop runs on.
    pub node: &'ast NodeDef,

    /// Statements in this hop block.
    pub statements: Vec<Statement>,
}

pub enum Statement {
    Assignment(AssignmentStatement),
    IfStmt(IfStatement),
    Empty,
}

pub struct AssignmentStatement {
    pub table: &'static TableDeclaration<'static>,

    /// The name of the PK column used.
    pub pk_column: String,

    /// Expression for the PK value, e.g. cid or 123.
    pub pk_expr: Expression,

    /// The field to update, e.g. "customerPoints"
    pub field_name: String,

    /// The new value expression, e.g. float literal 100.0
    pub rhs: Expression,
}

pub struct IfStatement {
    pub condition: Expression,
    pub then_branch: Vec<Statement>,
    pub else_branch: Option<Vec<Statement>>,
}

#[derive(Debug)]
pub enum Expression {
    /// Variable or parameter name, e.g. "total" or "custID".
    Ident(String),

    /// An integer literal, stored as an i64.
    IntLit(i64),

    /// A float literal, stored as an f64.
    FloatLit(f64),

    /// A string literal, e.g. "hello".
    StringLit(String),

    /// Unary operator (e.g., -expr, !expr).
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expression>,
    },

    /// Binary operator (e.g., expr + expr, expr && expr).
    BinaryOp {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>,
    },
}

#[derive(Debug)]
pub enum UnaryOp {
    Neg, // e.g. -expr
    Not, // e.g. !expr
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,    // +
    Sub,    // -
    Mul,    // *
    Div,    // /
    Eq,     // ==
    Neq,    // !=
    Lt,     // <
    Lte,    // <=
    Gt,     // >
    Gte,    // >=
    And,    // &&
    Or,     // ||
}