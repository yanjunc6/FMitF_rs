use std::rc::Rc;

/// The entire TransAct program, owning all data.
pub struct Program {
    /// All node definitions (NodeA, NodeB, etc.).
    pub nodes: Vec<Rc<NodeDef>>,

    /// All table declarations.
    pub tables: Vec<Rc<TableDeclaration>>,

    /// All functions.
    pub functions: Vec<FunctionDeclaration>,
}

/// Each node has a name, e.g. "NodeA".
#[derive(Debug, Clone)]
pub struct NodeDef {
    pub name: String,
}

/// A table definition references the NodeDef that stores it.
#[derive(Debug, Clone)]
pub struct TableDeclaration {
    /// Table name, e.g. "Customers"
    pub name: String,

    /// Reference to the node that holds this table.
    pub node: Rc<NodeDef>,

    /// Table fields.
    pub fields: Vec<FieldDeclaration>,
}

/// Each field has a type and a name, e.g. "int customerID".
#[derive(Debug, Clone)]
pub struct FieldDeclaration {
    pub field_type: TypeName,
    pub field_name: String,
}

#[derive(Debug, Clone)]
pub enum TypeName {
    Int,
    Float,
    String,
    Bool, // Add Boolean type
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub return_type: ReturnType,
    pub name: String,
    pub parameters: Vec<ParameterDecl>,
    pub hops: Vec<HopBlock>,
}

#[derive(Debug, Clone)]
pub enum ReturnType {
    Void,
    Type(TypeName),
}

#[derive(Debug, Clone)]
pub struct ParameterDecl {
    pub param_type: TypeName,
    pub param_name: String,
}

#[derive(Debug, Clone)]
pub struct HopBlock {
    /// Which node this hop runs on.
    pub node: Rc<NodeDef>,

    /// Statements in this hop block.
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assignment(AssignmentStatement),     // Table assignment
    VarAssignment(VarAssignmentStatement), // NEW: Variable assignment
    IfStmt(IfStatement),
    VarDecl(VarDeclStatement), // NEW: Variable declaration
    Return(ReturnStatement),   // NEW: Return statement
    Empty,
}

// Add new statement type:
#[derive(Debug, Clone)]
pub struct VarAssignmentStatement {
    pub var_name: String,
    pub rhs: Expression,
}

#[derive(Debug, Clone)]
pub struct AssignmentStatement {
    pub table: Rc<TableDeclaration>,

    /// The name of the PK column used.
    pub pk_column: String,

    /// Expression for the PK value, e.g. cid or 123.
    pub pk_expr: Expression,

    /// The field to update, e.g. "customerPoints"
    pub field_name: String,

    /// The new value expression, e.g. float literal 100.0
    pub rhs: Expression,
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub condition: Expression,
    pub then_branch: Vec<Statement>,
    pub else_branch: Option<Vec<Statement>>,
}

// Add is_global field to VarDeclStatement:
#[derive(Debug, Clone)]
pub struct VarDeclStatement {
    pub var_type: TypeName,
    pub var_name: String,
    pub init_value: Expression,
    pub is_global: bool,  // Whether this is a global variable within the function
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub value: Option<Expression>, // None for bare "return", Some(expr) for "return expr"
}

#[derive(Debug, Clone)]
pub enum Expression {
    /// Variable or parameter name, e.g. "total" or "custID".
    Ident(String),

    /// Integer literal, e.g. 42.
    IntLit(i64),

    /// Float literal, e.g. 3.14.
    FloatLit(f64),

    /// String literal, e.g. "hello".
    StringLit(String),

    /// Boolean literal, e.g. true, false.
    BoolLit(bool),

    /// Table field access, e.g. Users[userID: id].balance
    TableFieldAccess {
        table_name: String,
        pk_column: String,
        pk_expr: Box<Expression>,
        field_name: String,
    },

    /// Unary operation, e.g. !flag, -value.
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expression>,
    },

    /// Binary operation, e.g. a + b, x > y.
    BinaryOp {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>,
    },
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    /// Logical NOT: !
    Not,
    /// Arithmetic negation: -
    Neg,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    /// Addition: +
    Add,
    /// Subtraction: -
    Sub,
    /// Multiplication: *
    Mul,
    /// Division: /
    Div,
    /// Less than: <
    Lt,
    /// Less than or equal: <=
    Lte,
    /// Greater than: >
    Gt,
    /// Greater than or equal: >=
    Gte,
    /// Equal: ==
    Eq,
    /// Not equal: !=
    Neq,
    /// Logical AND: &&
    And,
    /// Logical OR: ||
    Or,
}
