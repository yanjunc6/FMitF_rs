use std::rc::Rc;

pub struct Program {
    pub nodes: Vec<Rc<NodeDef>>,
    pub tables: Vec<Rc<TableDeclaration>>,
    pub functions: Vec<FunctionDeclaration>,
}

#[derive(Debug, Clone)]
pub struct NodeDef {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct TableDeclaration {
    pub name: String,
    pub node: Rc<NodeDef>,
    pub fields: Vec<FieldDeclaration>,
}

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
    Bool,
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
    pub node: Rc<NodeDef>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assignment(AssignmentStatement),
    VarAssignment(VarAssignmentStatement),
    IfStmt(IfStatement),
    VarDecl(VarDeclStatement),
    Return(ReturnStatement),
    Empty,
}

#[derive(Debug, Clone)]
pub struct VarAssignmentStatement {
    pub var_name: String,
    pub rhs: Expression,
}

#[derive(Debug, Clone)]
pub struct AssignmentStatement {
    pub table: Rc<TableDeclaration>,
    pub pk_column: String,
    pub pk_expr: Expression,
    pub field_name: String,
    pub rhs: Expression,
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub condition: Expression,
    pub then_branch: Vec<Statement>,
    pub else_branch: Option<Vec<Statement>>,
}

#[derive(Debug, Clone)]
pub struct VarDeclStatement {
    pub var_type: TypeName,
    pub var_name: String,
    pub init_value: Expression,
    pub is_global: bool,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub value: Option<Expression>,
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
