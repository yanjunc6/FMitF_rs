use std::rc::Rc;

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
        Self { start: 0, end: 0, line: 1, column: 1 }
    }
}

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

pub type Expression = Spanned<ExpressionKind>;
pub type Statement = Spanned<StatementKind>;

pub struct Program {
    pub nodes: Vec<Rc<NodeDef>>,
    pub tables: Vec<Rc<TableDeclaration>>,
    pub functions: Vec<FunctionDeclaration>,
}

#[derive(Debug, Clone)]
pub struct NodeDef {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TableDeclaration {
    pub name: String,
    pub node: Rc<NodeDef>,
    pub fields: Vec<FieldDeclaration>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FieldDeclaration {
    pub field_type: TypeName,
    pub field_name: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeName {
    Int, Float, String, Bool,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub return_type: ReturnType,
    pub name: String,
    pub parameters: Vec<ParameterDecl>,
    pub hops: Vec<Rc<HopBlock>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ReturnType {
    Void, Type(TypeName),
}

#[derive(Debug, Clone)]
pub struct ParameterDecl {
    pub param_type: TypeName,
    pub param_name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HopBlock {
    pub node: Rc<NodeDef>,
    pub statements: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
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
pub enum ExpressionKind {
    Ident(String),
    IntLit(i64),
    FloatLit(f64),
    StringLit(String),
    BoolLit(bool),
    TableFieldAccess {
        table_name: String,
        pk_column: String,
        pk_expr: Box<Expression>,
        field_name: String,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expression>,
    },
    BinaryOp {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>,
    },
}

#[derive(Debug, Clone)]
pub enum UnaryOp { Not, Neg }

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add, Sub, Mul, Div, Lt, Lte, Gt, Gte, Eq, Neq, And, Or,
}
