use crate::ast::{Span, TypeName};

pub type Results<T> = Result<T, Vec<SpannedError>>;

#[derive(Debug, Clone)]
pub struct SpannedError {
    pub error: AstError,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub enum AstError {
    // Parsing errors
    ParseError(String),

    // Name resolution errors
    UndeclaredVariable(String),
    UndeclaredTable(String),
    UndeclaredField {
        table: String,
        field: String,
    },
    UndeclaredNode(String),
    DuplicateVariable(String),
    DuplicateFunction(String),
    DuplicateTable(String),
    DuplicateNode(String),
    GlobalVariableNotAllowed(String),

    // Type checking errors (for later use)
    TypeMismatch {
        expected: TypeName,
        found: TypeName,
    },
    InvalidUnaryOp {
        op: String,
        operand: TypeName,
    },
    InvalidBinaryOp {
        op: String,
        left: TypeName,
        right: TypeName,
    },
    InvalidCondition(TypeName),

    // Control flow errors
    BreakOutsideLoop,
    ContinueOutsideLoop,

    // Function/return errors
    MissingReturn(String),
    UnexpectedReturnValue,
    MissingReturnValue,

    // Cross-node access errors
    CrossNodeAccess {
        table: String,
        table_node: String,
        current_node: String,
    },
    InvalidPrimaryKey {
        table: String,
        column: String,
    },

    // Hop-specific errors
    AbortNotInFirstHop {
        function: String,
        hop_index: usize,
    },
}

impl std::fmt::Display for AstError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ParseError(msg) => write!(f, "Parse error: {}", msg),
            Self::UndeclaredVariable(name) => write!(f, "Undeclared variable: {}", name),
            Self::UndeclaredTable(name) => write!(f, "Undeclared table: {}", name),
            Self::UndeclaredField { table, field } => {
                write!(f, "Field '{}' not found in table '{}'", field, table)
            }
            Self::UndeclaredNode(name) => write!(f, "Undeclared node: {}", name),
            Self::DuplicateVariable(name) => write!(f, "Duplicate variable: {}", name),
            Self::DuplicateFunction(name) => write!(f, "Duplicate function: {}", name),
            Self::DuplicateTable(name) => write!(f, "Duplicate table: {}", name),
            Self::DuplicateNode(name) => write!(f, "Duplicate node: {}", name),
            Self::GlobalVariableNotAllowed(name) => {
                write!(f, "Global variables are not allowed: {}", name)
            }
            Self::TypeMismatch { expected, found } => write!(
                f,
                "Type mismatch: expected {:?}, found {:?}",
                expected, found
            ),
            Self::InvalidUnaryOp { op, operand } => {
                write!(f, "Invalid unary operation '{}' on type {:?}", op, operand)
            }
            Self::InvalidBinaryOp { op, left, right } => write!(
                f,
                "Invalid binary operation '{}' between {:?} and {:?}",
                op, left, right
            ),
            Self::InvalidCondition(ty) => {
                write!(f, "Invalid condition type: expected bool, found {:?}", ty)
            }
            Self::BreakOutsideLoop => write!(f, "Break statement outside of loop"),
            Self::ContinueOutsideLoop => write!(f, "Continue statement outside of loop"),
            Self::MissingReturn(func) => {
                write!(f, "Missing return statement in function '{}'", func)
            }
            Self::UnexpectedReturnValue => write!(f, "Unexpected return value in void function"),
            Self::MissingReturnValue => write!(f, "Missing return value in non-void function"),
            Self::CrossNodeAccess {
                table,
                table_node,
                current_node,
            } => write!(
                f,
                "Cannot access table '{}' on node '{}' from node '{}'",
                table, table_node, current_node
            ),
            Self::InvalidPrimaryKey { table, column } => write!(
                f,
                "Column '{}' is not the primary key of table '{}'",
                column, table
            ),
            Self::AbortNotInFirstHop {
                function,
                hop_index,
            } => write!(
                f,
                "Abort statement not allowed in hop {} of function '{}' (only in first hop)",
                hop_index, function
            ),
        }
    }
}

impl std::error::Error for AstError {}

pub fn format_errors(errors: &[SpannedError]) -> String {
    errors
        .iter()
        .map(|e| {
            if let Some(span) = &e.span {
                format!("Error at {}:{}: {}", span.line, span.column, e.error)
            } else {
                format!("Error: {}", e.error)
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}
