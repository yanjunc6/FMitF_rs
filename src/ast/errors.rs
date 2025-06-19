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
        write!(f, "{}: {}", self.error_type(), self.message())
    }
}

impl std::error::Error for AstError {}

impl AstError {
    /// Get the error type name for display purposes
    pub fn error_type(&self) -> &'static str {
        match self {
            Self::ParseError(_) => "ParseError",
            Self::UndeclaredVariable(_) => "UndeclaredVariable",
            Self::UndeclaredTable(_) => "UndeclaredTable",
            Self::UndeclaredField { .. } => "UndeclaredField",
            Self::UndeclaredNode(_) => "UndeclaredNode",
            Self::DuplicateVariable(_) => "DuplicateVariable",
            Self::DuplicateFunction(_) => "DuplicateFunction",
            Self::DuplicateTable(_) => "DuplicateTable",
            Self::DuplicateNode(_) => "DuplicateNode",
            Self::TypeMismatch { .. } => "TypeMismatch",
            Self::InvalidUnaryOp { .. } => "InvalidUnaryOp",
            Self::InvalidBinaryOp { .. } => "InvalidBinaryOp",
            Self::InvalidCondition(_) => "InvalidCondition",
            Self::BreakOutsideLoop => "BreakOutsideLoop",
            Self::ContinueOutsideLoop => "ContinueOutsideLoop",
            Self::MissingReturn(_) => "MissingReturn",
            Self::UnexpectedReturnValue => "UnexpectedReturnValue",
            Self::MissingReturnValue => "MissingReturnValue",
            Self::CrossNodeAccess { .. } => "CrossNodeAccess",
            Self::InvalidPrimaryKey { .. } => "InvalidPrimaryKey",
            Self::AbortNotInFirstHop { .. } => "AbortNotInFirstHop",
        }
    }

    /// Get the error message without the type prefix
    pub fn message(&self) -> String {
        match self {
            Self::ParseError(msg) => msg.clone(),
            Self::UndeclaredVariable(name) => format!("Variable '{}' is not declared", name),
            Self::UndeclaredTable(name) => format!("Table '{}' is not declared", name),
            Self::UndeclaredField { table, field } => {
                format!("Field '{}' does not exist in table '{}'", field, table)
            }
            Self::UndeclaredNode(name) => format!("Node '{}' is not declared", name),
            Self::DuplicateVariable(name) => format!("Variable '{}' is already declared", name),
            Self::DuplicateFunction(name) => format!("Function '{}' is already declared", name),
            Self::DuplicateTable(name) => format!("Table '{}' is already declared", name),
            Self::DuplicateNode(name) => format!("Node '{}' is already declared", name),
            Self::TypeMismatch { expected, found } => format!(
                "Expected type {:?} but found {:?}",
                expected, found
            ),
            Self::InvalidUnaryOp { op, operand } => {
                format!("Cannot apply operator '{}' to type {:?}", op, operand)
            }
            Self::InvalidBinaryOp { op, left, right } => format!(
                "Cannot apply operator '{}' between types {:?} and {:?}",
                op, left, right
            ),
            Self::InvalidCondition(ty) => {
                format!("Condition must be boolean, found {:?}", ty)
            }
            Self::BreakOutsideLoop => "Break statement can only be used inside a loop".to_string(),
            Self::ContinueOutsideLoop => "Continue statement can only be used inside a loop".to_string(),
            Self::MissingReturn(func) => {
                format!("Function '{}' must have a return statement", func)
            }
            Self::UnexpectedReturnValue => "Void function cannot return a value".to_string(),
            Self::MissingReturnValue => "Non-void function must return a value".to_string(),
            Self::CrossNodeAccess {
                table,
                table_node,
                current_node,
            } => format!(
                "Table '{}' belongs to node '{}' and cannot be accessed from node '{}'",
                table, table_node, current_node
            ),
            Self::InvalidPrimaryKey { table, column } => format!(
                "Column '{}' is not the primary key of table '{}'",
                column, table
            ),
            Self::AbortNotInFirstHop {
                function,
                hop_index,
            } => format!(
                "Abort statement in function '{}' can only be used in the first hop, not hop {}",
                function, hop_index
            ),
        }
    }
}

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
