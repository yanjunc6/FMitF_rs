use crate::ast::{TypeName, ReturnType, Span};

// Remove the Span struct and its impl from here
// Keep only error-related types:

#[derive(Debug, Clone)]
pub struct SpannedError {
    pub error: TransActError,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub enum TransActError {
    ParseError(String),
    TypeMismatch { expected: TypeName, found: TypeName },
    UndeclaredVariable(String),
    UndeclaredTable(String),
    UndeclaredField { table: String, field: String },
    UndeclaredNode(String),
    DuplicateFunction(String),
    DuplicateVariable(String),
    InvalidPrimaryKey { table: String, column: String },
    InvalidUnaryOp { op: String, operand: TypeName },
    InvalidBinaryOp { op: String, left: TypeName, right: TypeName },
    InvalidCondition(TypeName),
    ReturnTypeMismatch { expected: ReturnType, found: Option<TypeName> },
    ReturnInVoidFunction,
    MissingReturn(String),
    CrossNodeAccess { table: String, table_node: String, current_node: String },
}

pub type Results<T> = std::result::Result<T, Vec<SpannedError>>;

impl std::fmt::Display for TransActError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ParseError(msg) => write!(f, "Parse error: {}", msg),
            Self::TypeMismatch { expected, found } => 
                write!(f, "Type mismatch: expected {:?}, found {:?}", expected, found),
            Self::UndeclaredVariable(name) => write!(f, "Undeclared variable: {}", name),
            Self::UndeclaredTable(name) => write!(f, "Undeclared table: {}", name),
            Self::UndeclaredField { table, field } => 
                write!(f, "Field '{}' not found in table '{}'", field, table),
            Self::UndeclaredNode(name) => write!(f, "Undeclared node: {}", name),
            Self::DuplicateFunction(name) => write!(f, "Duplicate function: {}", name),
            Self::DuplicateVariable(name) => write!(f, "Duplicate variable: {}", name),
            Self::InvalidPrimaryKey { table, column } => 
                write!(f, "Invalid primary key '{}' for table '{}'", column, table),
            Self::InvalidUnaryOp { op, operand } => 
                write!(f, "Invalid unary operation '{}' on {:?}", op, operand),
            Self::InvalidBinaryOp { op, left, right } => 
                write!(f, "Invalid binary operation '{}' between {:?} and {:?}", op, left, right),
            Self::InvalidCondition(found) => 
                write!(f, "Invalid condition type: {:?} (expected bool)", found),
            Self::ReturnTypeMismatch { expected, found } => 
                write!(f, "Return type mismatch: expected {:?}, found {:?}", expected, found),
            Self::ReturnInVoidFunction => write!(f, "Return value in void function"),
            Self::MissingReturn(func) => write!(f, "Missing return statement in function '{}'", func),
            Self::CrossNodeAccess { table, table_node, current_node } => 
                write!(f, "Cannot access table '{}' (on '{}') from node '{}'", table, table_node, current_node),
        }
    }
}

impl std::error::Error for TransActError {}

pub fn format_errors(errors: &[SpannedError], source: &str) -> String {
    errors.iter()
        .enumerate()
        .map(|(i, spanned_err)| {
            let mut result = format!("Error {}: {}", i + 1, spanned_err.error);
            if let Some(span) = &spanned_err.span {
                result.push_str(&format!("\n  --> {}:{}", span.line, span.column));
                if let Some(line_text) = get_source_line(source, span.line) {
                    result.push_str(&format!("\n   | {}", line_text));
                    result.push_str(&format!("\n   | {}^", " ".repeat(span.column.saturating_sub(1))));
                }
            }
            result
        })
        .collect::<Vec<_>>()
        .join("\n\n")
}

fn get_source_line(source: &str, line_num: usize) -> Option<&str> {
    source.lines().nth(line_num.saturating_sub(1))
}