//! The `errors` module defines error types and utilities for handling errors in the AST.
//! It provides comprehensive error reporting with spans for precise localization of issues.
//!
//! # Overview
//!
//! - **AstError**: Represents various types of errors that can occur during parsing, name resolution,
//!   and semantic analysis.
//! - **SpannedError**: Combines an `AstError` with an optional `Span` for detailed error reporting.
//! - **Results**: A type alias for `Result` with a vector of `SpannedError`.
//!
//! # Features
//!
//! - Detailed error messages for debugging and user feedback.
//! - Support for complex error types such as cross-node access violations and type mismatches.
//!
//! # Usage
//!
//! Use the `format_errors` function to format a list of errors for display:
//!
//! ```rust
//! use crate::ast::errors::{format_errors, SpannedError};
//!
//! let errors: Vec<SpannedError> = vec![]; // Populate with errors
//! println!("{}", format_errors(&errors));
//! ```

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
    DuplicatePartition(String),
    DuplicateConstant(String),

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
    InvalidArrayAccess {
        array_type: TypeName,
        index_type: TypeName,
    },
    ArraySizeMismatch {
        expected: Option<usize>,
        found: Option<usize>,
    },

    // Assignment errors
    InvalidAssignmentTarget,
    ReadOnlyAssignment(String),

    // Partition errors
    UndeclaredPartition(String),
    InvalidPartitionArguments {
        partition: String,
        expected: usize,
        found: usize,
    },
    PartitionArgumentTypeMismatch {
        partition: String,
        param_index: usize,
        expected: TypeName,
        found: TypeName,
    },

    // Table validation errors
    EmptyPrimaryKey(String),
    InvalidPrimaryKeyField {
        table: String,
        field: String,
    },
    MissingTableFields(String),
    MissingTablePartition(String),
    DuplicateField {
        table: String,
        field: String,
    },
    UnresolvedTablePartition {
        table: String,
        partition: String,
    },
    InvalidTablePartitionArguments {
        table: String,
        partition: String,
        expected: usize,
        found: usize,
    },
    PartitionFieldNotInTable {
        table: String,
        partition: String,
        field: String,
    },

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

    // Constant expression errors
    NonConstantExpression {
        context: String,
    },
}

impl std::fmt::Display for AstError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.error_type(), self.message())
    }
}

impl std::error::Error for AstError {}

impl AstError {
    /// Get the error type name for display purposes.
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
            Self::DuplicatePartition(_) => "DuplicatePartition",
            Self::DuplicateConstant(_) => "DuplicateConstant",
            Self::TypeMismatch { .. } => "TypeMismatch",
            Self::InvalidUnaryOp { .. } => "InvalidUnaryOp",
            Self::InvalidBinaryOp { .. } => "InvalidBinaryOp",
            Self::InvalidCondition(_) => "InvalidCondition",
            Self::InvalidArrayAccess { .. } => "InvalidArrayAccess",
            Self::ArraySizeMismatch { .. } => "ArraySizeMismatch",
            Self::InvalidAssignmentTarget => "InvalidAssignmentTarget",
            Self::ReadOnlyAssignment(_) => "ReadOnlyAssignment",
            Self::UndeclaredPartition(_) => "UndeclaredPartition",
            Self::InvalidPartitionArguments { .. } => "InvalidPartitionArguments",
            Self::PartitionArgumentTypeMismatch { .. } => "PartitionArgumentTypeMismatch",
            Self::EmptyPrimaryKey(_) => "EmptyPrimaryKey",
            Self::InvalidPrimaryKeyField { .. } => "InvalidPrimaryKeyField",
            Self::MissingTableFields(_) => "MissingTableFields",
            Self::MissingTablePartition(_) => "MissingTablePartition",
            Self::DuplicateField { .. } => "DuplicateField",
            Self::UnresolvedTablePartition { .. } => "UnresolvedTablePartition",
            Self::InvalidTablePartitionArguments { .. } => "InvalidTablePartitionArguments",
            Self::PartitionFieldNotInTable { .. } => "PartitionFieldNotInTable",
            Self::BreakOutsideLoop => "BreakOutsideLoop",
            Self::ContinueOutsideLoop => "ContinueOutsideLoop",
            Self::MissingReturn(_) => "MissingReturn",
            Self::UnexpectedReturnValue => "UnexpectedReturnValue",
            Self::MissingReturnValue => "MissingReturnValue",
            Self::CrossNodeAccess { .. } => "CrossNodeAccess",
            Self::InvalidPrimaryKey { .. } => "InvalidPrimaryKey",
            Self::AbortNotInFirstHop { .. } => "AbortNotInFirstHop",
            Self::NonConstantExpression { .. } => "NonConstantExpression",
        }
    }

    /// Get the error message without the type prefix.
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
            Self::DuplicatePartition(name) => format!("Partition '{}' is already declared", name),
            Self::DuplicateConstant(name) => format!("Constant '{}' is already declared", name),
            Self::TypeMismatch { expected, found } => {
                format!("Expected type {:?} but found {:?}", expected, found)
            }
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
            Self::InvalidArrayAccess {
                array_type,
                index_type,
            } => format!(
                "Cannot index into {:?} with {:?}, expected integer index",
                array_type, index_type
            ),
            Self::ArraySizeMismatch { expected, found } => match (expected, found) {
                (Some(exp), Some(fnd)) => {
                    format!("Array size mismatch: expected {} but found {}", exp, fnd)
                }
                (Some(exp), None) => format!(
                    "Array size mismatch: expected {} but found variable size",
                    exp
                ),
                (None, Some(fnd)) => format!(
                    "Array size mismatch: expected variable size but found {}",
                    fnd
                ),
                (None, None) => "Array size mismatch".to_string(),
            },
            Self::InvalidAssignmentTarget => "Invalid assignment target".to_string(),
            Self::ReadOnlyAssignment(name) => {
                format!("Cannot assign to read-only variable '{}'", name)
            }
            Self::UndeclaredPartition(name) => format!("Partition '{}' is not declared", name),
            Self::InvalidPartitionArguments {
                partition,
                expected,
                found,
            } => format!(
                "Partition '{}' expects {} arguments but found {}",
                partition, expected, found
            ),
            Self::PartitionArgumentTypeMismatch {
                partition,
                param_index,
                expected,
                found,
            } => format!(
                "Partition '{}' parameter {} expects type {:?} but found {:?}",
                partition,
                param_index + 1,
                expected,
                found
            ),
            Self::EmptyPrimaryKey(table) => {
                format!("Table '{}' must have at least one primary key field", table)
            }
            Self::InvalidPrimaryKeyField { table, field } => format!(
                "Field '{}' in table '{}' is marked as primary key but doesn't exist in the table",
                field, table
            ),
            Self::MissingTableFields(table) => {
                format!("Table '{}' must have at least one field", table)
            }
            Self::MissingTablePartition(table) => {
                format!("Table '{}' must have a partition function", table)
            }
            Self::DuplicateField { table, field } => {
                format!(
                    "Field '{}' is declared multiple times in table '{}'",
                    field, table
                )
            }
            Self::UnresolvedTablePartition { table, partition } => format!(
                "Table '{}' references partition '{}' which is not declared",
                table, partition
            ),
            Self::InvalidTablePartitionArguments {
                table,
                partition,
                expected,
                found,
            } => format!(
                "Table '{}' uses partition '{}' with {} arguments but partition expects {}",
                table, partition, found, expected
            ),
            Self::PartitionFieldNotInTable {
                table,
                partition,
                field,
            } => format!(
                "Table '{}' uses partition '{}' with field '{}' which doesn't exist in the table",
                table, partition, field
            ),
            Self::BreakOutsideLoop => "Break statement can only be used inside a loop".to_string(),
            Self::ContinueOutsideLoop => {
                "Continue statement can only be used inside a loop".to_string()
            }
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
            Self::NonConstantExpression { context } => {
                format!("Expression must be constant for {}", context)
            }
        }
    }
}
