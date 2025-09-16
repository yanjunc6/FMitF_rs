//! Error Handling for AST Module
//!
//! Unified error handling for all AST-related operations.

use crate::util::CompilerErrorKind;
use std::fmt;

// ============================================================================
// --- Unified AST Error
// ============================================================================

#[derive(Debug, Clone)]
pub enum FrontEndErrorKind {
    // Parse errors
    SyntaxError { message: String },
    UnexpectedToken { expected: String, found: String },
    UnexpectedEof,
    ParseError(String),     // For pest parsing errors
    UnexpectedRule(String), // For unexpected grammar rules
    MissingField(String),   // For missing required fields

    // Name resolution errors
    UndefinedIdentifier { name: String },
    DuplicateDefinition { name: String },
    InvalidScope { name: String, details: String },
    ReturnOutsideFunction,
    UnresolvedReference { name: String, category: String },

    // Semantic errors
    TypeMismatch { expected: String, found: String },
    InvalidOperation { op: String, details: String },
    CircularDependency { names: Vec<String> },
    InvalidTransactionStatement { statement_type: String },
    MultipleTableNodes { table_name: String },
    InvalidTableGenericArgument { found_type: String },
    HopsForNonConstant { context: String },
    HopsForNonInteger { context: String, found_type: String },
    GlobalFunctionInNonGlobalHop { function_name: String },

    // General errors
    InvalidInput { message: String },
}

impl fmt::Display for FrontEndErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            FrontEndErrorKind::SyntaxError { message } => write!(f, "Syntax error: {}", message),
            FrontEndErrorKind::UnexpectedToken { expected, found } => {
                write!(f, "Expected {}, found {}", expected, found)
            }
            FrontEndErrorKind::UnexpectedEof => write!(f, "Unexpected end of file"),
            FrontEndErrorKind::ParseError(msg) => write!(f, "Parse error: {}", msg),
            FrontEndErrorKind::UnexpectedRule(rule) => write!(f, "Unexpected rule: {}", rule),
            FrontEndErrorKind::MissingField(field) => write!(f, "Missing field: {}", field),
            FrontEndErrorKind::UndefinedIdentifier { name } => {
                write!(f, "Undefined identifier: {}", name)
            }
            FrontEndErrorKind::DuplicateDefinition { name } => {
                write!(f, "Duplicate definition: {}", name)
            }
            FrontEndErrorKind::InvalidScope { name, details } => {
                write!(f, "Invalid scope for '{}': {}", name, details)
            }
            FrontEndErrorKind::ReturnOutsideFunction => {
                write!(f, "Return statement outside function")
            }
            FrontEndErrorKind::UnresolvedReference { name, category } => {
                write!(f, "Unresolved {} reference: {}", category, name)
            }
            FrontEndErrorKind::TypeMismatch { expected, found } => {
                write!(f, "Type mismatch: expected {}, found {}", expected, found)
            }
            FrontEndErrorKind::InvalidOperation { op, details } => {
                write!(f, "Invalid operation {}: {}", op, details)
            }
            FrontEndErrorKind::CircularDependency { names } => {
                write!(f, "Circular dependency: {}", names.join(" -> "))
            }
            FrontEndErrorKind::InvalidTransactionStatement { statement_type } => {
                write!(f, "Invalid top-level statement in transaction: {}. Only 'hop' and 'hops_for' are allowed.", statement_type)
            }
            FrontEndErrorKind::MultipleTableNodes { table_name } => {
                write!(
                    f,
                    "Table '{}' has multiple 'node' declarations. Only one is allowed.",
                    table_name
                )
            }
            FrontEndErrorKind::InvalidTableGenericArgument { found_type } => {
                write!(f, "Invalid generic argument for Table. Expected a table name, but found type '{}'.", found_type)
            }
            FrontEndErrorKind::HopsForNonConstant { context } => {
                write!(f, "'hops_for' {} must be a compile-time constant.", context)
            }
            FrontEndErrorKind::HopsForNonInteger {
                context,
                found_type,
            } => {
                write!(
                    f,
                    "'hops_for' {} must be an integer, but found type '{}'.",
                    context, found_type
                )
            }
            FrontEndErrorKind::GlobalFunctionInNonGlobalHop { function_name } => {
                write!(
                    f,
                    "Function '{}' is marked as @global and can only be called within @global hop blocks.",
                    function_name
                )
            }
            FrontEndErrorKind::InvalidInput { message } => {
                write!(f, "Invalid input: {}", message)
            }
        }
    }
}

impl CompilerErrorKind for FrontEndErrorKind {
    fn code(&self) -> &'static str {
        match &self {
            FrontEndErrorKind::SyntaxError { .. } => "A001",
            FrontEndErrorKind::UnexpectedToken { .. } => "A002",
            FrontEndErrorKind::UnexpectedEof => "A003",
            FrontEndErrorKind::ParseError(..) => "A004",
            FrontEndErrorKind::UnexpectedRule(..) => "A005",
            FrontEndErrorKind::MissingField(..) => "A006",
            FrontEndErrorKind::UndefinedIdentifier { .. } => "A007",
            FrontEndErrorKind::DuplicateDefinition { .. } => "A008",
            FrontEndErrorKind::InvalidScope { .. } => "A009",
            FrontEndErrorKind::ReturnOutsideFunction => "A010",
            FrontEndErrorKind::TypeMismatch { .. } => "A011",
            FrontEndErrorKind::InvalidOperation { .. } => "A012",
            FrontEndErrorKind::CircularDependency { .. } => "A013",
            FrontEndErrorKind::InvalidInput { .. } => "A014",
            FrontEndErrorKind::UnresolvedReference { .. } => "A015",
            FrontEndErrorKind::InvalidTransactionStatement { .. } => "A016",
            FrontEndErrorKind::MultipleTableNodes { .. } => "A017",
            FrontEndErrorKind::InvalidTableGenericArgument { .. } => "A018",
            FrontEndErrorKind::HopsForNonConstant { .. } => "A019",
            FrontEndErrorKind::HopsForNonInteger { .. } => "A020",
            FrontEndErrorKind::GlobalFunctionInNonGlobalHop { .. } => "A021",
        }
    }

    fn help(&self) -> Option<&str> {
        match &self {
            FrontEndErrorKind::UndefinedIdentifier { .. } => {
                Some("Check if the identifier is defined in the current scope")
            }
            FrontEndErrorKind::DuplicateDefinition { .. } => {
                Some("Use a different name or remove one of the definitions")
            }
            FrontEndErrorKind::InvalidScope { .. } => {
                Some("Check the scoping rules for this identifier")
            }
            FrontEndErrorKind::ReturnOutsideFunction => {
                Some("Return statements can only be used inside functions")
            }
            FrontEndErrorKind::CircularDependency { .. } => {
                Some("Break the circular dependency by restructuring your code")
            }
            FrontEndErrorKind::UnresolvedReference { .. } => {
                Some("This is likely an internal compiler error, as name resolution should have caught this.")
            }
            FrontEndErrorKind::InvalidTransactionStatement { .. } => {
                Some("Wrap your logic inside a 'hop' block if it needs to execute within a single partition.")
            }
            FrontEndErrorKind::MultipleTableNodes { .. } => {
                Some("A table can only be partitioned by one node. Consolidate your node declarations.")
            }
            FrontEndErrorKind::InvalidTableGenericArgument { .. } => {
                Some("The generic argument for a Table type, like in `Table<MyTable>`, must itself be a table.")
            }
            FrontEndErrorKind::HopsForNonConstant { .. } => {
                Some("The expression in a 'hops_for' loop is evaluated at compile-time to determine the loop bounds.")
            }
            FrontEndErrorKind::HopsForNonInteger { .. } => {
                Some("The bounds of a 'hops_for' loop must be an integer to define the iteration range.")
            }
            FrontEndErrorKind::GlobalFunctionInNonGlobalHop { .. } => {
                Some("Use @global decorator on the hop block to call global functions, or call a non-global function instead.")
            }
            _ => None,
        }
    }
}
