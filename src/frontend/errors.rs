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

    // Semantic errors
    TypeMismatch { expected: String, found: String },
    InvalidOperation { op: String, details: String },
    CircularDependency { names: Vec<String> },

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
            FrontEndErrorKind::TypeMismatch { expected, found } => {
                write!(f, "Type mismatch: expected {}, found {}", expected, found)
            }
            FrontEndErrorKind::InvalidOperation { op, details } => {
                write!(f, "Invalid operation {}: {}", op, details)
            }
            FrontEndErrorKind::CircularDependency { names } => {
                write!(f, "Circular dependency: {}", names.join(" -> "))
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
            _ => None,
        }
    }
}
