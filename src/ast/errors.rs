//! Error Handling for AST Module
//!
//! Unified error handling for all AST-related operations.

use std::fmt;
use crate::util::{CompilerError, Span};

// ============================================================================
// --- Unified AST Error
// ============================================================================

#[derive(Debug, Clone)]
pub struct AstError {
    pub kind: AstErrorKind,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub enum AstErrorKind {
    // Parse errors
    SyntaxError { message: String },
    UnexpectedToken { expected: String, found: String },
    UnexpectedEof,
    
    // Semantic errors  
    UndefinedIdentifier { name: String },
    DuplicateDefinition { name: String },
    TypeMismatch { expected: String, found: String },
    InvalidOperation { op: String, details: String },
    CircularDependency { names: Vec<String> },
    
    // General errors
    InvalidInput { message: String },
}

impl AstError {
    pub fn new(kind: AstErrorKind) -> Self {
        Self { kind, span: None }
    }
    
    pub fn syntax_error(message: String) -> Self {
        Self::new(AstErrorKind::SyntaxError { message })
    }
    
    pub fn undefined_identifier(name: String) -> Self {
        Self::new(AstErrorKind::UndefinedIdentifier { name })
    }
    
    pub fn type_mismatch(expected: String, found: String) -> Self {
        Self::new(AstErrorKind::TypeMismatch { expected, found })
    }
}

impl fmt::Display for AstError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            AstErrorKind::SyntaxError { message } => write!(f, "Syntax error: {}", message),
            AstErrorKind::UnexpectedToken { expected, found } => {
                write!(f, "Expected {}, found {}", expected, found)
            }
            AstErrorKind::UnexpectedEof => write!(f, "Unexpected end of file"),
            AstErrorKind::UndefinedIdentifier { name } => {
                write!(f, "Undefined identifier: {}", name)
            }
            AstErrorKind::DuplicateDefinition { name } => {
                write!(f, "Duplicate definition: {}", name)
            }
            AstErrorKind::TypeMismatch { expected, found } => {
                write!(f, "Type mismatch: expected {}, found {}", expected, found)
            }
            AstErrorKind::InvalidOperation { op, details } => {
                write!(f, "Invalid operation {}: {}", op, details)
            }
            AstErrorKind::CircularDependency { names } => {
                write!(f, "Circular dependency: {}", names.join(" -> "))
            }
            AstErrorKind::InvalidInput { message } => {
                write!(f, "Invalid input: {}", message)
            }
        }
    }
}

impl CompilerError for AstError {
    fn code(&self) -> &'static str {
        match &self.kind {
            AstErrorKind::SyntaxError { .. } => "A001",
            AstErrorKind::UnexpectedToken { .. } => "A002",
            AstErrorKind::UnexpectedEof => "A003",
            AstErrorKind::UndefinedIdentifier { .. } => "A004",
            AstErrorKind::DuplicateDefinition { .. } => "A005",
            AstErrorKind::TypeMismatch { .. } => "A006",
            AstErrorKind::InvalidOperation { .. } => "A007",
            AstErrorKind::CircularDependency { .. } => "A008",
            AstErrorKind::InvalidInput { .. } => "A009",
        }
    }

    fn span(&self) -> Option<Span> {
        self.span
    }
    
    fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    fn help(&self) -> Option<&str> {
        match &self.kind {
            AstErrorKind::UndefinedIdentifier { .. } => Some("Check if the identifier is defined in the current scope"),
            AstErrorKind::DuplicateDefinition { .. } => Some("Use a different name or remove one of the definitions"),
            AstErrorKind::CircularDependency { .. } => Some("Break the circular dependency by restructuring your code"),
            _ => None,
        }
    }
}
