//! Error Handling for AST Module
//!
//! This module provides comprehensive error handling for all AST-related operations
//! including parsing, name resolution, type checking, semantic analysis, and 
//! constant evaluation.

use std::fmt;
use crate::util::{CompilerError, Severity, Span};

// ============================================================================
// --- Parse Errors
// ============================================================================

#[derive(Debug, Clone)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    SyntaxError { message: String },
    UnexpectedToken { expected: String, found: String },
    InvalidToken { token: String },
    UnexpectedEof,
    InvalidDecorator { decorator: String },
    ConflictingDecorators { decorators: Vec<String> },
}

impl ParseError {
    pub fn new(kind: ParseErrorKind) -> Self {
        Self { kind, span: None }
    }
    
    pub fn syntax_error(message: String) -> Self {
        Self::new(ParseErrorKind::SyntaxError { message })
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ParseErrorKind::SyntaxError { message } => write!(f, "Syntax error: {}", message),
            ParseErrorKind::UnexpectedToken { expected, found } => {
                write!(f, "Expected {}, found {}", expected, found)
            }
            ParseErrorKind::InvalidToken { token } => write!(f, "Invalid token: {}", token),
            ParseErrorKind::UnexpectedEof => write!(f, "Unexpected end of file"),
            ParseErrorKind::InvalidDecorator { decorator } => {
                write!(f, "Invalid decorator: @{}", decorator)
            }
            ParseErrorKind::ConflictingDecorators { decorators } => {
                write!(f, "Conflicting decorators: {}", decorators.join(", "))
            }
        }
    }
}

impl CompilerError for ParseError {
    fn code(&self) -> &'static str {
        match &self.kind {
            ParseErrorKind::SyntaxError { .. } => "E001",
            ParseErrorKind::UnexpectedToken { .. } => "E002", 
            ParseErrorKind::InvalidToken { .. } => "E003",
            ParseErrorKind::UnexpectedEof => "E004",
            ParseErrorKind::InvalidDecorator { .. } => "E005",
            ParseErrorKind::ConflictingDecorators { .. } => "E006",
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
            ParseErrorKind::InvalidDecorator { .. } => Some("Valid decorators: @intrinsic, @infix, @prefix, @postfix"),
            ParseErrorKind::ConflictingDecorators { .. } => Some("Use only one operator decorator per declaration"),
            _ => None,
        }
    }
}

// ============================================================================
// --- Semantic Errors
// ============================================================================

#[derive(Debug, Clone)]
pub struct SemanticError {
    pub kind: SemanticErrorKind,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub enum SemanticErrorKind {
    InvalidTableDefinition { table_name: String, reason: String },
    InvalidPartitionFunction { function_name: String, reason: String },
    UnsupportedLanguageFeature { feature: String },
    InvalidTransaction { transaction_name: String, reason: String },
}

impl SemanticError {
    pub fn new(kind: SemanticErrorKind) -> Self {
        Self { kind, span: None }
    }
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            SemanticErrorKind::InvalidTableDefinition { table_name, reason } => {
                write!(f, "Invalid table definition '{}': {}", table_name, reason)
            }
            SemanticErrorKind::InvalidPartitionFunction { function_name, reason } => {
                write!(f, "Invalid partition function '{}': {}", function_name, reason)
            }
            SemanticErrorKind::UnsupportedLanguageFeature { feature } => {
                write!(f, "Unsupported language feature: {}", feature)
            }
            SemanticErrorKind::InvalidTransaction { transaction_name, reason } => {
                write!(f, "Invalid transaction '{}': {}", transaction_name, reason)
            }
        }
    }
}

impl CompilerError for SemanticError {
    fn code(&self) -> &'static str {
        match &self.kind {
            SemanticErrorKind::InvalidTableDefinition { .. } => "S001",
            SemanticErrorKind::InvalidPartitionFunction { .. } => "S002",
            SemanticErrorKind::UnsupportedLanguageFeature { .. } => "S003",
            SemanticErrorKind::InvalidTransaction { .. } => "S004",
        }
    }

    fn span(&self) -> Option<Span> {
        self.span
    }
    
    fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }
}

// ============================================================================
// --- Name Resolution Errors
// ============================================================================

#[derive(Debug, Clone)]
pub struct NameResolutionError {
    pub kind: NameResolutionErrorKind,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub enum NameResolutionErrorKind {
    UndefinedIdentifier { name: String },
    DuplicateDefinition { name: String },
    CircularDependency { names: Vec<String> },
}

impl NameResolutionError {
    pub fn new(kind: NameResolutionErrorKind) -> Self {
        Self { kind, span: None }
    }
}

impl fmt::Display for NameResolutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            NameResolutionErrorKind::UndefinedIdentifier { name } => {
                write!(f, "Undefined identifier: {}", name)
            }
            NameResolutionErrorKind::DuplicateDefinition { name } => {
                write!(f, "Duplicate definition: {}", name)
            }
            NameResolutionErrorKind::CircularDependency { names } => {
                write!(f, "Circular dependency: {}", names.join(" -> "))
            }
        }
    }
}

impl CompilerError for NameResolutionError {
    fn code(&self) -> &'static str {
        match &self.kind {
            NameResolutionErrorKind::UndefinedIdentifier { .. } => "N001",
            NameResolutionErrorKind::DuplicateDefinition { .. } => "N002",
            NameResolutionErrorKind::CircularDependency { .. } => "N003",
        }
    }

    fn span(&self) -> Option<Span> {
        self.span
    }
    
    fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }
}

// ============================================================================
// --- Type Check Errors
// ============================================================================

#[derive(Debug, Clone)]
pub struct TypeCheckError {
    pub kind: TypeCheckErrorKind,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub enum TypeCheckErrorKind {
    TypeMismatch { expected: String, found: String },
    InvalidOperation { op: String, types: Vec<String> },
    UnknownType { type_name: String },
}

impl TypeCheckError {
    pub fn new(kind: TypeCheckErrorKind) -> Self {
        Self { kind, span: None }
    }
}

impl fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            TypeCheckErrorKind::TypeMismatch { expected, found } => {
                write!(f, "Type mismatch: expected {}, found {}", expected, found)
            }
            TypeCheckErrorKind::InvalidOperation { op, types } => {
                write!(f, "Invalid operation {} for types: {}", op, types.join(", "))
            }
            TypeCheckErrorKind::UnknownType { type_name } => {
                write!(f, "Unknown type: {}", type_name)
            }
        }
    }
}

impl CompilerError for TypeCheckError {
    fn code(&self) -> &'static str {
        match &self.kind {
            TypeCheckErrorKind::TypeMismatch { .. } => "T001",
            TypeCheckErrorKind::InvalidOperation { .. } => "T002",
            TypeCheckErrorKind::UnknownType { .. } => "T003",
        }
    }

    fn span(&self) -> Option<Span> {
        self.span
    }
    
    fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }
}

// ============================================================================
// --- Constant Errors
// ============================================================================

#[derive(Debug, Clone)]
pub struct ConstantError {
    pub kind: ConstantErrorKind,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub enum ConstantErrorKind {
    NotConstant { expression: String },
    IntegerOverflow { value: String },
    DivisionByZero,
    InvalidTypeOperation { op: String, type_name: String },
}

impl ConstantError {
    pub fn new(kind: ConstantErrorKind) -> Self {
        Self { kind, span: None }
    }
}

impl fmt::Display for ConstantError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ConstantErrorKind::NotConstant { expression } => {
                write!(f, "Expression is not constant: {}", expression)
            }
            ConstantErrorKind::IntegerOverflow { value } => {
                write!(f, "Integer overflow: {}", value)
            }
            ConstantErrorKind::DivisionByZero => {
                write!(f, "Division by zero")
            }
            ConstantErrorKind::InvalidTypeOperation { op, type_name } => {
                write!(f, "Invalid operation {} for constant type {}", op, type_name)
            }
        }
    }
}

impl CompilerError for ConstantError {
    fn code(&self) -> &'static str {
        match &self.kind {
            ConstantErrorKind::NotConstant { .. } => "C001",
            ConstantErrorKind::IntegerOverflow { .. } => "C002",
            ConstantErrorKind::DivisionByZero => "C003",
            ConstantErrorKind::InvalidTypeOperation { .. } => "C004",
        }
    }

    fn span(&self) -> Option<Span> {
        self.span
    }
    
    fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    fn severity(&self) -> Severity {
        match &self.kind {
            ConstantErrorKind::IntegerOverflow { .. } => Severity::Warning,
            _ => Severity::Error,
        }
    }
}
