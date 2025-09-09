//! Error Handling for AST Module
//!
//! This module provides comprehensive error handling for all AST-related operations
//! including parsing, name resolution, type checking, semantic analysis, and
//! constant evaluation.

use std::fmt;

use crate::ast::Span;
use crate::util::DiagnosticError;

// ============================================================================
// --- Core Error Types
// ============================================================================

/// Main error type for all AST operations
#[derive(Debug, Clone)]
pub enum AstError {
    Parse(ParseError),
    NameResolution(NameResolutionError),
    TypeChecking(TypeCheckError),
    Semantic(SemanticError),
    ConstantEvaluation(ConstantError),
}

impl fmt::Display for AstError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AstError::Parse(e) => write!(f, "Parse error: {}", e),
            AstError::NameResolution(e) => write!(f, "Name resolution error: {}", e),
            AstError::TypeChecking(e) => write!(f, "Type checking error: {}", e),
            AstError::Semantic(e) => write!(f, "Semantic error: {}", e),
            AstError::ConstantEvaluation(e) => write!(f, "Constant evaluation error: {}", e),
        }
    }
}

impl std::error::Error for AstError {}

impl AstError {
    /// Get a string representing the error type
    pub fn error_type(&self) -> &'static str {
        match self {
            AstError::Parse(_) => "Parse",
            AstError::NameResolution(_) => "NameResolution",
            AstError::TypeChecking(_) => "TypeChecking",
            AstError::Semantic(_) => "Semantic",
            AstError::ConstantEvaluation(_) => "ConstantEvaluation",
        }
    }

    /// Get a string message for the error
    pub fn message(&self) -> String {
        self.to_string()
    }
}

impl DiagnosticError for AstError {
    fn error_type(&self) -> &'static str {
        self.error_type()
    }

    fn message(&self) -> String {
        self.message()
    }

    fn help(&self) -> Option<String> {
        match self {
            AstError::Parse(ParseError::UnexpectedToken { expected, .. }) => {
                Some(format!("Expected: {}", expected))
            }
            AstError::NameResolution(NameResolutionError::UndefinedIdentifier { name, .. }) => {
                Some(format!("Make sure '{}' is declared before use", name))
            }
            AstError::TypeChecking(TypeCheckError::TypeMismatch { expected, .. }) => {
                Some(format!("Expected type: {}", expected))
            }
            _ => None,
        }
    }

    fn note(&self) -> Option<String> {
        match self {
            AstError::Parse(_) => Some("Check syntax against language specification".to_string()),
            AstError::NameResolution(_) => {
                Some("Variables and functions must be declared before use".to_string())
            }
            AstError::TypeChecking(_) => {
                Some("All expressions must have compatible types".to_string())
            }
            AstError::Semantic(_) => Some("Code must follow semantic rules".to_string()),
            AstError::ConstantEvaluation(_) => {
                Some("Constants must evaluate to literal values".to_string())
            }
        }
    }
}

// ============================================================================
// --- Spanned Error
// ============================================================================

/// Error with source location information
#[derive(Debug, Clone)]
pub struct SpannedError {
    pub error: AstError,
    pub span: Option<Span>,
}

impl SpannedError {
    pub fn new(error: AstError, span: Option<Span>) -> Self {
        Self { error, span }
    }
}

impl fmt::Display for SpannedError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(span) = self.span {
            write!(
                f,
                "{} at line {}, column {}",
                self.error, span.line, span.column
            )
        } else {
            write!(f, "{}", self.error)
        }
    }
}

// ============================================================================
// --- Parse Errors
// ============================================================================

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedToken { expected: String, found: String },
    InvalidSyntax { message: String },
    PestError { message: String },
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken { expected, found } => {
                write!(f, "Expected {}, found {}", expected, found)
            }
            ParseError::InvalidSyntax { message } => {
                write!(f, "Invalid syntax: {}", message)
            }
            ParseError::PestError { message } => {
                write!(f, "Parse error: {}", message)
            }
        }
    }
}

// ============================================================================
// --- Name Resolution Errors
// ============================================================================

#[derive(Debug, Clone)]
pub enum NameResolutionError {
    UndefinedIdentifier {
        name: String,
    },
    DuplicateDefinition {
        name: String,
        first_definition: Option<Span>,
    },
    CircularDependency {
        cycle: Vec<String>,
    },
    InvalidReference {
        name: String,
        context: String,
    },
}

impl fmt::Display for NameResolutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NameResolutionError::UndefinedIdentifier { name } => {
                write!(f, "Undefined identifier '{}'", name)
            }
            NameResolutionError::DuplicateDefinition { name, .. } => {
                write!(f, "Duplicate definition of '{}'", name)
            }
            NameResolutionError::CircularDependency { cycle } => {
                write!(f, "Circular dependency: {}", cycle.join(" -> "))
            }
            NameResolutionError::InvalidReference { name, context } => {
                write!(f, "Invalid reference to '{}' in {}", name, context)
            }
        }
    }
}

// ============================================================================
// --- Type Checking Errors
// ============================================================================

#[derive(Debug, Clone)]
pub enum TypeCheckError {
    TypeMismatch {
        expected: String,
        found: String,
    },
    UndefinedType {
        name: String,
    },
    InvalidTypeArguments {
        type_name: String,
        expected: usize,
        found: usize,
    },
    IncompatibleTypes {
        left: String,
        right: String,
        operation: String,
    },
    InvalidFunctionCall {
        function_name: String,
        reason: String,
    },
    InvalidAssignment {
        target: String,
        value_type: String,
    },
    InvalidOperation {
        operation: String,
        operand_types: Vec<String>,
    },
    InvalidReference {
        name: String,
        context: String,
    },
    InvalidTableAccess {
        table_name: String,
        reason: String,
    },
}

// Type alias for backwards compatibility
pub type TypeResolutionError = TypeCheckError;

impl fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeCheckError::TypeMismatch { expected, found } => {
                write!(f, "Type mismatch: expected {}, found {}", expected, found)
            }
            TypeCheckError::UndefinedType { name } => {
                write!(f, "Undefined type '{}'", name)
            }
            TypeCheckError::InvalidTypeArguments {
                type_name,
                expected,
                found,
            } => {
                write!(
                    f,
                    "Type '{}' expects {} arguments, found {}",
                    type_name, expected, found
                )
            }
            TypeCheckError::IncompatibleTypes {
                left,
                right,
                operation,
            } => {
                write!(
                    f,
                    "Incompatible types for {}: {} and {}",
                    operation, left, right
                )
            }
            TypeCheckError::InvalidFunctionCall {
                function_name,
                reason,
            } => {
                write!(f, "Invalid function call '{}': {}", function_name, reason)
            }
            TypeCheckError::InvalidAssignment { target, value_type } => {
                write!(
                    f,
                    "Invalid assignment to '{}' with type '{}'",
                    target, value_type
                )
            }
            TypeCheckError::InvalidOperation {
                operation,
                operand_types,
            } => {
                write!(
                    f,
                    "Invalid operation '{}' with operand types: {}",
                    operation,
                    operand_types.join(", ")
                )
            }
            TypeCheckError::InvalidReference { name, context } => {
                write!(f, "Invalid reference to '{}' in {}", name, context)
            }
            TypeCheckError::InvalidTableAccess { table_name, reason } => {
                write!(f, "Invalid table access '{}': {}", table_name, reason)
            }
        }
    }
}

// ============================================================================
// --- Semantic Errors
// ============================================================================

#[derive(Debug, Clone)]
pub enum SemanticError {
    InvalidTableDefinition {
        table_name: String,
        reason: String,
    },
    InvalidPartitionFunction {
        function_name: String,
        reason: String,
    },
    InvalidTransactionStructure {
        transaction_name: String,
        reason: String,
    },
    InvariantViolation {
        invariant: String,
        context: String,
    },
    InvalidDecorator {
        decorator: String,
        context: String,
    },
    ConflictingDecorators {
        decorators: Vec<String>,
    },
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SemanticError::InvalidTableDefinition { table_name, reason } => {
                write!(f, "Invalid table definition '{}': {}", table_name, reason)
            }
            SemanticError::InvalidPartitionFunction {
                function_name,
                reason,
            } => {
                write!(
                    f,
                    "Invalid partition function '{}': {}",
                    function_name, reason
                )
            }
            SemanticError::InvalidTransactionStructure {
                transaction_name,
                reason,
            } => {
                write!(
                    f,
                    "Invalid transaction structure '{}': {}",
                    transaction_name, reason
                )
            }
            SemanticError::InvariantViolation { invariant, context } => {
                write!(f, "Invariant violation '{}' in {}", invariant, context)
            }
            SemanticError::InvalidDecorator { decorator, context } => {
                write!(f, "Invalid decorator '{}' in {}", decorator, context)
            }
            SemanticError::ConflictingDecorators { decorators } => {
                write!(f, "Conflicting decorators: {}", decorators.join(", "))
            }
        }
    }
}

// ============================================================================
// --- Constant Evaluation Errors
// ============================================================================

#[derive(Debug, Clone)]
pub enum ConstantError {
    NonConstantExpression {
        expression: String,
    },
    UndefinedConstant {
        name: String,
    },
    CircularConstantDependency {
        cycle: Vec<String>,
    },
    IntegerOverflow {
        operation: String,
        value: String,
    },
    DivisionByZero,
    InvalidTypeOperation {
        operation: String,
        operand_type: String,
    },
}

impl fmt::Display for ConstantError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConstantError::NonConstantExpression { expression } => {
                write!(f, "Non-constant expression: {}", expression)
            }
            ConstantError::UndefinedConstant { name } => {
                write!(f, "Undefined constant '{}'", name)
            }
            ConstantError::CircularConstantDependency { cycle } => {
                write!(f, "Circular constant dependency: {}", cycle.join(" -> "))
            }
            ConstantError::IntegerOverflow { operation, value } => {
                write!(f, "Integer overflow in {}: {}", operation, value)
            }
            ConstantError::DivisionByZero => {
                write!(f, "Division by zero")
            }
            ConstantError::InvalidTypeOperation {
                operation,
                operand_type,
            } => {
                write!(
                    f,
                    "Invalid operation '{}' for type '{}'",
                    operation, operand_type
                )
            }
        }
    }
}

// ============================================================================
// --- Error Collection and Results
// ============================================================================

/// Collects multiple errors during analysis
#[derive(Debug, Clone)]
pub struct ErrorCollector {
    pub errors: Vec<SpannedError>,
}

impl ErrorCollector {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    pub fn add_error(&mut self, error: AstError, span: Option<Span>) {
        self.errors.push(SpannedError::new(error, span));
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn into_result<T>(self, value: Option<T>) -> Results<T> {
        if self.has_errors() {
            Results::failure(self.errors)
        } else if let Some(v) = value {
            Results::success(v)
        } else {
            Results::failure(vec![SpannedError::new(
                AstError::Parse(ParseError::InvalidSyntax {
                    message: "No value produced".to_string(),
                }),
                None,
            )])
        }
    }
}

impl Default for ErrorCollector {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// --- Results Type
// ============================================================================

/// Result type that can contain multiple errors
#[derive(Debug, Clone)]
pub enum Results<T> {
    Success(T),
    Failure(Vec<SpannedError>),
}

impl<T> Results<T> {
    pub fn success(value: T) -> Self {
        Results::Success(value)
    }

    pub fn failure(errors: Vec<SpannedError>) -> Self {
        Results::Failure(errors)
    }

    pub fn is_success(&self) -> bool {
        matches!(self, Results::Success(_))
    }

    pub fn is_failure(&self) -> bool {
        matches!(self, Results::Failure(_))
    }

    pub fn unwrap(self) -> T {
        match self {
            Results::Success(value) => value,
            Results::Failure(errors) => {
                panic!("Called unwrap on failure result with errors: {:?}", errors)
            }
        }
    }

    pub fn unwrap_or_else<F>(self, f: F) -> T
    where
        F: FnOnce(Vec<SpannedError>) -> T,
    {
        match self {
            Results::Success(value) => value,
            Results::Failure(errors) => f(errors),
        }
    }

    pub fn map<U, F>(self, f: F) -> Results<U>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Results::Success(value) => Results::Success(f(value)),
            Results::Failure(errors) => Results::Failure(errors),
        }
    }

    pub fn and_then<U, F>(self, f: F) -> Results<U>
    where
        F: FnOnce(T) -> Results<U>,
    {
        match self {
            Results::Success(value) => f(value),
            Results::Failure(errors) => Results::Failure(errors),
        }
    }
}

impl<T> fmt::Display for Results<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Results::Success(value) => write!(f, "Success: {}", value),
            Results::Failure(errors) => {
                write!(f, "Failure with {} errors:", errors.len())?;
                for error in errors {
                    write!(f, "\n  {}", error)?;
                }
                Ok(())
            }
        }
    }
}
