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
    SyntaxError {
        message: String,
    },
    UnexpectedToken {
        expected: String,
        found: String,
    },
    UnexpectedEof,
    ParseError(String),     // For pest parsing errors
    UnexpectedRule(String), // For unexpected grammar rules
    MissingField(String),   // For missing required fields

    // Name resolution errors
    UndefinedIdentifier {
        name: String,
    },
    DuplicateDefinition {
        name: String,
    },
    InvalidScope {
        name: String,
        details: String,
    },
    ReturnOutsideFunction,
    UnresolvedReference {
        name: String,
        category: String,
    },

    // Semantic errors
    TypeMismatch {
        expected: String,
        found: String,
        context: String,
    },
    CannotInferType {
        item: String,
    },
    ArgumentCountMismatch {
        expected: usize,
        found: usize,
    },
    InvalidOperation {
        op: String,
        details: String,
    },
    CircularDependency {
        names: Vec<String>,
    },
    InvalidTransactionStatement {
        statement_type: String,
    },
    MultipleTableNodes {
        table_name: String,
    },
    InvalidTableGenericArgument {
        found_type: String,
    },
    HopsForNonConstant {
        context: String,
    },
    HopsForNonInteger {
        context: String,
        found_type: String,
    },
    GlobalFunctionInNonGlobalHop {
        function_name: String,
    },
    MemberNotFound {
        member_name: String,
        type_name: String,
    },
    NotAFunction {
        name: String,
    },
    HopInFunction {
        function_name: String,
    },
    TransactionWithGenerics {
        transaction_name: String,
    },
    PartitionMustReturnInt {
        function_name: String,
        found_type: String,
    },
    InferTypeFound {
        context: String,
    },
    LambdaCaptureNotSupported {
        variable_name: String,
    },
    LambdaParameterTypeInference {
        parameter_name: String,
    },

    // General errors
    InvalidInput {
        message: String,
    },
    NotYetImplemented(String),
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
            FrontEndErrorKind::TypeMismatch {
                expected,
                found,
                context,
            } => {
                write!(
                    f,
                    "Type mismatch in {}: expected `{}`, found `{}`",
                    context, expected, found
                )
            }
            FrontEndErrorKind::CannotInferType { item } => {
                write!(f, "Cannot infer type for '{}'", item)
            }
            FrontEndErrorKind::ArgumentCountMismatch { expected, found } => {
                write!(
                    f,
                    "Argument count mismatch: expected {}, found {}",
                    expected, found
                )
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
            FrontEndErrorKind::MemberNotFound {
                member_name,
                type_name,
            } => write!(
                f,
                "Member '{}' not found on type '{}'",
                member_name, type_name
            ),
            FrontEndErrorKind::NotAFunction { name } => {
                write!(f, "'{}' is not a function and cannot be called", name)
            }
            FrontEndErrorKind::HopInFunction { function_name } => {
                write!(
                    f,
                    "Hop blocks are not allowed in function '{}'. Use hop blocks only in transactions.",
                    function_name
                )
            }
            FrontEndErrorKind::TransactionWithGenerics { transaction_name } => {
                write!(
                    f,
                    "Transaction '{}' cannot have generic parameters. Transactions must be concrete.",
                    transaction_name
                )
            }
            FrontEndErrorKind::PartitionMustReturnInt {
                function_name,
                found_type,
            } => {
                write!(
                    f,
                    "Partition function '{}' must return int, but found type '{}'.",
                    function_name, found_type
                )
            }
            FrontEndErrorKind::InferTypeFound { context } => {
                write!(
                    f,
                    "Inference type found in {}: all types must be fully resolved",
                    context
                )
            }
            FrontEndErrorKind::LambdaCaptureNotSupported { variable_name } => {
                write!(
                    f,
                    "Lambda closure captures are not supported. Variable '{}' cannot be accessed from within the lambda.",
                    variable_name
                )
            }
            FrontEndErrorKind::LambdaParameterTypeInference { parameter_name } => {
                write!(
                    f,
                    "Lambda parameter '{}' type cannot be inferred. Please provide explicit type annotation.",
                    parameter_name
                )
            }
            FrontEndErrorKind::InvalidInput { message } => {
                write!(f, "Invalid input: {}", message)
            }
            FrontEndErrorKind::NotYetImplemented(feature) => {
                write!(f, "Feature not yet implemented: {}", feature)
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
            FrontEndErrorKind::ArgumentCountMismatch { .. } => "A012",
            FrontEndErrorKind::InvalidOperation { .. } => "A013",
            FrontEndErrorKind::CircularDependency { .. } => "A014",
            FrontEndErrorKind::InvalidInput { .. } => "A015",
            FrontEndErrorKind::UnresolvedReference { .. } => "A016",
            FrontEndErrorKind::CannotInferType { .. } => "A018",
            FrontEndErrorKind::InvalidTransactionStatement { .. } => "A019",
            FrontEndErrorKind::MultipleTableNodes { .. } => "A020",
            FrontEndErrorKind::InvalidTableGenericArgument { .. } => "A021",
            FrontEndErrorKind::HopsForNonConstant { .. } => "A022",
            FrontEndErrorKind::HopsForNonInteger { .. } => "A023",
            FrontEndErrorKind::GlobalFunctionInNonGlobalHop { .. } => "A024",
            FrontEndErrorKind::MemberNotFound { .. } => "A025",
            FrontEndErrorKind::NotAFunction { .. } => "A026",
            FrontEndErrorKind::HopInFunction { .. } => "A027",
            FrontEndErrorKind::TransactionWithGenerics { .. } => "A028",
            FrontEndErrorKind::PartitionMustReturnInt { .. } => "A029",
            FrontEndErrorKind::InferTypeFound { .. } => "A030",
            FrontEndErrorKind::LambdaCaptureNotSupported { .. } => "A031",
            FrontEndErrorKind::LambdaParameterTypeInference { .. } => "A032",
            FrontEndErrorKind::NotYetImplemented(..) => "A999",
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
            FrontEndErrorKind::CannotInferType { .. } => {
                Some("Try adding an explicit type annotation, for example: `let x: int = ...`")
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
            FrontEndErrorKind::HopInFunction { .. } => {
                Some("Functions should contain regular logic without hop blocks. Move partition-specific logic to transactions.")
            }
            FrontEndErrorKind::TransactionWithGenerics { .. } => {
                Some("Transactions operate on concrete data partitions and cannot be generic. Use concrete types for all parameters.")
            }
            FrontEndErrorKind::PartitionMustReturnInt { .. } => {
                Some("Partition functions determine how data is distributed across nodes and must return an integer partition ID.")
            }
            FrontEndErrorKind::InferTypeFound { .. } => {
                Some("This indicates a type inference failure. All types should be fully resolved after type checking.")
            }
            FrontEndErrorKind::LambdaCaptureNotSupported { .. } => {
                Some("Lambda expressions currently don't support capturing variables from the outer scope. Pass required values as parameters instead.")
            }
            FrontEndErrorKind::LambdaParameterTypeInference { .. } => {
                Some("Lambda parameters must have explicit type annotations, e.g., (x: int) -> bool instead of (x) -> bool.")
            }
            _ => None,
        }
    }
}
