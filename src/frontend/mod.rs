pub use crate::ast::Program;
pub use crate::util::CompilerError;

// ============================================================================
// --- Main Parse Function
// ============================================================================

/// Parse source code into AST with complete processing including name resolution,
/// type checking, and semantic analysis
pub fn parse_and_analyze_program(
    source: &str,
    filename: &'static str,
) -> Result<Program, Vec<CompilerError>> {
    // Stage 1: Basic AST parsing with prelude
    let program = ast_builder::parse_program(source, filename)?;

    // Stage 2: Name resolution
    // if let Err(errors) = name_resolver::resolve_names(&mut program) {
    //     return Err(errors);
    // }

    // Stage 3: Type checking (disabled for now)
    // if let Err(errors) = type_checker::check_types(&mut program) {
    //     return Err(errors);
    // }

    // Stage 4: Semantic analysis (disabled for now)
    // if let Err(errors) = semantic_analyzer::analyze_semantics(&program) {
    //     return Err(errors);
    // }

    Ok(program)
}

// ============================================================================
// --- Module Declarations
// ============================================================================

pub mod ast_builder;
pub mod errors;
// pub mod name_resolver;
pub mod util;
// pub mod type_resolver;
// pub mod type_checker;
// pub mod semantic_analyzer;
// Complex modules with legacy issues:
// pub mod constant_checker;
// pub mod ast_debug;

// ============================================================================
// --- Public Interface Re-exports
// ============================================================================

// Core types - using unified error system
pub use errors::FrontEndErrorKind;

// AST builder functions

// Analysis phases (disabled for now)
// pub use name_resolver::resolve_names;
// pub use type_resolver::resolve_types;
// pub use semantics_analysis::analyze_semantics;
// pub use constant_checker::{check_constants, evaluate_constant_expression, ConstantValue};

// Debug utilities (disabled for now)
// pub use ast_debug::{print_program, print_expression, print_statement, DebugConfig};
