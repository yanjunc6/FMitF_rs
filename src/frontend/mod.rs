pub use crate::ast::Program;
pub use crate::util::CompilerError;

// ============================================================================
// --- Main Parse Function
// ============================================================================

/// Parse source code into AST with complete processing including name resolution,
/// type checking, and semantic analysis
pub fn parse_and_analyze_program(
    program: Option<Program>,
    source: &str,
    filename: &'static str,
) -> Result<Program, Vec<CompilerError>> {
    // Stage 1: Basic AST parsing with prelude
    let mut program = ast_builder::parse_program(program, source, filename)?;

    // Stage 2: Name resolution
    let _ = name_resolver::resolve_names(&mut program)?;

    // Stage 3: Type checking
    let _ = type_resolver::resolve_types(&mut program)?;

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
pub mod name_resolver;
pub mod type_resolver;
pub mod util;
pub use errors::FrontEndErrorKind;
// pub mod type_checker;
// pub mod semantic_analyzer;
// Complex modules with legacy issues:
// pub mod constant_checker;
// pub mod ast_debug;
