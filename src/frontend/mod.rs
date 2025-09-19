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
    // A vector to accumulate errors from all stages
    let mut all_errors = Vec::new();

    // Stage 1: Basic AST parsing with prelude
    let mut program = match ast_builder::parse_program(program, source, filename) {
        Ok(p) => p,
        Err(errors) => {
            all_errors.extend(errors);
            return Err(all_errors);
        }
    };

    // Stage 1.5: Generate accessor functions for table Row<T> types
    // generated_functions::generate_table_accessors(&mut program);
    // no need to have get/set functions

    // Stage 2: Name resolution
    if let Err(errors) = name_resolver::resolve_names(&mut program) {
        all_errors.extend(errors);
    }

    // Stage 3: Type checking
    if let Err(errors) = type_resolver::resolve_types(&mut program) {
        all_errors.extend(errors);
    }

    // If there are errors from critical early stages, we might not want to proceed.
    if !all_errors.is_empty() {
        return Err(all_errors);
    }

    // Stage 4: Semantic analysis
    if let Err(errors) = semantics_analyzer::analyze_semantics(&program) {
        all_errors.extend(errors);
    }

    if !all_errors.is_empty() {
        Err(all_errors)
    } else {
        Ok(program)
    }
}

// ============================================================================
// --- Module Declarations
// ============================================================================

pub mod ast_builder;
pub mod errors;
pub mod generated_functions;
pub mod name_resolver;
pub mod semantics_analyzer;
pub mod type_resolver;
pub use errors::FrontEndErrorKind;
