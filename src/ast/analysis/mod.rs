//! AST Analysis Submodules
//!
//! This module contains various analysis passes that can be performed on the AST,
//! organized into separate submodules for maintainability and clarity.

pub mod constant_checker;
mod semantics_analysis; // Keep the original file for now

// Re-export the main interfaces
pub use constant_checker::{
    evaluate_constant_expression, is_constant_expression, ConstantChecker, ConstantValue,
};
pub use semantics_analysis::{analyze_program, analyze_program_with_types, SemanticAnalyzer};
