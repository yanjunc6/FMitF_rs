pub mod ast;
pub mod frontend;
pub mod output;

// Re-export commonly used items for easier imports
pub use ast::*;
pub use frontend::parse::{parse_program, TransActError, Results, SpannedError, format_errors};
pub use frontend::analyze::SemanticAnalyzer;
pub use output::{print_program, PrintOptions, PrintMode};