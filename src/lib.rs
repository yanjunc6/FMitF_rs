pub mod ast;
pub mod frontend;
pub mod graph;
pub mod output;
pub mod verification;

// Re-export
pub use ast::*;
pub use frontend::analyze::SemanticAnalyzer;
pub use frontend::parse::{format_errors, parse_program, Results, SpannedError, TransActError};
pub use graph::{EdgeType, SCGraph};
pub use output::{print_dot_graph, print_program, save_dot_file, PrintMode, PrintOptions};
pub use verification::AutoVerifier;
