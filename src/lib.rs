pub mod ast;
pub mod frontend;
pub mod graph;
pub mod output;

// Re-export
pub use ast::*;
pub use frontend::parse::{parse_program, TransActError, Results, SpannedError, format_errors};
pub use frontend::analyze::SemanticAnalyzer;
pub use output::{print_program, PrintOptions, PrintMode, save_dot_file, print_dot_graph};
pub use graph::{SCGraph, EdgeType};