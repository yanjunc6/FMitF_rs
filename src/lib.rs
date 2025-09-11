#![allow(non_snake_case)]

pub mod ast;
pub mod cli;
// pub mod frontend;  // Disabled due to parsing dependencies
pub mod pretty;
pub mod util;

// Re-export main functionality
pub use ast::{Program};
pub use util::{CompilerError, DiagnosticReporter, Span};
