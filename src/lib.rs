#![allow(non_snake_case)]

pub mod ast;
pub mod cfg;
pub mod cli;
pub mod frontend;
pub mod pretty;
pub mod util;
// Re-export main functionality
pub use ast::Program;
pub use util::{CompilerError, CompilerErrorKind, DiagnosticReporter, Span};
