#![allow(non_snake_case)]

pub mod ast;
pub mod cfg;
pub mod cli;
pub mod codegen;
pub mod dataflow;
pub mod frontend;
pub mod optimization;
pub mod pretty;
pub mod sc_graph;
pub mod util;
pub mod verification;
// Re-export main functionality
pub use ast::Program;
pub use util::{CompilerError, CompilerErrorKind, DiagnosticReporter, Span};
