#![allow(non_snake_case)]

pub mod ast;
pub mod cli;
pub mod pretty;
pub mod util;

// Re-export main functionality
pub use ast::{parse_program, Program};
pub use ast::{AstError, AstErrorKind};
pub use util::{CompilerError, DiagnosticReporter, Span};
