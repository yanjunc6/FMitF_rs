#![allow(non_snake_case)]

pub mod ast;
pub mod cli;
pub mod pretty;
pub mod util;

// Re-export main functionality
pub use ast::{Program, parse_and_analyze};
pub use ast::{ParseError, SemanticError, NameResolutionError, TypeCheckError, ConstantError};
pub use util::{Span, CompilerError, DiagnosticReporter};
