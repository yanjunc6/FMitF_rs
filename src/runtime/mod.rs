//! TransAct Interactive Runtime
//! 
//! This module provides a simple REPL environment for testing TransAct programs.
//! It's designed for quick testing and experimentation, not production use.

use std::fmt;

/// Runtime values - keep it simple
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RuntimeValue {
    Int(i64),
    String(String),
    Bool(bool),
}

/// Minimal errors for testing
#[derive(Debug)]
pub enum RuntimeError {
    ParseError(String),
    NotFound(String),
    ExecutionError(String),
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RuntimeError::ParseError(msg) => write!(f, "Parse error: {}", msg),
            RuntimeError::NotFound(msg) => write!(f, "Not found: {}", msg),
            RuntimeError::ExecutionError(msg) => write!(f, "Execution error: {}", msg),
        }
    }
}

impl fmt::Display for RuntimeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RuntimeValue::Int(i) => write!(f, "{}", i),
            RuntimeValue::String(s) => write!(f, "{}", s),
            RuntimeValue::Bool(b) => write!(f, "{}", b),
        }
    }
}

/// REPL commands for clap-repl
#[derive(clap::Parser, Debug, Clone)]
pub enum ReplCommand {
    /// Call function: call transfer 1 2 100  
    Call { 
        function: String,
        #[arg(trailing_var_arg = true)]
        args: Vec<String>,
    },
    
    /// Show table: table Account
    Table { name: String },
    
    /// List functions
    Functions,
    
    /// List tables  
    Tables,
    
    /// Clear all table data (reset database)
    Clear,
    
    /// Exit REPL
    Exit,
}

// Module declarations
mod state;
mod executor; 
mod repl;

// Re-exports
pub use state::RuntimeState;
pub use executor::execute_function;
pub use repl::{start_runtime_repl, start_runtime_repl_with_cfg};