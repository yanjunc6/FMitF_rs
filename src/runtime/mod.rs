//! TransAct Interactive Runtime (Simple Testing REPL)
//! 
//! This is a minimal REPL for testing TransAct programs. Keep it simple!
//!
//! ## Simple Architecture (3 files only):
//!
//! 1. **State** (`state.rs`): One struct with CFG + simple maps for everything
//! 2. **Executor** (`executor.rs`): Walk CFG blocks, update maps
//! 3. **Repl** (`repl.rs`): clap-repl loop + table printing
//!
//! ## Design Principles:
//! - Use HashMap/BTreeMap for everything (tables, name lookups)  
//! - Primary keys as Vec<RuntimeValue> (no string concat)
//! - ID-based internally, but simple string→ID lookup
//! - CFG execution only (no AST interpretation)
//! - Minimal error handling for testing

// use crate::cfg::{FunctionId, TableId, FieldId}; // Will be used in implementation files
use std::fmt;

// Simple 3-file architecture
mod state;    // RuntimeState - one struct with everything
mod executor; // execute_function() - walk CFG blocks  
mod repl;     // REPL loop with clap-repl

pub use state::RuntimeState;
pub use executor::execute_function;
pub use repl::start_runtime_repl;

/// Runtime values - keep it dead simple  
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

/// REPL commands for clap-repl
#[derive(clap::Parser, Debug, Clone)]
pub enum ReplCommand {
    /// Load a TransAct file: load examples/bank.transact
    Load { file: String },
    
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
    
    /// Exit REPL
    Exit,
}

// Display implementations
impl fmt::Display for RuntimeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RuntimeValue::Int(i) => write!(f, "{}", i),
            RuntimeValue::String(s) => write!(f, "{}", s),
            RuntimeValue::Bool(b) => write!(f, "{}", b),
        }
    }
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