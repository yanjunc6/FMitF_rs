//! AST Builder
//!
//! This module provides the `AstBuilder` struct which converts Pest parse trees
//! into the AST defined in `mod.rs`. It handles:
//! - Converting Pest pairs to AST nodes
//! - Creating proper arena-based storage for all nodes
//! - Maintaining span information for error reporting
//! - Building the complete program structure

use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;

use crate::ast::errors::*;
use crate::ast::*;

// ============================================================================
// --- Default Program
// ============================================================================

impl Default for Program {
    fn default() -> Self {
        // default program with a prelude defined in `src/ast/prelude.transact`
        todo!()
    }
}



// ============================================================================
// --- Pest Parser Definition
// ============================================================================

#[derive(Parser)]
#[grammar = "ast/grammar.pest"]
pub struct TransactParser;

// ============================================================================
// --- AST Builder
// ============================================================================

pub struct AstBuilder {
    pub program: Program,
    errors: Vec<AstError>,
}

impl AstBuilder {

}

// ============================================================================
// --- Public Interface
// ============================================================================

/// Parse a source string into an AST Program  
pub fn parse_program(source: &str) -> Result<Program, Vec<AstError>> {
    AstBuilder::parse(source)
}
