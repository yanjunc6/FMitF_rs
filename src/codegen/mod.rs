mod gen_converters;
mod gen_global;
mod gen_transaction;
mod gen_util;
mod util;

use crate::cfg;
use crate::sc_graph::SCGraph;
use std::error::Error;

#[derive(Debug, Clone)]
pub struct GoProgram {
    pub filename: String,
    pub content: String,
}

impl GoProgram {
    pub fn new(filename: impl Into<String>, content: impl Into<String>) -> Self {
        Self {
            filename: filename.into(),
            content: content.into(),
        }
    }
}

/// Generate Go artifacts for the optimized CFG.
///
/// Returns an in-memory collection of files to be written by the caller.
pub fn generate_go_code(
    program: &cfg::Program,
    sc_graph: &SCGraph,
) -> Result<Vec<GoProgram>, Box<dyn Error>> {
    let mut files = Vec::new();
    files.push(gen_util::generate_util()?);
    files.push(gen_converters::generate_converters()?);
    files.push(gen_global::generate_global(program, sc_graph)?);
    files.extend(gen_transaction::generate_transactions(program, sc_graph)?);
    Ok(files)
}
