mod gen_global;

use crate::cfg;
use std::error::Error;
use std::path::{Path, PathBuf};

/// Generate Go artifacts for the optimized CFG.
///
/// Currently emits `global.go` containing table definitions and helpers.
pub fn generate_go_files(
    program: &cfg::Program,
    output_dir: &Path,
) -> Result<Vec<PathBuf>, Box<dyn Error>> {
    let global_path = gen_global::generate_global(program, output_dir)?;
    Ok(vec![global_path])
}
