use crate::cfg::CfgProgram;
use crate::verification::commutativity_check::VerificationUnit;
use std::fs;
use std::path::{Path, PathBuf};

/// A Boogie file entry containing filename and code
#[derive(Debug, Clone)]
pub struct BoogieFile {
    pub filename: String,
    pub code: String,
}

/// Abstracted file operations for Boogie files
pub struct BoogieFileManager;

impl BoogieFileManager {
    /// Generate a consistent filename from a VerificationUnit
    pub fn generate_filename(unit: &VerificationUnit, cfg: &CfgProgram) -> String {
        let func_a = &cfg.functions[unit.function_a];
        let func_b = &cfg.functions[unit.function_b];

        format!(
            "{}_{}_{}_{}.bpl",
            func_a.name,
            unit.final_a.index(),
            func_b.name,
            unit.final_b.index()
        )
    }

    /// Write a Boogie file to a specific directory
    pub fn write_file(file: &BoogieFile, dir: &Path) -> Result<PathBuf, String> {
        let file_path = dir.join(&file.filename);
        fs::write(&file_path, &file.code)
            .map_err(|e| format!("Failed to write Boogie file {}: {}", file_path.display(), e))?;
        Ok(file_path)
    }

    /// Write multiple Boogie files to a directory
    pub fn write_files(files: &[BoogieFile], dir: &Path) -> Result<Vec<PathBuf>, String> {
        // Create the output directory if it doesn't exist
        fs::create_dir_all(dir)
            .map_err(|e| format!("Failed to create directory {}: {}", dir.display(), e))?;

        let mut paths = Vec::new();
        for file in files {
            let path = Self::write_file(file, dir)?;
            paths.push(path);
        }
        Ok(paths)
    }

    /// Write a temporary Boogie file for verification
    pub fn write_temp_file(file: &BoogieFile) -> Result<PathBuf, String> {
        let temp_dir = Path::new("tmp");
        fs::create_dir_all(temp_dir)
            .map_err(|e| format!("Failed to create tmp directory: {}", e))?;
        Self::write_file(file, temp_dir)
    }

    /// Clean up a list of files
    pub fn cleanup_files(file_paths: &[PathBuf]) {
        for file_path in file_paths {
            if let Err(e) = fs::remove_file(file_path) {
                eprintln!(
                    "Warning: Failed to remove file {}: {}",
                    file_path.display(),
                    e
                );
            }
        }
    }
}
