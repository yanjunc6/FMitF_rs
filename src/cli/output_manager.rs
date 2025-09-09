//! Output Management for AST-only Compilation
//!
//! This module handles generating and writing output files for the simplified
//! AST-only compilation pipeline.

use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use crate::pretty::print_program;
use crate::AstProgram;

/// Manages output generation for AST compilation
pub struct OutputManager {
    output_dir: PathBuf,
}

impl OutputManager {
    /// Create a new OutputManager with the specified output directory
    pub fn new(output_dir: &Path) -> Self {
        Self {
            output_dir: output_dir.to_path_buf(),
        }
    }

    /// Write AST output files
    pub fn write_ast_output(&self, ast: &AstProgram) -> io::Result<()> {
        // Create output directory if it doesn't exist
        if !self.output_dir.exists() {
            fs::create_dir_all(&self.output_dir)?;
        }

        // Generate AST summary
        let summary = self.generate_ast_summary(ast);

        // Write summary file
        let summary_path = self.output_dir.join("ast_summary.txt");
        fs::write(&summary_path, summary)?;

        // Write AST dump (debug format)
        let ast_dump = format!("{:#?}", ast);
        let dump_path = self.output_dir.join("ast_dump.txt");
        fs::write(&dump_path, ast_dump)?;

        // Write pretty-printed AST
        let pretty_ast = print_program(ast);
        let pretty_path = self.output_dir.join("ast_pretty.txt");
        fs::write(&pretty_path, pretty_ast)?;

        println!("AST output written to: {}", self.output_dir.display());

        Ok(())
    }

    /// Generate a human-readable summary of the AST
    fn generate_ast_summary(&self, ast: &AstProgram) -> String {
        let mut summary = String::new();

        summary.push_str("=== AST Summary ===\n\n");

        // Basic statistics
        summary.push_str(&format!("Functions: {}\n", ast.functions.len()));
        summary.push_str(&format!("Tables: {}\n", ast.table_decls.len()));
        summary.push_str(&format!("Type Declarations: {}\n", ast.type_decls.len()));
        summary.push_str(&format!("Constants: {}\n", ast.const_decls.len()));

        summary.push_str("\n=== Functions ===\n");
        for function_id in ast.functions.iter() {
            let function = &ast.functions[function_id.0];
            summary.push_str(&format!("- {:?}\n", function.name.value));
            summary.push_str(&format!("  Parameters: {}\n", function.params.len()));
            if let Some(_body) = &function.body {
                summary.push_str("  Has body\n");
            } else {
                summary.push_str("  Forward declaration\n");
            }
        }

        summary.push_str("\n=== Tables ===\n");
        for table_id in ast.table_decls.iter() {
            let table = &ast.table_decls[table_id.0];
            summary.push_str(&format!("- {:?}\n", table.name.value));
            // TODO: Add field count when we understand table structure
        }

        if ast.type_decls.len() > 0 {
            summary.push_str("\n=== Type Declarations ===\n");
            for type_id in ast.type_decls.iter() {
                let type_decl = &ast.type_decls[type_id.0];
                summary.push_str(&format!("- {:?}\n", type_decl.name.value));
            }
        }

        if ast.const_decls.len() > 0 {
            summary.push_str("\n=== Constants ===\n");
            for const_id in ast.const_decls.iter() {
                let const_decl = &ast.const_decls[const_id.0];
                summary.push_str(&format!("- {:?}\n", const_decl.name.value));
            }
        }

        summary
    }
}
