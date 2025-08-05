// src/cli/output.rs
use super::CompilationResult;
use colored::*;
use std::fs;
use std::io::Write;
use std::path::PathBuf;

pub struct OutputManager;

impl OutputManager {
    pub fn new() -> Self {
        Self
    }

    /// Write all debug output to a directory
    pub fn write_directory_output(
        &self,
        result: &CompilationResult,
        output_dir: &std::path::Path,
    ) -> Result<(), String> {
        // Create output directory
        std::fs::create_dir_all(output_dir)
            .map_err(|e| format!("Failed to create output directory: {}", e))?;

        let output_pathbuf = output_dir.to_path_buf();

        // Write AST dump and pretty print
        self.write_ast_dump(result, &output_pathbuf)?;
        self.write_ast_pretty(result, &output_pathbuf)?;

        // Write CFG dump and pretty print
        self.write_cfg_dump(result, &output_pathbuf)?;
        self.write_cfg_pretty(result, &output_pathbuf)?;

        // Write SC-Graph dump, pretty print, and DOT file
        self.write_scgraph_dump(result, &output_pathbuf)?;
        self.write_scgraph_pretty(result, &output_pathbuf)?;
        self.write_scgraph_dot(result, &output_pathbuf)?;

        // Write compilation log
        self.write_compilation_log(result, &output_pathbuf)?;

        // Write summary
        self.write_summary(result, &output_pathbuf)?;

        Ok(())
    }

    /// Write a console summary of the compilation results
    pub fn write_console_summary(&self, result: &CompilationResult) {
        let stats = result.get_stats();

        if result.success {
            println!("{} Compilation successful", "✓".green().bold());
        } else {
            println!("{} Compilation failed", "✗".red().bold());
        }

        println!(
            "  {}  : {} functions, {} tables, {} partitions",
            "AST".bright_blue(),
            stats.functions,
            stats.tables,
            stats.partitions
        );

        println!(
            "  {}  : {} basic blocks",
            "CFG".bright_blue(),
            stats.basic_blocks
        );

        println!(
            "  {}: {} nodes, {} S-edges, {} C-edges",
            "SC-Graph".bright_blue(),
            stats.sc_nodes,
            stats.s_edges,
            stats.c_edges
        );

        println!(
            "\nCompilation completed in {}ms",
            result.compilation_time_ms
        );
    }

    fn write_compilation_log(
        &self,
        result: &CompilationResult,
        path: &PathBuf,
    ) -> Result<(), String> {
        let file_path = path.join("compilation.log");
        let mut file =
            fs::File::create(file_path).map_err(|e| format!("Failed to create log file: {}", e))?;

        writeln!(file, "FMitF Compilation Log")
            .map_err(|e| format!("Failed to write log: {}", e))?;
        writeln!(
            file,
            "Generated: {}",
            chrono::Utc::now().format("%Y-%m-%d %H:%M:%S UTC")
        )
        .map_err(|e| format!("Failed to write log: {}", e))?;
        writeln!(
            file,
            "\nStatus: {}",
            if result.success { "SUCCESS" } else { "FAILED" }
        )
        .map_err(|e| format!("Failed to write log: {}", e))?;
        writeln!(file, "Compilation time: {}ms", result.compilation_time_ms)
            .map_err(|e| format!("Failed to write log: {}", e))?;

        let stats = result.get_stats();
        writeln!(file, "\nStage Results:").map_err(|e| format!("Failed to write log: {}", e))?;
        writeln!(
            file,
            "- AST: {} functions, {} tables, {} partitions",
            stats.functions, stats.tables, stats.partitions
        )
        .map_err(|e| format!("Failed to write log: {}", e))?;
        writeln!(file, "- CFG: {} basic blocks", stats.basic_blocks)
            .map_err(|e| format!("Failed to write log: {}", e))?;
        writeln!(
            file,
            "- SC-Graph: {} nodes, {} S-edges, {} C-edges",
            stats.sc_nodes, stats.s_edges, stats.c_edges
        )
        .map_err(|e| format!("Failed to write log: {}", e))?;

        Ok(())
    }

    /// Write AST debug dump
    fn write_ast_dump(&self, result: &CompilationResult, path: &PathBuf) -> Result<(), String> {
        let ast_content = format!("{:#?}", result.ast_program);
        let file_path = path.join("ast_dump.txt");
        fs::write(file_path, ast_content)
            .map_err(|e| format!("Failed to write AST dump: {}", e))?;
        Ok(())
    }

    /// Write CFG debug dump  
    fn write_cfg_dump(&self, result: &CompilationResult, path: &PathBuf) -> Result<(), String> {
        let cfg_content = format!("{:#?}", result.cfg_program);
        let file_path = path.join("cfg_dump.txt");
        fs::write(file_path, cfg_content)
            .map_err(|e| format!("Failed to write CFG dump: {}", e))?;
        Ok(())
    }

    /// Write SC-Graph debug dump
    fn write_scgraph_dump(&self, result: &CompilationResult, path: &PathBuf) -> Result<(), String> {
        let scgraph_content = format!("{:#?}", result.sc_graph);
        let file_path = path.join("scgraph_dump.txt");
        fs::write(file_path, scgraph_content)
            .map_err(|e| format!("Failed to write SC-Graph dump: {}", e))?;
        Ok(())
    }

    /// Write pretty-printed AST
    fn write_ast_pretty(&self, result: &CompilationResult, path: &PathBuf) -> Result<(), String> {
        use crate::pretty::{AstPrinter, PrettyPrinter};

        let printer = AstPrinter::new();
        let ast_content = printer
            .print_to_string(&result.ast_program)
            .map_err(|e| format!("Failed to pretty-print AST: {}", e))?;
        let file_path = path.join("ast_pretty.txt");
        fs::write(file_path, ast_content)
            .map_err(|e| format!("Failed to write AST pretty: {}", e))?;
        Ok(())
    }

    /// Write pretty-printed CFG
    fn write_cfg_pretty(&self, result: &CompilationResult, path: &PathBuf) -> Result<(), String> {
        use crate::pretty::{CfgPrinter, PrettyPrinter};

        let printer = CfgPrinter::new();
        let cfg_content = printer
            .print_to_string(&result.cfg_program)
            .map_err(|e| format!("Failed to pretty-print CFG: {}", e))?;
        let file_path = path.join("cfg_pretty.txt");
        fs::write(file_path, cfg_content)
            .map_err(|e| format!("Failed to write CFG pretty: {}", e))?;
        Ok(())
    }

    /// Write pretty-printed SC-Graph
    fn write_scgraph_pretty(
        &self,
        result: &CompilationResult,
        path: &PathBuf,
    ) -> Result<(), String> {
        use crate::pretty::{PrettyPrinter, SCGraphPrinter};

        let printer = SCGraphPrinter::new();
        let scgraph_content = printer
            .print_to_string(&result.sc_graph)
            .map_err(|e| format!("Failed to pretty-print SC-Graph: {}", e))?;
        let file_path = path.join("scgraph_pretty.txt");
        fs::write(file_path, scgraph_content)
            .map_err(|e| format!("Failed to write SC-Graph pretty: {}", e))?;
        Ok(())
    }

    /// Write SC-Graph DOT file for visualization
    fn write_scgraph_dot(&self, result: &CompilationResult, path: &PathBuf) -> Result<(), String> {
        use crate::pretty::DotPrinter;

        let printer = DotPrinter::new();
        let mut dot_content = Vec::new();
        printer
            .generate_dot(&result.sc_graph, &result.cfg_program, &mut dot_content)
            .map_err(|e| format!("Failed to generate DOT file: {}", e))?;

        let file_path = path.join("scgraph.dot");
        fs::write(file_path, dot_content)
            .map_err(|e| format!("Failed to write SC-Graph DOT file: {}", e))?;
        Ok(())
    }

    /// Write summary markdown file
    fn write_summary(&self, result: &CompilationResult, path: &PathBuf) -> Result<(), String> {
        let stats = result.get_stats();
        let status = if result.success {
            "✅ SUCCESS"
        } else {
            "❌ FAILED"
        };

        let summary_content = format!(
            r#"# FMitF Compilation Summary

**Status:** {}  
**Compilation Time:** {}ms  
**Generated:** {}

## Statistics

| Stage | Count |
|-------|-------|
| Functions | {} |
| Tables | {} |
| Partitions | {} |
| Basic Blocks | {} |
| SC-Graph Nodes | {} |
| S-edges (Sequential) | {} |
| C-edges (Conflict) | {} |

## Generated Files

- `ast_dump.txt` - Raw AST structure
- `ast_pretty.txt` - Human-readable AST  
- `cfg_dump.txt` - Raw CFG structure
- `cfg_pretty.txt` - Human-readable CFG
- `scgraph_dump.txt` - Raw SC-Graph structure
- `scgraph_pretty.txt` - Human-readable SC-Graph
- `scgraph.dot` - GraphViz DOT file for visualization
- `compilation.log` - Detailed compilation log

## Visualization

To visualize the SC-Graph, use GraphViz:

```bash
# Generate PNG image
dot -Tpng scgraph.dot -o scgraph.png

# Generate SVG image  
dot -Tsvg scgraph.dot -o scgraph.svg

# Generate PDF
dot -Tpdf scgraph.dot -o scgraph.pdf
```

Legend:
- **Blue arrows (→)**: S-edges (sequential within transactions)
- **Red lines (—)**: C-edges (conflicts between transactions)
"#,
            status,
            result.compilation_time_ms,
            chrono::Utc::now().format("%Y-%m-%d %H:%M:%S UTC"),
            stats.functions,
            stats.tables,
            stats.partitions,
            stats.basic_blocks,
            stats.sc_nodes,
            stats.s_edges,
            stats.c_edges,
        );

        let file_path = path.join("summary.md");
        fs::write(file_path, summary_content)
            .map_err(|e| format!("Failed to write summary: {}", e))?;
        Ok(())
    }
}

/// Error handling utilities
pub fn print_spanned_error(spanned_error: &crate::AstSpannedError, source_code: &str) {
    use colored::*;

    if let Some(span_value) = &spanned_error.span {
        eprintln!(
            "{}: {} at line {}, column {}",
            spanned_error.error.error_type().red().bold(),
            spanned_error.error.message(),
            span_value.line.to_string().red(),
            span_value.column.to_string().red()
        );
        if let Some(line_content) = source_code.lines().nth(span_value.line.saturating_sub(1)) {
            eprintln!("   |");
            eprintln!("{} | {}", span_value.line.to_string().red(), line_content);
            eprintln!("   | {}{}", " ".repeat(span_value.column), "^".red().bold());
        }
    } else {
        eprintln!(
            "{}: {}",
            spanned_error.error.error_type().red().bold(),
            spanned_error.error.message()
        );
    }
}
