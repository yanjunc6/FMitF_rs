// src/cli/output_manager.rs
//! Centralized output management for compilation results

use crate::{AstProgram, CfgProgram};
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};

/// Manages all file output for compilation, ensuring proper ordering and no overwrites
pub struct OutputManager {
    output_dir: PathBuf,
    log_file: Option<File>,
}

impl OutputManager {
    pub fn new(output_dir: PathBuf) -> Result<Self, String> {
        // Create output directory
        std::fs::create_dir_all(&output_dir)
            .map_err(|e| format!("Failed to create output directory: {}", e))?;

        // Initialize compilation.log file
        let log_path = output_dir.join("compilation.log");
        let log_file = File::create(&log_path)
            .map_err(|e| format!("Failed to create compilation.log: {}", e))?;

        Ok(Self {
            output_dir,
            log_file: Some(log_file),
        })
    }

    /// Write a line to compilation.log
    pub fn write_log_line(&mut self, line: &str) -> Result<(), String> {
        if let Some(ref mut log_file) = self.log_file {
            writeln!(log_file, "{}", line)
                .map_err(|e| format!("Failed to write to compilation.log: {}", e))?;
            log_file
                .flush()
                .map_err(|e| format!("Failed to flush compilation.log: {}", e))?;
        }
        Ok(())
    }

    /// Write multiple lines to compilation.log
    pub fn write_log_lines(&mut self, lines: &[String]) -> Result<(), String> {
        for line in lines {
            self.write_log_line(line)?;
        }
        Ok(())
    }

    /// Write a section header to compilation.log
    pub fn write_log_section(&mut self, section: &str) -> Result<(), String> {
        self.write_log_line("")?;
        self.write_log_line(&format!("=== {} ===", section))
    }

    /// Write compilation start info
    pub fn write_compilation_start(&mut self, input_path: &Path) -> Result<(), String> {
        self.write_log_line(&format!(
            "Compilation started for: {}",
            input_path.display()
        ))?;
        self.write_log_line(&format!("Output directory: {}", self.output_dir.display()))?;
        self.write_log_line("")
    }

    /// Write stage completion info
    pub fn write_stage_completion(
        &mut self,
        stage_name: &str,
        success: bool,
        duration_ms: Option<u64>,
    ) -> Result<(), String> {
        let status = if success { "SUCCESS" } else { "FAILED" };
        let line = if let Some(ms) = duration_ms {
            format!("{}: {} ({}ms)", stage_name, status, ms)
        } else {
            format!("{}: {}", stage_name, status)
        };
        self.write_log_line(&line)
    }

    /// Write final compilation statistics
    pub fn write_compilation_stats(
        &mut self,
        ast: &AstProgram,
        cfg: &CfgProgram,
        scg: &crate::sc_graph::SCGraph,
        boogie_program_count: usize,
        total_duration_ms: u64,
    ) -> Result<(), String> {
        self.write_log_section("Compilation Statistics")?;
        self.write_log_line(&format!("Total compilation time: {} ms", total_duration_ms))?;
        self.write_log_line(&format!(
            "AST: {} functions, {} tables, {} partitions",
            ast.functions.len(),
            ast.tables.len(),
            ast.partitions.len()
        ))?;
        self.write_log_line(&format!(
            "CFG: {} basic blocks",
            cfg.functions
                .iter()
                .map(|(_, f)| f.blocks.len())
                .sum::<usize>()
        ))?;

        let s_edges = scg
            .edges
            .iter()
            .filter(|e| e.edge_type == crate::sc_graph::EdgeType::S)
            .count();
        let c_edges = scg
            .edges
            .iter()
            .filter(|e| e.edge_type == crate::sc_graph::EdgeType::C)
            .count();
        self.write_log_line(&format!(
            "SC-Graph: {} nodes, {} S-edges, {} C-edges",
            scg.nodes.len(),
            s_edges,
            c_edges
        ))?;
        self.write_log_line(&format!(
            "Verification: {} Boogie programs generated",
            boogie_program_count
        ))
    }

    /// Write Boogie verification results
    pub fn write_boogie_verification_result(
        &mut self,
        filename: &str,
        success: bool,
        stdout: &str,
        stderr: &str,
    ) -> Result<(), String> {
        self.write_log_line(&format!("File: {}", filename))?;
        self.write_log_line(&format!(
            "Status: {}",
            if success { "Success" } else { "Failed" }
        ))?;

        if !stdout.trim().is_empty() {
            self.write_log_line("Output:")?;
            for line in stdout.lines() {
                self.write_log_line(&format!("  {}", line))?;
            }
        }

        if !stderr.trim().is_empty() {
            self.write_log_line("Errors:")?;
            for line in stderr.lines() {
                self.write_log_line(&format!("  {}", line))?;
            }
        }

        self.write_log_line("") // Empty line between files
    }

    /// Write AST output files
    pub fn write_ast_output(&self, ast: &AstProgram) -> Result<(), String> {
        let ast_dump_path = self.output_dir.join("ast_dump.txt");
        let ast_pretty_path = self.output_dir.join("ast_pretty.txt");

        // Write AST dump
        let ast_dump = format!("{:#?}", ast);
        std::fs::write(&ast_dump_path, ast_dump)
            .map_err(|e| format!("Failed to write ast_dump.txt: {}", e))?;

        // Write AST pretty print
        use crate::pretty::print_program_to_string;
        let ast_pretty = print_program_to_string(ast)
            .map_err(|e| format!("Failed to pretty print AST: {}", e))?;
        std::fs::write(&ast_pretty_path, ast_pretty)
            .map_err(|e| format!("Failed to write ast_pretty.txt: {}", e))?;

        Ok(())
    }

    /// Write CFG output files
    pub fn write_cfg_output(&self, cfg: &CfgProgram, prefix: &str) -> Result<(), String> {
        let cfg_dump_path = self.output_dir.join(format!("{}_dump.txt", prefix));
        let cfg_pretty_path = self.output_dir.join(format!("{}_pretty.txt", prefix));

        // Write CFG dump
        let cfg_dump = format!("{:#?}", cfg);
        std::fs::write(&cfg_dump_path, cfg_dump)
            .map_err(|e| format!("Failed to write {}_dump.txt: {}", prefix, e))?;

        // Write CFG pretty print
        use crate::pretty::{CfgPrinter, PrettyPrinter};
        let printer = CfgPrinter::new();
        let cfg_pretty = printer
            .print_to_string(cfg)
            .map_err(|e| format!("Failed to pretty print CFG: {}", e))?;
        std::fs::write(&cfg_pretty_path, cfg_pretty)
            .map_err(|e| format!("Failed to write {}_pretty.txt: {}", prefix, e))?;

        Ok(())
    }

    /// Write SC-Graph output files with custom filename prefix
    pub fn write_scgraph_output_with_name(
        &self,
        scg: &crate::sc_graph::SCGraph,
        cfg: &CfgProgram,
        filename_prefix: &str,
    ) -> Result<(), String> {
        let scgraph_dump_path = self
            .output_dir
            .join(format!("{}_dump.txt", filename_prefix));
        let scgraph_pretty_path = self
            .output_dir
            .join(format!("{}_pretty.txt", filename_prefix));
        let scgraph_dot_path = self.output_dir.join(format!("{}.dot", filename_prefix));

        // Write SC-Graph dump
        let scgraph_dump = format!("{:#?}", scg);
        std::fs::write(&scgraph_dump_path, scgraph_dump)
            .map_err(|e| format!("Failed to write {}_dump.txt: {}", filename_prefix, e))?;

        // Write SC-Graph pretty print
        use crate::pretty::{DotPrinter, PrettyPrinter, SCGraphPrinter};
        let printer = SCGraphPrinter::new();
        let scgraph_pretty = printer
            .print_to_string(scg)
            .map_err(|e| format!("Failed to pretty print SC-Graph: {}", e))?;
        std::fs::write(&scgraph_pretty_path, scgraph_pretty)
            .map_err(|e| format!("Failed to write {}_pretty.txt: {}", filename_prefix, e))?;

        // Write SC-Graph DOT file
        let dot_printer = DotPrinter::new();
        let mut dot_content = Vec::new();
        dot_printer
            .generate_dot(scg, cfg, &mut dot_content)
            .map_err(|e| format!("Failed to generate DOT file: {}", e))?;
        std::fs::write(&scgraph_dot_path, dot_content)
            .map_err(|e| format!("Failed to write {}.dot: {}", filename_prefix, e))?;

        Ok(())
    }

    /// Write combined SC-Graph output files with custom filename prefix
    pub fn write_combined_scgraph_output_with_name(
        &self,
        combined_scg: &crate::sc_graph::CombinedSCGraph,
        cfg: &CfgProgram,
        filename_prefix: &str,
    ) -> Result<(), String> {
        let combined_dump_path = self
            .output_dir
            .join(format!("{}_dump.txt", filename_prefix));
        let combined_pretty_path = self
            .output_dir
            .join(format!("{}_pretty.txt", filename_prefix));
        let combined_dot_path = self.output_dir.join(format!("{}.dot", filename_prefix));

        // Write Combined SC-Graph dump
        let combined_dump = format!("{:#?}", combined_scg);
        std::fs::write(&combined_dump_path, combined_dump)
            .map_err(|e| format!("Failed to write {}_dump.txt: {}", filename_prefix, e))?;

        // Write Combined SC-Graph pretty print
        let mut pretty_content = String::new();
        pretty_content.push_str(&format!("Combined SC-Graph ({})\n", filename_prefix));
        pretty_content.push_str(&format!(
            "Vertices: {} | S-edges: {} | C-edges: {} | Acyclic: {}\n\n",
            combined_scg.vertices.len(),
            combined_scg
                .edges
                .iter()
                .filter(|e| e.edge_type == crate::sc_graph::EdgeType::S)
                .count(),
            combined_scg
                .edges
                .iter()
                .filter(|e| e.edge_type == crate::sc_graph::EdgeType::C)
                .count(),
            combined_scg.is_acyclic()
        ));

        for vertex in &combined_scg.vertices {
            pretty_content.push_str(&format!("Vertex {}:\n", vertex.id));
            for piece in &vertex.pieces {
                let function_name = cfg
                    .functions
                    .get(piece.function_id)
                    .map(|f| f.name.clone())
                    .unwrap_or_else(|| format!("unknown_{:?}", piece.function_id));
                pretty_content.push_str(&format!(
                    "  {} #{} [hops: {:?}]\n",
                    function_name,
                    piece.instance + 1,
                    piece.hop_ids
                ));
            }
            pretty_content.push_str("\n");
        }

        pretty_content.push_str("Edges:\n");
        for edge in &combined_scg.edges {
            let edge_type_str = match edge.edge_type {
                crate::sc_graph::EdgeType::S => "S",
                crate::sc_graph::EdgeType::C => "C",
            };
            pretty_content.push_str(&format!(
                "  {} -> {} ({})\n",
                edge.source, edge.target, edge_type_str
            ));
        }

        std::fs::write(&combined_pretty_path, pretty_content)
            .map_err(|e| format!("Failed to write {}_pretty.txt: {}", filename_prefix, e))?;

        // Write Combined SC-Graph DOT file
        use crate::pretty::CombinedDotPrinter;
        let combined_dot_printer = CombinedDotPrinter::new();
        let mut dot_content = Vec::new();
        combined_dot_printer
            .generate_dot(combined_scg, cfg, &mut dot_content)
            .map_err(|e| format!("Failed to generate combined DOT file: {}", e))?;
        std::fs::write(&combined_dot_path, dot_content)
            .map_err(|e| format!("Failed to write {}.dot: {}", filename_prefix, e))?;

        Ok(())
    }

    /// Write Boogie verification programs
    pub fn write_boogie_programs(
        &self,
        boogie_programs: &[crate::verification::Boogie::BoogieProgram],
    ) -> Result<(), String> {
        let boogie_dir = self.output_dir.join("Boogie");
        std::fs::create_dir_all(&boogie_dir)
            .map_err(|e| format!("Failed to create Boogie directory: {}", e))?;

        for program in boogie_programs.iter() {
            let file_name = format!("{}.bpl", program.name);
            let file_path = boogie_dir.join(file_name);

            // Write Boogie program to file
            let content = program.to_string();
            std::fs::write(&file_path, content.as_bytes())
                .map_err(|e| format!("Failed to write Boogie file: {}", e))?;
        }

        Ok(())
    }

    /// Write final summary markdown file
    pub fn write_summary_markdown(
        &self,
        input_path: &Path,
        ast: &AstProgram,
        cfg: &CfgProgram,
        scg: &crate::sc_graph::SCGraph,
        boogie_program_count: usize,
        compilation_time_ms: u64,
    ) -> Result<(), String> {
        let summary_path = self.output_dir.join("summary.md");

        let s_edges = scg
            .edges
            .iter()
            .filter(|e| e.edge_type == crate::sc_graph::EdgeType::S)
            .count();
        let c_edges = scg
            .edges
            .iter()
            .filter(|e| e.edge_type == crate::sc_graph::EdgeType::C)
            .count();
        let basic_blocks = cfg
            .functions
            .iter()
            .map(|(_, f)| f.blocks.len())
            .sum::<usize>();

        let summary_content = format!(
            "# FMitF Compilation Report\n\n\
             **Input**: {}\n\
             **Output Directory**: {}\n\
             **Compilation Time**: {} ms\n\
             **Status**: ✅ Success\n\n\
             ## Statistics\n\n\
             | Stage | Count |\n\
             |-------|-------|\n\
             | Functions | {} |\n\
             | Tables | {} |\n\
             | Partitions | {} |\n\
             | Basic Blocks | {} |\n\
             | SC-Graph Nodes | {} |\n\
             | S-edges | {} |\n\
             | C-edges | {} |\n\
             | Boogie Programs | {} |\n\n\
             ## Generated Files\n\n\
             - `ast_dump.txt` - AST debug dump\n\
             - `ast_pretty.txt` - Pretty-printed AST\n\
             - `cfg_dump.txt` - CFG debug dump\n\
             - `cfg_pretty.txt` - Pretty-printed CFG\n\
             - `scgraph_dump.txt` - SC-Graph debug dump\n\
             - `scgraph_pretty.txt` - Pretty-printed SC-Graph\n\
             - `scgraph.dot` - GraphViz DOT file\n\
             - `combined_scgraph_dump.txt` - Combined SC-Graph debug dump\n\
             - `combined_scgraph_pretty.txt` - Combined SC-Graph human-readable format\n\
             - `combined_scgraph.dot` - Combined SC-Graph GraphViz DOT file (deadlock-free)\n\
             - `Boogie/*.bpl` - Boogie verification programs\n\
             - `compilation.log` - Detailed compilation log with Boogie verification results\n",
            input_path.display(),
            self.output_dir.display(),
            compilation_time_ms,
            ast.functions.len(),
            ast.tables.len(),
            ast.partitions.len(),
            basic_blocks,
            scg.nodes.len(),
            s_edges,
            c_edges,
            boogie_program_count,
        );

        std::fs::write(&summary_path, summary_content)
            .map_err(|e| format!("Failed to write summary.md: {}", e))?;

        Ok(())
    }

    /// Get all .bpl files in the Boogie directory for verification
    pub fn get_boogie_files(&self) -> Result<Vec<PathBuf>, String> {
        let boogie_dir = self.output_dir.join("Boogie");

        if !boogie_dir.exists() {
            return Ok(Vec::new()); // No Boogie files to verify
        }

        let bpl_files = std::fs::read_dir(&boogie_dir)
            .map_err(|e| format!("Failed to read Boogie directory: {}", e))?
            .filter_map(|entry| {
                let entry = entry.ok()?;
                let path = entry.path();
                if path.extension()? == "bpl" {
                    Some(path)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        Ok(bpl_files)
    }
}
