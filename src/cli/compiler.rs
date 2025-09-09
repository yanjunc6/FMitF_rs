use colored::*;
use std::time::Instant;

use crate::cli::{print_ast_spanned_error, Cli, Logger, OutputManager};
use crate::pretty::print_program;
use crate::{parse_and_analyze, AstProgram, AstSpannedError};

/// Represents the result of compilation
#[derive(Debug)]
pub struct CompilationResult {
    pub success: bool,
    pub compilation_time_ms: u64,
    pub ast: Option<AstProgram>,
}

/// Simple AST-only compiler that parses .transact files
pub struct Compiler {
    pub logger: Logger,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            logger: Logger::new(),
        }
    }

    /// Main compilation method - simplified to only parse AST
    pub fn compile(&mut self, source_code: &str, cli: &Cli) -> Result<CompilationResult, String> {
        let start_time = Instant::now();

        self.logger.process_start("AST parsing");

        let ast_result = self.stage_ast(source_code, cli);

        let compilation_time = start_time.elapsed();
        let compilation_time_ms = compilation_time.as_millis() as u64;

        match ast_result {
            Ok(ast) => {
                self.logger.success(&format!(
                    "AST parsing completed successfully in {} ms",
                    compilation_time_ms
                ));

                // Generate output
                let output_manager = OutputManager::new(&cli.get_output_dir());
                if let Err(e) = output_manager.write_ast_output(&ast) {
                    self.logger
                        .error(&format!("Failed to write AST output: {}", e));
                }

                Ok(CompilationResult {
                    success: true,
                    compilation_time_ms,
                    ast: Some(ast),
                })
            }
            Err(errors) => {
                self.logger
                    .error_with_count("AST parsing failed", errors.len());

                // Print all AST errors
                for error in &errors {
                    print_ast_spanned_error(error, source_code, "input.transact");
                }

                Ok(CompilationResult {
                    success: false,
                    compilation_time_ms,
                    ast: None,
                })
            }
        }
    }

    /// Stage 1: Parse and analyze AST  
    fn stage_ast(
        &mut self,
        source_code: &str,
        _cli: &Cli,
    ) -> Result<AstProgram, Vec<AstSpannedError>> {
        self.logger.stage_start(1, 1, "Parsing AST");

        match parse_and_analyze(source_code) {
            crate::ast::errors::Results::Success(ast) => {
                self.logger.stage_success();
                self.logger.detail(&format!(
                    "AST parsed successfully with {} functions",
                    ast.functions.len()
                ));
                Ok(ast)
            }
            crate::ast::errors::Results::Failure(errors) => {
                self.logger.stage_error(errors.len());
                Err(errors)
            }
        }
    }

    /// Print compilation complete message
    pub fn print_compilation_complete(&self, compilation_time_ms: u64) {
        println!(
            "{} Compilation completed successfully in {} ms",
            "SUCCESS:".green().bold(),
            compilation_time_ms.to_string().bright_green()
        );
    }

    /// Print compilation failed message
    pub fn print_compilation_failed(&self, compilation_time_ms: u64, error_count: usize) {
        println!(
            "{} Compilation failed with {} error(s) in {} ms",
            "FAILED:".red().bold(),
            error_count.to_string().bright_red(),
            compilation_time_ms.to_string().bright_red()
        );
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
