// src/cli/pipeline.rs
use super::{output::*, stages::*, traits::*, Cli, Mode, StageContext};

pub struct Pipeline {
    pub ast_stage: AstStage,
    pub cfg_stage: CfgStage,
    pub optimize_stage: OptimizeStage,
    pub scgraph_stage: ScGraphStage,
    pub verification_stage: VerificationStage,
}

impl Pipeline {
    pub fn new(cli: &Cli) -> Self {
        Self {
            ast_stage: AstStage,
            cfg_stage: CfgStage,
            optimize_stage: OptimizeStage {
                skip_optimization: cli.no_optimize,
            },
            scgraph_stage: ScGraphStage,
            verification_stage: VerificationStage {
                timeout: cli.timeout,
            },
        }
    }

    pub fn execute(
        &mut self,
        source_code: String,
        target_mode: Mode,
        cli: &Cli,
    ) -> Result<(), String> {
        let ctx = StageContext::new(cli).with_source(&source_code);

        // Stage 1: AST
        if !cli.quiet {
            println!(
                "⚙️ Stage {}/5: {}...",
                self.ast_stage.stage_number(),
                self.ast_stage.name()
            );
        }

        let ast_program = self
            .ast_stage
            .execute(source_code.clone())
            .map_err(|errors| {
                eprintln!("❌ AST stage failed with {} error(s):", errors.len());
                for error in &errors {
                    if let Some(source) = ctx.source_code {
                        print_spanned_error(error, source);
                    }
                }
                "AST stage failed".to_string()
            })?;

        if !cli.quiet {
            println!("✅ {} completed successfully", self.ast_stage.name());
        }

        if target_mode == Mode::Ast {
            return OutputManager::handle_file_output(&self.ast_stage, &ast_program, cli);
        }

        // Stage 2: CFG
        if !cli.quiet {
            println!(
                "⚙️ Stage {}/5: {}...",
                self.cfg_stage.stage_number(),
                self.cfg_stage.name()
            );
        }

        let cfg_program = self.cfg_stage.execute(ast_program).map_err(|e| {
            eprintln!("❌ CFG stage failed: {}", e);
            e
        })?;

        if !cli.quiet {
            println!("✅ {} completed successfully", self.cfg_stage.name());
        }

        if target_mode == Mode::Cfg {
            return OutputManager::handle_file_output(&self.cfg_stage, &cfg_program, cli);
        }

        // Stage 3: Optimization
        if !cli.quiet {
            if self.optimize_stage.skip_optimization {
                println!(
                    "⚙️ Stage {}/5: {} (skipped due to --no-optimize)",
                    self.optimize_stage.stage_number(),
                    self.optimize_stage.name()
                );
            } else {
                println!(
                    "⚙️ Stage {}/5: {}...",
                    self.optimize_stage.stage_number(),
                    self.optimize_stage.name()
                );
            }
        }

        let optimized_cfg = self.optimize_stage.execute(cfg_program)?;

        if !cli.quiet {
            if self.optimize_stage.skip_optimization {
                println!("✅ {} skipped", self.optimize_stage.name());
            } else {
                println!("✅ {} completed successfully", self.optimize_stage.name());
            }
        }

        if target_mode == Mode::Optimize {
            return OutputManager::handle_file_output(&self.optimize_stage, &optimized_cfg, cli);
        }

        // Stage 4: SC-Graph
        if !cli.quiet {
            println!(
                "⚙️ Stage {}/5: {}...",
                self.scgraph_stage.stage_number(),
                self.scgraph_stage.name()
            );
        }

        let (cfg_program, sc_graph) = self.scgraph_stage.execute(optimized_cfg)?;

        if !cli.quiet {
            // We'll get the summary after verification to avoid moving the values
            println!("✅ {} completed successfully", self.scgraph_stage.name());
        }

        if target_mode == Mode::Scgraph {
            // For Scgraph mode, we need to output and return early
            // Get summary without consuming the values
            let summary = format!("SC-Graph generated with {} nodes", sc_graph.nodes.len());
            if !cli.quiet {
                println!("SC-Graph summary: {}", summary);
            }
            return OutputManager::handle_file_output(
                &self.scgraph_stage,
                &(cfg_program, sc_graph),
                cli,
            );
        }

        // Stage 5: Verification
        if !cli.quiet {
            println!("🔍 Starting verification process...");
            println!(
                "⚙️ Stage {}/5: {}...",
                self.verification_stage.stage_number(),
                self.verification_stage.name()
            );
        }

        let verification_result = self.verification_stage.execute((cfg_program, sc_graph))?;
        let (_final_cfg, final_scgraph, results) = &verification_result;

        if !cli.quiet {
            let summary = self.verification_stage.get_summary(&verification_result);
            println!(
                "✅ {} completed: {}",
                self.verification_stage.name(),
                summary
            );
        }

        // Handle verification output
        if cli.output_dir.is_some() || cli.output.is_some() {
            OutputManager::handle_directory_output(
                &self.verification_stage,
                &verification_result,
                cli,
            )?;
        }

        // Print detailed results and final state
        print_verification_results(results, cli);
        check_final_state(final_scgraph, cli);

        Ok(())
    }
}
