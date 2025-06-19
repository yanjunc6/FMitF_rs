// src/cli/pipeline.rs
use super::{output::*, stages::*, traits::*, Cli, Mode, StageContext, Logger};

pub struct Pipeline {
    pub ast_stage: AstStage,
    pub cfg_stage: CfgStage,
    pub optimize_stage: OptimizeStage,
    pub scgraph_stage: ScGraphStage,
    pub verification_stage: VerificationStage,
    pub logger: Logger,
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
                boogie_output_dir: cli.output_dir.clone().or_else(|| cli.output.clone()), // Pass the determined directory to the stage
            },
            logger: Logger::new(cli.verbose, cli.quiet),
        }
    }

    /// Calculate the total number of stages for a given mode
    fn total_stages_for_mode(mode: &Mode) -> usize {
        match mode {
            Mode::Ast => 1,
            Mode::Cfg => 2,
            Mode::Optimize => 3,
            Mode::Runtime => 3,  // AST + CFG + Optimize
            Mode::Scgraph => 4,  // AST + CFG + Optimize + SCGraph
            Mode::Verify => 5,   // AST + CFG + Optimize + SCGraph + Verification
        }
    }

    pub fn execute(
        &mut self,
        source_code: String,
        target_mode: Mode,
        cli: &Cli,
    ) -> Result<(), String> {
        let ctx = StageContext::new(cli).with_source(&source_code);
        let total_stages = Self::total_stages_for_mode(&target_mode);

        // Stage 1: AST
        self.logger.stage_start(
            self.ast_stage.stage_number(),
            total_stages,
            "Frontend Analysis"
        );

        let ast_program = self
            .ast_stage
            .execute(source_code.clone())
            .map_err(|errors| {
                self.logger.stage_error(errors.len());
                for error in &errors {
                    if let Some(source) = ctx.source_code {
                        print_spanned_error(error, source);
                    }
                }
                self.logger.abort_pipeline();
                "AST stage failed".to_string()
            })?;

        self.logger.stage_success();

        if target_mode == Mode::Ast {
            return OutputManager::handle_file_output(&self.ast_stage, &ast_program, cli);
        }

        // Stage 2: CFG
        self.logger.stage_start(
            self.cfg_stage.stage_number(),
            total_stages,
            "Building Control Flow Graph"
        );

        let cfg_program = self.cfg_stage.execute(ast_program).map_err(|e| {
            self.logger.error(&format!("CFG stage failed: {}", e));
            e
        })?;

        self.logger.stage_success();

        if target_mode == Mode::Cfg {
            return OutputManager::handle_file_output(&self.cfg_stage, &cfg_program, cli);
        }

        // Stage 3: Optimization
        self.logger.stage_start(
            self.optimize_stage.stage_number(),
            total_stages,
            "Optimizing Control Flow Graph"
        );

        let optimized_cfg = self.optimize_stage.execute(cfg_program)?;

        if self.optimize_stage.skip_optimization {
            self.logger.stage_skipped("skipped");
        } else {
            self.logger.stage_success();
        }

        if target_mode == Mode::Optimize {
            return OutputManager::handle_file_output(&self.optimize_stage, &optimized_cfg, cli);
        }

        // Runtime mode: Start REPL with the optimized CFG
        if target_mode == Mode::Runtime {
            self.logger.process_start("interactive runtime with optimized CFG");
            crate::runtime::start_runtime_repl_with_cfg(optimized_cfg)?;
            return Ok(());
        }

        // Stage 4: SC-Graph
        self.logger.stage_start(
            self.scgraph_stage.stage_number(),
            total_stages,
            "Building Serializability Conflict Graph"
        );

        let (cfg_program, sc_graph) = self.scgraph_stage.execute(optimized_cfg)?;

        self.logger.stage_success();

        if target_mode == Mode::Scgraph {
            // For Scgraph mode, we need to output and return early
            // Get summary without consuming the values
            let summary = format!("SC-Graph generated with {} nodes", sc_graph.nodes.len());
            self.logger.detail(&summary);
            return OutputManager::handle_file_output(
                &self.scgraph_stage,
                &(cfg_program, sc_graph),
                cli,
            );
        }

        // Stage 5: Verification
        self.logger.process_start("verification process");
        self.logger.stage_start(
            self.verification_stage.stage_number(),
            total_stages,
            "Verification & C-edge Pruning"
        );

        let verification_result = self.verification_stage.execute((cfg_program, sc_graph))?;
        let (_final_cfg, final_scgraph, results) = &verification_result;

        self.logger.stage_success();

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
