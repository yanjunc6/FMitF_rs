//! CFG Optimization Framework
//!
//! This module provides various optimization passes for CFG programs:
//! - Constant propagation and folding
//! - Dead code elimination  
//! - Common subexpression elimination

use crate::cfg::{CfgProgram, FunctionId};
use std::collections::HashMap;

mod common_subexpression_elimination;
mod constant_propagation;
mod dead_code_elimination;

pub use common_subexpression_elimination::CommonSubexpressionEliminationPass;
pub use constant_propagation::ConstantPropagationPass;
pub use dead_code_elimination::DeadCodeEliminationPass;

/// Trait for optimization passes
pub trait OptimizationPass {
    /// Apply the optimization pass to a function
    fn optimize_function(&self, program: &mut CfgProgram, func_id: FunctionId) -> bool;

    /// Get the name of this optimization pass
    fn name(&self) -> &'static str;
}

/// Main optimizer that orchestrates multiple passes
pub struct CfgOptimizer {
    passes: Vec<Box<dyn OptimizationPass>>,
    max_iterations: usize,
}

impl CfgOptimizer {
    pub fn new() -> Self {
        Self {
            passes: Vec::new(),
            max_iterations: 10, // Prevent infinite loops
        }
    }

    /// Add an optimization pass
    pub fn add_pass(mut self, pass: Box<dyn OptimizationPass>) -> Self {
        self.passes.push(pass);
        self
    }

    /// Create a default optimizer with standard passes
    pub fn default_passes() -> Self {
        Self::new()
            .add_pass(Box::new(ConstantPropagationPass::new()))
            .add_pass(Box::new(DeadCodeEliminationPass::new()))
            .add_pass(Box::new(CommonSubexpressionEliminationPass::new()))
    }

    /// Optimize an entire CFG program
    pub fn optimize_program(&self, program: &mut CfgProgram) -> OptimizationResults {
        let mut results = OptimizationResults::new();

        eprintln!("=== OPTIMIZATION DEBUG START ===");
        eprintln!(
            "Starting optimization with {} passes: {:?}",
            self.passes.len(),
            self.passes.iter().map(|p| p.name()).collect::<Vec<_>>()
        );

        let function_ids: Vec<FunctionId> = program.functions.iter().map(|(id, _)| id).collect();

        for func_id in function_ids {
            let func_name = program
                .functions
                .get(func_id)
                .map(|f| f.name.clone())
                .unwrap_or_else(|| format!("func_{:?}", func_id));

            eprintln!(
                "\n--- Optimizing function: {} (id: {:?}) ---",
                func_name, func_id
            );

            // Count statements before optimization
            let initial_stmt_count = self.count_function_statements(program, func_id);
            eprintln!("Initial statement count: {}", initial_stmt_count);

            let func_results = self.optimize_function_by_id(program, func_id);

            // Count statements after optimization
            let final_stmt_count = self.count_function_statements(program, func_id);
            eprintln!(
                "Final statement count: {} (removed: {})",
                final_stmt_count,
                initial_stmt_count.saturating_sub(final_stmt_count)
            );

            results.merge_function_results(func_id, func_results);
        }

        eprintln!("=== OPTIMIZATION DEBUG END ===\n");
        results
    }

    /// Optimize a single function by ID with iterative application of passes
    pub fn optimize_function_by_id(
        &self,
        program: &mut CfgProgram,
        func_id: FunctionId,
    ) -> FunctionOptimizationResults {
        let mut results = FunctionOptimizationResults::new();
        let mut iteration = 0;

        loop {
            if iteration >= self.max_iterations {
                eprintln!(
                    "  WARNING: Reached max iterations ({}) for function {:?}",
                    self.max_iterations, func_id
                );
                break;
            }

            let mut changed_this_iteration = false;
            eprintln!("  Iteration {}: ", iteration);

            for pass in &self.passes {
                let stmt_count_before = self.count_function_statements(program, func_id);
                eprintln!(
                    "    Running pass: {} (statements before: {})",
                    pass.name(),
                    stmt_count_before
                );

                let changed = pass.optimize_function(program, func_id);

                let stmt_count_after = self.count_function_statements(program, func_id);
                let removed_count = stmt_count_before.saturating_sub(stmt_count_after);

                if changed {
                    changed_this_iteration = true;
                    results.record_pass_application(pass.name());
                    eprintln!(
                        "      ✓ CHANGED: {} statements after (removed: {})",
                        stmt_count_after, removed_count
                    );
                } else {
                    eprintln!("      - No change: {} statements", stmt_count_after);
                }
            }

            if !changed_this_iteration {
                eprintln!("  Fixed point reached after {} iterations", iteration);
                break; // Fixed point reached
            }

            iteration += 1;
        }

        results.iterations = iteration;
        results
    }

    /// Count total statements in a function
    fn count_function_statements(&self, program: &CfgProgram, func_id: FunctionId) -> usize {
        program
            .functions
            .get(func_id)
            .map(|func| {
                func.hops
                    .iter()
                    .map(|&hop_id| {
                        let hop = &program.hops[hop_id];
                        hop.blocks
                            .iter()
                            .map(|&block_id| program.blocks[block_id].statements.len())
                            .sum::<usize>()
                    })
                    .sum::<usize>()
            })
            .unwrap_or(0)
    }
}

/// Results of optimization
#[derive(Debug)]
pub struct OptimizationResults {
    pub function_results: HashMap<FunctionId, FunctionOptimizationResults>,
}

#[derive(Debug)]
pub struct FunctionOptimizationResults {
    pub pass_applications: HashMap<String, usize>,
    pub iterations: usize,
}

impl OptimizationResults {
    pub fn new() -> Self {
        Self {
            function_results: HashMap::new(),
        }
    }

    fn merge_function_results(
        &mut self,
        func_id: FunctionId,
        results: FunctionOptimizationResults,
    ) {
        self.function_results.insert(func_id, results);
    }
}

impl FunctionOptimizationResults {
    pub fn new() -> Self {
        Self {
            pass_applications: HashMap::new(),
            iterations: 0,
        }
    }

    fn record_pass_application(&mut self, pass_name: &str) {
        *self
            .pass_applications
            .entry(pass_name.to_string())
            .or_insert(0) += 1;
    }
}
