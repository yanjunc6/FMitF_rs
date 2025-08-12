//! CFG Optimization Framework
//!
//! This module provides various optimization passes for CFG programs:
//! - Constant propagation and folding
//! - Dead code elimination  
//! - Copy propagation
//! - Common subexpression elimination

use crate::cfg::{CfgProgram, FunctionId};

mod common_subexpression_elimination;
mod constant_folding;
mod constant_propagation;
mod copy_propagation;
mod dead_code_elimination;

pub use common_subexpression_elimination::CommonSubexpressionEliminationPass;
pub use constant_folding::ConstantFoldingPass;
pub use constant_propagation::ConstantPropagationPass;
pub use copy_propagation::CopyPropagation;
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

    /// Create a default optimizer with standard passes in recommended order
    pub fn default_passes() -> Self {
        Self::new()
            .add_pass(Box::new(ConstantPropagationPass::new()))
            .add_pass(Box::new(ConstantFoldingPass::new()))
            .add_pass(Box::new(CopyPropagation::new()))
            .add_pass(Box::new(CommonSubexpressionEliminationPass::new()))
            .add_pass(Box::new(DeadCodeEliminationPass::new()))
    }

    /// Optimize an entire CFG program
    pub fn optimize_program(&self, program: &mut CfgProgram) -> OptimizationResults {
        let mut results = OptimizationResults::new();
        let function_ids: Vec<FunctionId> = program.functions.iter().map(|(id, _)| id).collect();

        for func_id in function_ids {
            let func_results = self.optimize_function_by_id(program, func_id);
            results.merge_function_results(func_id, func_results);
        }

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
                break;
            }

            let mut changed_this_iteration = false;

            for pass in &self.passes {
                let changed = pass.optimize_function(program, func_id);
                if changed {
                    changed_this_iteration = true;
                    results.record_pass_application(pass.name());
                }
            }

            if !changed_this_iteration {
                break; // Fixed point reached
            }

            iteration += 1;
        }

        results.iterations = iteration;
        results
    }
}

/// Results of optimization
#[derive(Debug)]
pub struct OptimizationResults {
    pub function_results: std::collections::HashMap<FunctionId, FunctionOptimizationResults>,
}

#[derive(Debug)]
pub struct FunctionOptimizationResults {
    pub pass_applications: std::collections::HashMap<String, usize>,
    pub iterations: usize,
}

impl OptimizationResults {
    pub fn new() -> Self {
        Self {
            function_results: std::collections::HashMap::new(),
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
            pass_applications: std::collections::HashMap::new(),
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
