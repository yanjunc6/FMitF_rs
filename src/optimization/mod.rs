//! CFG Optimization Framework
//!
//! This module provides various optimization passes for CFG programs:
//! - Constant propagation and folding
//! - Dead code elimination  
//! - Common subexpression elimination

use crate::cfg::{CfgProgram, FunctionCfg, FunctionId};
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
    fn optimize_function(&self, func: &mut FunctionCfg) -> bool;

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

        for (func_id, function) in program.functions.iter_mut() {
            let func_results = self.optimize_function(function);
            results.merge_function_results(func_id, func_results);
        }

        results
    }

    /// Optimize a single function with iterative application of passes
    pub fn optimize_function(&self, func: &mut FunctionCfg) -> FunctionOptimizationResults {
        let mut results = FunctionOptimizationResults::new();
        let mut iteration = 0;

        loop {
            if iteration >= self.max_iterations {
                break;
            }

            let mut changed_this_iteration = false;

            for pass in &self.passes {
                let changed = pass.optimize_function(func);
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
