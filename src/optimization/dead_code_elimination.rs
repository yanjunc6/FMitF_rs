use crate::cfg::{FunctionCfg, BasicBlockId, Statement, VarId, Operand};
use crate::dataflow::{analyze_live_variables, DataflowResults, SetLattice};
use crate::optimization::OptimizationPass;
use std::collections::HashSet;

pub struct DeadCodeEliminationPass {
    // Configuration options could go here
}

impl DeadCodeEliminationPass {
    pub fn new() -> Self {
        Self {}
    }
}

impl OptimizationPass for DeadCodeEliminationPass {
    fn name(&self) -> &'static str {
        "dead_code_elimination"
    }
    
    fn optimize_function(&self, func: &mut FunctionCfg) -> bool {
        // Run live variables analysis
        let liveness_results = analyze_live_variables(func);
        
        let mut changed = false;
        
        // Process each block
        for (block_id, block) in func.blocks.iter_mut() {
            // Create a default empty set to avoid lifetime issues
            let empty_set = HashSet::new();
            let live_vars_at_exit = liveness_results.exit.get(&block_id)
                .map(|s| &s.set)
                .unwrap_or(&empty_set);
            
            // Simulate backward liveness for this block
            let mut current_live = live_vars_at_exit.clone();
            
            // Process statements in reverse order
            let mut statements_to_keep = Vec::new();
            
            for stmt in block.statements.iter().rev() {
                match stmt {
                    Statement::Assign { var, rvalue, .. } => {
                        // Check if the assigned variable is live
                        if current_live.contains(var) {
                            // Keep this statement
                            statements_to_keep.push(stmt.clone());
                            
                            // Remove the defined variable from live set
                            current_live.remove(var);
                            
                            // Add used variables to live set
                            self.add_used_vars_from_rvalue(rvalue, &mut current_live);
                        } else {
                            // This is a dead assignment, remove it
                            changed = true;
                            // Don't add to statements_to_keep
                        }
                    }
                    Statement::TableAssign { pk_value, value, .. } => {
                        // Table assignments have side effects, always keep them
                        statements_to_keep.push(stmt.clone());
                        
                        // Add used variables to live set
                        if let Operand::Var(var_id) = pk_value {
                            current_live.insert(*var_id);
                        }
                        if let Operand::Var(var_id) = value {
                            current_live.insert(*var_id);
                        }
                    }
                }
            }
            
            // Reverse to get original order
            statements_to_keep.reverse();
            
            if statements_to_keep.len() != block.statements.len() {
                block.statements = statements_to_keep;
                changed = true;
            }
        }
        
        changed
    }
}

impl DeadCodeEliminationPass {
    fn add_used_vars_from_rvalue(&self, rvalue: &crate::cfg::Rvalue, live_vars: &mut HashSet<VarId>) {
        use crate::cfg::Rvalue;
        
        match rvalue {
            Rvalue::Use(operand) => {
                if let Operand::Var(var_id) = operand {
                    live_vars.insert(*var_id);
                }
            }
            Rvalue::UnaryOp { operand, .. } => {
                if let Operand::Var(var_id) = operand {
                    live_vars.insert(*var_id);
                }
            }
            Rvalue::BinaryOp { left, right, .. } => {
                if let Operand::Var(var_id) = left {
                    live_vars.insert(*var_id);
                }
                if let Operand::Var(var_id) = right {
                    live_vars.insert(*var_id);
                }
            }
            Rvalue::TableAccess { pk_value, .. } => {
                if let Operand::Var(var_id) = pk_value {
                    live_vars.insert(*var_id);
                }
            }
        }
    }
}