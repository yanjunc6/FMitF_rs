use crate::cfg::{CfgProgram, FunctionId, Operand, Statement};
use crate::dataflow::{analyze_copies, CopyMapLattice, AnalysisLevel, StmtLoc};
use crate::optimization::OptimizationPass;

/// Copy Propagation optimization pass
/// 
/// Uses copy analysis to replace variable uses with their copy sources.
pub struct CopyPropagation;

impl OptimizationPass for CopyPropagation {
    fn name(&self) -> &'static str {
        "Copy Propagation"
    }

    fn optimize_function(&self, program: &mut CfgProgram, func_id: FunctionId) -> bool {
        let func = match program.functions.get(func_id) {
            Some(f) => f,
            None => return false,
        };

        // Run copy analysis
        let copies = analyze_copies(program, func_id, AnalysisLevel::Function);
        let mut changed = false;

        // Process each block
        for &block_id in &func.blocks {
            if let Some(block) = program.blocks.get_mut(block_id) {
                for (stmt_idx, stmt) in block.statements.iter_mut().enumerate() {
                    let stmt_loc = StmtLoc { block: block_id, index: stmt_idx };
                    
                    if let Some(copy_state) = copies.stmt_entry.get(&stmt_loc) {
                        if self.propagate_copies_in_statement(stmt, copy_state) {
                            changed = true;
                        }
                    }
                }
            }
        }

        changed
    }
}

impl CopyPropagation {
    pub fn new() -> Self {
        Self
    }

    /// Propagate copies in a statement, returning true if changed
    fn propagate_copies_in_statement(
        &self,
        stmt: &mut Statement,
        copies: &CopyMapLattice,
    ) -> bool {
        match stmt {
            Statement::Assign { rvalue, .. } => {
                self.propagate_copies_in_rvalue(rvalue, copies)
            }
        }
    }

    /// Propagate copies in an rvalue, returning true if changed
    fn propagate_copies_in_rvalue(
        &self,
        rvalue: &mut crate::cfg::Rvalue,
        copies: &CopyMapLattice,
    ) -> bool {
        use crate::cfg::Rvalue;
        match rvalue {
            Rvalue::Use(operand) => self.propagate_copies_in_operand(operand, copies),
            Rvalue::TableAccess { pk_values, .. } => {
                let mut changed = false;
                for pk_value in pk_values {
                    if self.propagate_copies_in_operand(pk_value, copies) {
                        changed = true;
                    }
                }
                changed
            }
            Rvalue::ArrayAccess { array, index } => {
                let mut changed = false;
                if self.propagate_copies_in_operand(array, copies) {
                    changed = true;
                }
                if self.propagate_copies_in_operand(index, copies) {
                    changed = true;
                }
                changed
            }
            Rvalue::UnaryOp { operand, .. } => {
                self.propagate_copies_in_operand(operand, copies)
            }
            Rvalue::BinaryOp { left, right, .. } => {
                let mut changed = false;
                if self.propagate_copies_in_operand(left, copies) {
                    changed = true;
                }
                if self.propagate_copies_in_operand(right, copies) {
                    changed = true;
                }
                changed
            }
        }
    }

    /// Propagate copies in an operand, returning true if changed
    fn propagate_copies_in_operand(
        &self,
        operand: &mut Operand,
        copies: &CopyMapLattice,
    ) -> bool {
        match operand {
            Operand::Var(var_id) => {
                if let Some(source) = copies.get_copy_source(*var_id) {
                    // Replace with copy source
                    *operand = Operand::Var(source);
                    true
                } else {
                    false
                }
            }
            Operand::Const(_) => false, // Constants are not copies
        }
    }
}
