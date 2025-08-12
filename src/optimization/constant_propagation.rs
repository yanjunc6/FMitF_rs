use crate::cfg::{CfgProgram, FunctionId, Operand, Statement};
use crate::dataflow::{analyze_constants, AnalysisLevel, ConstantMapLattice, StmtLoc};
use crate::optimization::OptimizationPass;

/// Constant Propagation optimization pass
///
/// Uses constant analysis to replace variable uses with their constant values.
pub struct ConstantPropagationPass;

impl OptimizationPass for ConstantPropagationPass {
    fn name(&self) -> &'static str {
        "Constant Propagation"
    }

    fn optimize_function(&self, program: &mut CfgProgram, func_id: FunctionId) -> bool {
        let func = match program.functions.get(func_id) {
            Some(f) => f,
            None => return false,
        };

        // Run constant analysis
        let constants = analyze_constants(program, func_id, AnalysisLevel::Function);
        let mut changed = false;

        // Process each block
        for &block_id in &func.blocks {
            if let Some(block) = program.blocks.get_mut(block_id) {
                for (stmt_idx, stmt) in block.statements.iter_mut().enumerate() {
                    let stmt_loc = StmtLoc {
                        block: block_id,
                        index: stmt_idx,
                    };

                    if let Some(const_state) = constants.stmt_entry.get(&stmt_loc) {
                        if self.propagate_constants_in_statement(stmt, const_state) {
                            changed = true;
                        }
                    }
                }
            }
        }

        changed
    }
}

impl ConstantPropagationPass {
    pub fn new() -> Self {
        Self
    }

    /// Propagate constants in a statement, returning true if changed
    fn propagate_constants_in_statement(
        &self,
        stmt: &mut Statement,
        constants: &ConstantMapLattice,
    ) -> bool {
        match stmt {
            Statement::Assign { rvalue, .. } => {
                self.propagate_constants_in_rvalue(rvalue, constants)
            }
        }
    }

    /// Propagate constants in an rvalue, returning true if changed
    fn propagate_constants_in_rvalue(
        &self,
        rvalue: &mut crate::cfg::Rvalue,
        constants: &ConstantMapLattice,
    ) -> bool {
        use crate::cfg::Rvalue;
        match rvalue {
            Rvalue::Use(operand) => self.propagate_constants_in_operand(operand, constants),
            Rvalue::TableAccess { pk_values, .. } => {
                let mut changed = false;
                for pk_value in pk_values {
                    if self.propagate_constants_in_operand(pk_value, constants) {
                        changed = true;
                    }
                }
                changed
            }
            Rvalue::ArrayAccess { array, index } => {
                let mut changed = false;
                if self.propagate_constants_in_operand(array, constants) {
                    changed = true;
                }
                if self.propagate_constants_in_operand(index, constants) {
                    changed = true;
                }
                changed
            }
            Rvalue::UnaryOp { operand, .. } => {
                self.propagate_constants_in_operand(operand, constants)
            }
            Rvalue::BinaryOp { left, right, .. } => {
                let mut changed = false;
                if self.propagate_constants_in_operand(left, constants) {
                    changed = true;
                }
                if self.propagate_constants_in_operand(right, constants) {
                    changed = true;
                }
                changed
            }
        }
    }

    /// Propagate constants in an operand, returning true if changed
    fn propagate_constants_in_operand(
        &self,
        operand: &mut Operand,
        constants: &ConstantMapLattice,
    ) -> bool {
        match operand {
            Operand::Var(var_id) => {
                // Check if this variable has a known constant value
                match constants.get(*var_id) {
                    crate::dataflow::ConstantState::Constant(constant) => {
                        // Replace variable with constant
                        *operand = Operand::Const(constant);
                        true
                    }
                    _ => false, // Not a constant or unknown
                }
            }
            Operand::Const(_) => false, // Already a constant
        }
    }
}
