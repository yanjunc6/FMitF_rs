use super::{
    AnalysisKind, AnalysisLevel, DataflowAnalysis, DataflowResults, Direction, Flat, Lattice,
    MapLattice, StmtLoc, TransferFunction,
};
use crate::cfg::{
    BasicBlock, ControlFlowEdge, FunctionCfg, LValue, Operand, RValue, Statement, VarId,
};

/// Transfer function for copy propagation analysis
pub struct CopyTransfer;

impl TransferFunction<MapLattice<VarId, VarId>> for CopyTransfer {
    /// For each statement, update copy relations
    fn transfer_statement(
        &self,
        stmt: &Statement,
        _stmt_loc: StmtLoc,
        state: &MapLattice<VarId, VarId>,
    ) -> MapLattice<VarId, VarId> {
        if state.is_top() {
            return Lattice::top().unwrap();
        }

        let mut result = state.clone();

        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => {
                match lvalue {
                    LValue::Variable { var } => {
                        // Kill: remove any copy relation for this variable
                        result.insert(*var, Flat::Bottom);

                        // Gen: If rvalue is a simple variable use, add copy relation
                        if let RValue::Use(Operand::Var(source_var)) = rvalue {
                            result.insert(*var, Flat::Value(*source_var));
                        }
                    }
                    LValue::ArrayElement { array, .. } => {
                        // Array element assignment kills copy relations involving the array
                        result.insert(*array, Flat::Bottom);
                    }
                    LValue::TableField { .. } => {
                        // Table field assignments don't affect local copy relations
                    }
                }
            }
        }

        result
    }

    fn transfer_edge(
        &self,
        _edge: &ControlFlowEdge,
        state: &MapLattice<VarId, VarId>,
    ) -> MapLattice<VarId, VarId> {
        state.clone()
    }

    fn initial_value(&self) -> MapLattice<VarId, VarId> {
        Lattice::bottom().unwrap()
    }

    fn boundary_value(&self, _func: &FunctionCfg, _block: &BasicBlock) -> MapLattice<VarId, VarId> {
        Lattice::bottom().unwrap()
    }
}

/// Analyze copy relations in a function (forward, function-level)
/// Maps each variable to the single variable it copies from (if any)
pub fn analyze_copies(
    func: &FunctionCfg,
    cfg_program: &crate::cfg::CfgProgram,
) -> DataflowResults<MapLattice<VarId, VarId>> {
    let analysis = DataflowAnalysis::new(
        AnalysisLevel::Function,
        Direction::Forward,
        AnalysisKind::Must,
        CopyTransfer,
    );
    analysis.analyze(func, cfg_program)
}
