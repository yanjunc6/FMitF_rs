use crate::cfg::{ControlFlowEdge, LValue, Operand, Rvalue, Statement, VarId};
use crate::dataflow::{AnalysisLevel, DataflowResults, SetLattice, TransferFunction};
use std::collections::HashMap;

/// Copy data for a single variable
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CopyMapData {
    pub var: VarId,
    pub state: CopyState,
}

/// States for copy analysis
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CopyState {
    Bottom,      // Undefined
    Copy(VarId), // Variable holds same value as another variable
    Top,         // Unknown (could be any variable)
}

impl CopyMapData {
    pub fn new(var: VarId, state: CopyState) -> Self {
        Self { var, state }
    }

    pub fn bottom(var: VarId) -> Self {
        Self::new(var, CopyState::Bottom)
    }

    pub fn copy(var: VarId, source: VarId) -> Self {
        Self::new(var, CopyState::Copy(source))
    }

    pub fn top(var: VarId) -> Self {
        Self::new(var, CopyState::Top)
    }
}

/// Copy analysis using SetLattice
pub type CopyMapLattice = SetLattice<CopyMapData>;

impl CopyMapLattice {
    pub fn get(&self, var: VarId) -> CopyState {
        // Find the entry for this variable
        for entry in &self.set {
            if entry.var == var {
                return entry.state.clone();
            }
        }
        CopyState::Bottom
    }

    pub fn set(&mut self, var: VarId, state: CopyState) {
        // Remove any existing entry for this variable
        self.set.retain(|entry| entry.var != var);

        // Add the new entry
        self.set.insert(CopyMapData::new(var, state));
    }

    pub fn get_copy_source(&self, var: VarId) -> Option<VarId> {
        match self.get(var) {
            CopyState::Copy(source) => Some(source),
            _ => None,
        }
    }
}

/// Transfer function for copy analysis
pub struct CopyAnalysisTransfer;

impl CopyAnalysisTransfer {
    fn is_copy_assignment(&self, rvalue: &Rvalue) -> Option<VarId> {
        match rvalue {
            Rvalue::Use(Operand::Var(var_id)) => Some(*var_id),
            _ => None,
        }
    }

    fn invalidate_copies_using_var(&self, state: &mut CopyMapLattice, modified_var: VarId) {
        // Remove any copies that point to the modified variable
        let vars_to_invalidate: Vec<VarId> = state
            .set
            .iter()
            .filter_map(|entry| match &entry.state {
                CopyState::Copy(source) if *source == modified_var => Some(entry.var),
                _ => None,
            })
            .collect();

        for var in vars_to_invalidate {
            self.set(state, var, CopyState::Top);
        }
    }

    fn set(&self, state: &mut CopyMapLattice, var: VarId, new_state: CopyState) {
        state.set(var, new_state);
    }
}

impl TransferFunction<CopyMapLattice> for CopyAnalysisTransfer {
    fn transfer_statement(&self, stmt: &Statement, state: &CopyMapLattice) -> CopyMapLattice {
        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => {
                let mut new_state = state.clone();
                match lvalue {
                    LValue::Variable { var } => {
                        // Kill any existing copy information for this variable
                        self.invalidate_copies_using_var(&mut new_state, *var);

                        // Check if this is a copy assignment
                        if let Some(source_var) = self.is_copy_assignment(rvalue) {
                            self.set(&mut new_state, *var, CopyState::Copy(source_var));
                        } else {
                            self.set(&mut new_state, *var, CopyState::Top);
                        }
                    }
                    LValue::ArrayElement { array, .. } => {
                        // Array element assignment invalidates copies of the array
                        self.invalidate_copies_using_var(&mut new_state, *array);
                        self.set(&mut new_state, *array, CopyState::Top);
                    }
                    LValue::TableField { .. } => {
                        // Table field assignments don't affect local variable copies
                    }
                }
                new_state
            }
        }
    }

    fn transfer_edge(&self, _edge: &ControlFlowEdge, state: &CopyMapLattice) -> CopyMapLattice {
        state.clone()
    }

    fn initial_value(&self) -> CopyMapLattice {
        SetLattice::new(std::collections::HashSet::new())
    }

    fn boundary_value(&self) -> CopyMapLattice {
        SetLattice::new(std::collections::HashSet::new())
    }
}

/// Run copy analysis on a function
pub fn analyze_copies(
    prog: &crate::cfg::CfgProgram,
    func_id: crate::cfg::FunctionId,
    level: AnalysisLevel,
) -> DataflowResults<CopyMapLattice> {
    use crate::dataflow::{DataflowAnalysis, Direction};

    if let Some(func) = prog.functions.get(func_id) {
        let analysis = DataflowAnalysis::new(level, Direction::Forward, CopyAnalysisTransfer);
        analysis.analyze(func, prog)
    } else {
        DataflowResults {
            block_entry: HashMap::new(),
            block_exit: HashMap::new(),
            stmt_entry: HashMap::new(),
            stmt_exit: HashMap::new(),
        }
    }
}
