use crate::cfg::{ControlFlowEdge, LValue, Operand, Rvalue, Statement, VarId};
use crate::dataflow::{AnalysisLevel, DataflowResults, Lattice, TransferFunction};
use std::collections::HashMap;

/// Copy lattice value for a single variable
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CopyLattice {
    Bottom,      // Undefined
    Copy(VarId), // Variable holds same value as another variable
    Top,         // Unknown (could be any variable)
}

impl Lattice for CopyLattice {
    fn bottom() -> Option<Self> {
        Some(CopyLattice::Bottom)
    }

    fn top() -> Option<Self> {
        Some(CopyLattice::Top)
    }

    fn meet(&self, other: &Self) -> Self {
        match (self, other) {
            (CopyLattice::Bottom, _) | (_, CopyLattice::Bottom) => CopyLattice::Bottom,
            (CopyLattice::Top, x) | (x, CopyLattice::Top) => x.clone(),
            (CopyLattice::Copy(v1), CopyLattice::Copy(v2)) => {
                if v1 == v2 {
                    CopyLattice::Copy(*v1)
                } else {
                    CopyLattice::Top
                }
            }
        }
    }

    fn join(&self, other: &Self) -> Self {
        match (self, other) {
            (CopyLattice::Top, _) | (_, CopyLattice::Top) => CopyLattice::Top,
            (CopyLattice::Bottom, x) | (x, CopyLattice::Bottom) => x.clone(),
            (CopyLattice::Copy(v1), CopyLattice::Copy(v2)) => {
                if v1 == v2 {
                    CopyLattice::Copy(*v1)
                } else {
                    CopyLattice::Top
                }
            }
        }
    }
}

/// Map lattice for copy analysis (Var -> CopyLattice)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CopyMapLattice {
    map: HashMap<VarId, CopyLattice>,
}

impl CopyMapLattice {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn get(&self, var: VarId) -> CopyLattice {
        self.map.get(&var).cloned().unwrap_or(CopyLattice::Bottom)
    }

    pub fn set(&mut self, var: VarId, value: CopyLattice) {
        match value {
            CopyLattice::Bottom => {
                self.map.remove(&var);
            }
            _ => {
                self.map.insert(var, value);
            }
        }
    }

    pub fn get_copy_source(&self, var: VarId) -> Option<VarId> {
        match self.get(var) {
            CopyLattice::Copy(source) => Some(source),
            _ => None,
        }
    }
}

impl Lattice for CopyMapLattice {
    fn bottom() -> Option<Self> {
        Some(CopyMapLattice::new())
    }

    fn top() -> Option<Self> {
        None // Top is infinite map
    }

    fn meet(&self, other: &Self) -> Self {
        let mut result_map = HashMap::new();

        // Intersection of keys
        for (&var, value1) in &self.map {
            if let Some(value2) = other.map.get(&var) {
                let meet_result = value1.meet(value2);
                if meet_result != CopyLattice::Bottom {
                    result_map.insert(var, meet_result);
                }
            }
        }

        CopyMapLattice { map: result_map }
    }

    fn join(&self, other: &Self) -> Self {
        let mut result_map = HashMap::new();

        // Union of keys
        let all_vars: std::collections::HashSet<VarId> =
            self.map.keys().chain(other.map.keys()).cloned().collect();

        for var in all_vars {
            let value1 = self.get(var);
            let value2 = other.get(var);
            let join_result = value1.join(&value2);
            if join_result != CopyLattice::Bottom {
                result_map.insert(var, join_result);
            }
        }

        CopyMapLattice { map: result_map }
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
            .map
            .iter()
            .filter_map(|(&var, value)| match value {
                CopyLattice::Copy(source) if *source == modified_var => Some(var),
                _ => None,
            })
            .collect();

        for var in vars_to_invalidate {
            state.set(var, CopyLattice::Top);
        }
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
                            new_state.set(*var, CopyLattice::Copy(source_var));
                        } else {
                            new_state.set(*var, CopyLattice::Top);
                        }
                    }
                    LValue::ArrayElement { array, .. } => {
                        // Array element assignment invalidates copies of the array
                        self.invalidate_copies_using_var(&mut new_state, *array);
                        new_state.set(*array, CopyLattice::Top);
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
        CopyMapLattice::new()
    }

    fn boundary_value(&self) -> CopyMapLattice {
        CopyMapLattice::new()
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
