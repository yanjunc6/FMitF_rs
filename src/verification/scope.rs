// In a central location, e.g., src/verification/scope.rs

use crate::cfg::{BasicBlockId, Program as CfgProgram, VariableId as VarId};
use std::collections::HashMap;

pub type SliceId = usize;

/// A comprehensive naming authority for generating collision-free identifiers.
#[derive(Clone, Debug, Default)]
pub struct ExecutionScope {
    /// The current slice being processed, e.g., `0` for slice "A", `1` for slice "B".
    current_slice_id: Option<SliceId>,

    // --- Caches to improve performance ---
    var_name_cache: HashMap<(VarId, SliceId), String>,
    label_cache: HashMap<(BasicBlockId, SliceId), String>,
}

impl ExecutionScope {
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the context to a specific slice. Called by the verification walker.
    pub fn set_current_slice(&mut self, slice_id: SliceId) {
        self.current_slice_id = Some(slice_id);
    }

    pub fn clear_current_slice(&mut self) {
        self.current_slice_id = None;
    }

    fn get_current_slice_id(&self) -> SliceId {
        self.current_slice_id
            .expect("ExecutionScope: current_slice_id is not set")
    }

    // --- THE NAMING METHODS ---

    /// **Generates a unique name for a CFG variable within the current slice.**
    /// Example: For a variable named `temp`, it returns `s0_temp` or `s1_temp`.
    pub fn get_scoped_variable_name(&mut self, cfg_program: &CfgProgram, var_id: VarId) -> String {
        let slice_id = self.get_current_slice_id();
        if let Some(name) = self.var_name_cache.get(&(var_id, slice_id)) {
            return name.clone();
        }

        let var = &cfg_program.variables[var_id];
        // The core naming convention
        let name = format!("s{}_{}", slice_id, var.name);
        self.var_name_cache.insert((var_id, slice_id), name.clone());
        name
    }

    /// **Generates a unique label for a basic block within the current slice.**
    /// Example: For BasicBlockId 15, it returns `s0_block15` or `s1_block15`.
    pub fn get_scoped_label(&mut self, block_id: BasicBlockId) -> String {
        let slice_id = self.get_current_slice_id();
        if let Some(label) = self.label_cache.get(&(block_id, slice_id)) {
            return label.clone();
        }

        // The core naming convention for labels
        let label = format!("s{}_block{}", slice_id, block_id.index());
        self.label_cache.insert((block_id, slice_id), label.clone());
        label
    }

    /// **Generates other unique names on the fly.**
    /// Useful for temporary snapshot variables, etc.
    /// Example: `get_unique_name("final_state")` -> `s0_final_state`
    pub fn get_scoped_name(&self, base_name: &str) -> String {
        let slice_id = self.get_current_slice_id();
        format!("s{}_{}", slice_id, base_name)
    }
}
