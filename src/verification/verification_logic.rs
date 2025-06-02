use crate::cfg::{CfgProgram, FunctionId, HopId, NodeId, TableId, TypeName, VarId};
use crate::verification::VerificationUnit;
use std::collections::HashMap;

/// Represents the logical structure of a verification scenario
#[derive(Debug, Clone)]
pub struct VerificationPlan {
    pub func1_id: FunctionId,
    pub func2_id: FunctionId,
    pub relevant_hops_f1: Vec<HopId>,
    pub relevant_hops_f2: Vec<HopId>,
    pub global_vars: HashMap<VarId, (FunctionId, TypeName)>,
    pub relevant_tables: Vec<TableId>,
    pub interleavings: Vec<Vec<(FunctionId, HopId)>>,
    pub serial_orders: [(FunctionId, Vec<HopId>, FunctionId, Vec<HopId>); 2],
}

impl VerificationPlan {
    /// Build a verification plan from a verification unit
    pub fn from_verification_unit(unit: &VerificationUnit, cfg: &CfgProgram) -> Self {
        let func1_cfg = &cfg.functions[unit.func1_id];
        let func2_cfg = &cfg.functions[unit.func2_id];

        // 1. Collect relevant variables
        let mut global_vars = HashMap::new();

        // Add all variables from both functions
        for (var_id, variable) in func1_cfg.variables.iter() {
            global_vars.insert(var_id, (unit.func1_id, variable.ty.clone()));
        }
        for (var_id, variable) in func2_cfg.variables.iter() {
            global_vars
                .entry(var_id)
                .or_insert_with(|| (unit.func2_id, variable.ty.clone()));
        }

        // Add live variables from the unit
        for var_id in &unit.live_vars_at_hop1_exit {
            if let Some(var_info) = func1_cfg.variables.get(*var_id) {
                global_vars.insert(*var_id, (unit.func1_id, var_info.ty.clone()));
            }
        }
        for var_id in &unit.live_vars_at_hop2_exit {
            if let Some(var_info) = func2_cfg.variables.get(*var_id) {
                global_vars.insert(*var_id, (unit.func2_id, var_info.ty.clone()));
            }
        }

        // 2. Get relevant hops
        let relevant_hops_f1 =
            get_relevant_hops_on_node(func1_cfg, unit.hop1_id, unit.conflict_node);
        let relevant_hops_f2 =
            get_relevant_hops_on_node(func2_cfg, unit.hop2_id, unit.conflict_node);

        // 3. Generate interleavings
        let f1_markers: Vec<_> = relevant_hops_f1
            .iter()
            .map(|&h| (unit.func1_id, h))
            .collect();
        let f2_markers: Vec<_> = relevant_hops_f2
            .iter()
            .map(|&h| (unit.func2_id, h))
            .collect();
        let interleavings = generate_all_interleavings(&f1_markers, &f2_markers);

        // 4. Define serial orders
        let serial_orders = [
            (
                unit.func1_id,
                relevant_hops_f1.clone(),
                unit.func2_id,
                relevant_hops_f2.clone(),
            ),
            (
                unit.func2_id,
                relevant_hops_f2.clone(),
                unit.func1_id,
                relevant_hops_f1.clone(),
            ),
        ];

        Self {
            func1_id: unit.func1_id,
            func2_id: unit.func2_id,
            relevant_hops_f1,
            relevant_hops_f2,
            global_vars,
            relevant_tables: unit.relevant_tables.clone(),
            interleavings,
            serial_orders,
        }
    }
}

fn get_relevant_hops_on_node(
    func_cfg: &crate::cfg::FunctionCfg,
    target_hop_id: HopId,
    conflict_node_id: NodeId,
) -> Vec<HopId> {
    let mut relevant_hops = Vec::new();

    // Special case: single hop
    if func_cfg.hop_order.len() == 1 {
        if let Some(hop_id) = func_cfg.hop_order.first() {
            if let Some(hop_cfg) = func_cfg.hops.get(*hop_id) {
                if hop_cfg.node_id == conflict_node_id {
                    relevant_hops.push(*hop_id);
                }
            }
        }
        return relevant_hops;
    }

    // Collect hops on conflict node up to target hop
    for &hop_id in &func_cfg.hop_order {
        if let Some(hop_cfg) = func_cfg.hops.get(hop_id) {
            if hop_cfg.node_id == conflict_node_id {
                relevant_hops.push(hop_id);
            }
        }
        if hop_id == target_hop_id {
            break;
        }
    }

    // Ensure target hop is included if it's on the conflict node
    if !relevant_hops.contains(&target_hop_id) {
        if let Some(hop_cfg) = func_cfg.hops.get(target_hop_id) {
            if hop_cfg.node_id == conflict_node_id {
                relevant_hops.push(target_hop_id);
            }
        }
    }

    relevant_hops
}

fn generate_all_interleavings(
    seq1: &[(FunctionId, HopId)],
    seq2: &[(FunctionId, HopId)],
) -> Vec<Vec<(FunctionId, HopId)>> {
    let mut result = Vec::new();
    let mut current = Vec::new();
    generate_interleavings_recursive(seq1, 0, seq2, 0, &mut current, &mut result);
    result
}

fn generate_interleavings_recursive(
    seq1: &[(FunctionId, HopId)],
    idx1: usize,
    seq2: &[(FunctionId, HopId)],
    idx2: usize,
    current: &mut Vec<(FunctionId, HopId)>,
    all_interleavings: &mut Vec<Vec<(FunctionId, HopId)>>,
) {
    if idx1 == seq1.len() && idx2 == seq2.len() {
        if !current.is_empty() {
            all_interleavings.push(current.clone());
        }
        return;
    }

    if idx1 < seq1.len() {
        current.push(seq1[idx1]);
        generate_interleavings_recursive(seq1, idx1 + 1, seq2, idx2, current, all_interleavings);
        current.pop();
    }

    if idx2 < seq2.len() {
        current.push(seq2[idx2]);
        generate_interleavings_recursive(seq1, idx1, seq2, idx2 + 1, current, all_interleavings);
        current.pop();
    }
}
