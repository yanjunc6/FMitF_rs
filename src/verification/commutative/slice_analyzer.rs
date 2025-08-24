use super::CommutativeUnit;
use crate::cfg::{CfgProgram, HopId, VarId};
use crate::dataflow::{
    analyze_live_variables, analyze_table_mod_ref, AccessType, LiveVar, TableAccess,
};
use crate::verification::errors::Results;
use crate::verification::Boogie::gen_Boogie::BoogieProgramGenerator;
use crate::verification::Boogie::BoogieType;
use std::collections::{HashMap, HashSet};

/// Analysis results for slice commutativity verification
pub struct SliceAnalysisInfo {
    /// Variables live-in to slice A
    pub live_in_a: HashSet<VarId>,
    /// Variables live-out from slice A
    pub live_out_a: HashSet<VarId>,
    /// Variables live-in to slice B
    pub live_in_b: HashSet<VarId>,
    /// Variables live-out from slice B
    pub live_out_b: HashSet<VarId>,
    /// Tables read by slice A
    pub tables_read_a: HashSet<String>,
    /// Tables written by slice A
    pub tables_written_a: HashSet<String>,
    /// Tables read by slice B
    pub tables_read_b: HashSet<String>,
    /// Tables written by slice B
    pub tables_written_b: HashSet<String>,
    /// Tables written by last hop of slice A
    pub tables_written_last_hop_a: HashSet<String>,
    /// Tables written by last hop of slice B
    pub tables_written_last_hop_b: HashSet<String>,
    /// Table variable name to type mapping for easy type resolution
    pub table_var_types: HashMap<String, BoogieType>,
}

pub struct SliceAnalyzer;

impl SliceAnalyzer {
    pub fn new() -> Self {
        SliceAnalyzer
    }

    /// Analyze liveness and table access for both slices
    pub fn analyze_slice_info(
        &self,
        cfg_program: &CfgProgram,
        unit: &CommutativeUnit,
    ) -> Results<SliceAnalysisInfo> {
        // Helper function to collect live variables from entry block
        fn collect_live_vars_entry(
            liveness_results: &crate::dataflow::DataflowResults<
                crate::dataflow::SetLattice<LiveVar>,
            >,
            entry_block: crate::cfg::BasicBlockId,
        ) -> HashSet<VarId> {
            let mut vars = HashSet::new();
            if let Some(live_vars) = liveness_results.block_entry.get(&entry_block) {
                if let Some(var_set) = live_vars.as_set() {
                    for LiveVar(var_id) in var_set {
                        vars.insert(*var_id);
                    }
                }
            }
            vars
        }

        // Helper function to collect live variables from exit blocks
        fn collect_live_vars_exit(
            liveness_results: &crate::dataflow::DataflowResults<
                crate::dataflow::SetLattice<LiveVar>,
            >,
            cfg_program: &CfgProgram,
            hop_id: HopId,
        ) -> HashSet<VarId> {
            let mut vars = HashSet::new();
            let hop = &cfg_program.hops[hop_id];

            for &block_id in &hop.blocks {
                let block = &cfg_program.blocks[block_id];
                for edge in &block.successors {
                    match &edge.edge_type {
                        crate::cfg::EdgeType::HopExit { .. }
                        | crate::cfg::EdgeType::Return { .. }
                        | crate::cfg::EdgeType::Abort => {
                            if let Some(live_vars) = liveness_results.block_exit.get(&block_id) {
                                if let Some(var_set) = live_vars.as_set() {
                                    for LiveVar(var_id) in var_set {
                                        vars.insert(*var_id);
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
            vars
        }

        // Helper function to collect table accesses from slice
        fn collect_table_accesses(
            table_analysis: &crate::dataflow::DataflowResults<
                crate::dataflow::SetLattice<TableAccess>,
            >,
            cfg_program: &CfgProgram,
            slice: &[HopId],
        ) -> (HashSet<TableAccess>, HashSet<TableAccess>) {
            let mut reads = HashSet::new();
            let mut writes = HashSet::new();

            for &hop_id in slice {
                let hop = &cfg_program.hops[hop_id];
                for &block_id in &hop.blocks {
                    if let Some(block_exit) = table_analysis.block_exit.get(&block_id) {
                        if let Some(access_set) = block_exit.as_set() {
                            for table_access in access_set {
                                match table_access.access_type {
                                    AccessType::Read => {
                                        reads.insert(table_access.clone());
                                    }
                                    AccessType::Write => {
                                        writes.insert(table_access.clone());
                                    }
                                }
                            }
                        }
                    }
                }
            }
            (reads, writes)
        }

        // Helper function to collect table accesses from last hop only
        fn collect_last_hop_writes(
            table_analysis: &crate::dataflow::DataflowResults<
                crate::dataflow::SetLattice<TableAccess>,
            >,
            cfg_program: &CfgProgram,
            hop_id: HopId,
        ) -> HashSet<TableAccess> {
            let mut writes = HashSet::new();
            let hop = &cfg_program.hops[hop_id];

            for &block_id in &hop.blocks {
                if let Some(block_exit) = table_analysis.block_exit.get(&block_id) {
                    if let Some(access_set) = block_exit.as_set() {
                        for table_access in access_set {
                            if let AccessType::Write = table_access.access_type {
                                writes.insert(table_access.clone());
                            }
                        }
                    }
                }
            }
            writes
        }

        let slice_a = &unit.hops_A;
        let slice_b = &unit.hops_B;
        let func_a = &cfg_program.functions[unit.func_id_A];
        let func_b = &cfg_program.functions[unit.func_id_B];

        let mut live_in_a = HashSet::new();
        let mut live_out_a = HashSet::new();
        let mut live_in_b = HashSet::new();
        let mut live_out_b = HashSet::new();

        // Analyze liveness for function A
        let liveness_results_a = analyze_live_variables(func_a, cfg_program);

        // Get entry and exit blocks for slice A - use entry_block, not first block
        if let Some(&first_hop_a) = slice_a.first() {
            let first_hop_cfg_a = &cfg_program.hops[first_hop_a];
            if let Some(entry_block_a) = first_hop_cfg_a.entry_block {
                live_in_a = collect_live_vars_entry(&liveness_results_a, entry_block_a);
            }
        }

        if let Some(&last_hop_a) = slice_a.last() {
            live_out_a = collect_live_vars_exit(&liveness_results_a, cfg_program, last_hop_a);
        }

        // Analyze liveness for function B (if different from A)
        if unit.func_id_A != unit.func_id_B {
            let liveness_results_b = analyze_live_variables(func_b, cfg_program);

            if let Some(&first_hop_b) = slice_b.first() {
                let first_hop_cfg_b = &cfg_program.hops[first_hop_b];
                if let Some(entry_block_b) = first_hop_cfg_b.entry_block {
                    live_in_b = collect_live_vars_entry(&liveness_results_b, entry_block_b);
                }
            }

            if let Some(&last_hop_b) = slice_b.last() {
                live_out_b = collect_live_vars_exit(&liveness_results_b, cfg_program, last_hop_b);
            }
        } else {
            // Same function, use the same liveness results
            if let Some(&first_hop_b) = slice_b.first() {
                let first_hop_cfg_b = &cfg_program.hops[first_hop_b];
                if let Some(entry_block_b) = first_hop_cfg_b.entry_block {
                    live_in_b = collect_live_vars_entry(&liveness_results_a, entry_block_b);
                }
            }

            if let Some(&last_hop_b) = slice_b.last() {
                live_out_b = collect_live_vars_exit(&liveness_results_a, cfg_program, last_hop_b);
            }
        }

        // Analyze table access for both slices
        let table_analysis_a = analyze_table_mod_ref(func_a, cfg_program);
        let (table_reads_a, table_writes_a) =
            collect_table_accesses(&table_analysis_a, cfg_program, slice_a);

        let table_writes_last_hop_a = if let Some(&last_hop_a) = slice_a.last() {
            collect_last_hop_writes(&table_analysis_a, cfg_program, last_hop_a)
        } else {
            HashSet::new()
        };

        // Analyze table access for slice B
        let table_analysis_b = if unit.func_id_A != unit.func_id_B {
            analyze_table_mod_ref(func_b, cfg_program)
        } else {
            table_analysis_a
        };

        let (table_reads_b, table_writes_b) =
            collect_table_accesses(&table_analysis_b, cfg_program, slice_b);

        let table_writes_last_hop_b = if let Some(&last_hop_b) = slice_b.last() {
            collect_last_hop_writes(&table_analysis_b, cfg_program, last_hop_b)
        } else {
            HashSet::new()
        };

        // Convert TableAccess to strings using gen_table_field_var_name
        let mut tables_read_a = HashSet::new();
        let mut tables_written_a = HashSet::new();
        let mut tables_written_last_hop_a_strings = HashSet::new();

        // Build table variable name to type mapping directly
        let mut table_var_types = HashMap::new();

        for access in table_reads_a {
            let table = &cfg_program.tables[access.table];
            let field = &cfg_program.fields[access.field];
            let var_name =
                BoogieProgramGenerator::gen_table_field_var_name(&table.name, &field.name);
            tables_read_a.insert(var_name.clone());
            let var_type = BoogieProgramGenerator::gen_table_field_type(
                cfg_program,
                access.table,
                access.field,
            );
            table_var_types.insert(var_name, var_type);
        }

        for access in table_writes_a {
            let table = &cfg_program.tables[access.table];
            let field = &cfg_program.fields[access.field];
            let var_name =
                BoogieProgramGenerator::gen_table_field_var_name(&table.name, &field.name);
            tables_written_a.insert(var_name.clone());
            let var_type = BoogieProgramGenerator::gen_table_field_type(
                cfg_program,
                access.table,
                access.field,
            );
            table_var_types.insert(var_name, var_type);
        }

        for access in table_writes_last_hop_a {
            let table = &cfg_program.tables[access.table];
            let field = &cfg_program.fields[access.field];
            let var_name =
                BoogieProgramGenerator::gen_table_field_var_name(&table.name, &field.name);
            tables_written_last_hop_a_strings.insert(var_name.clone());
            let var_type = BoogieProgramGenerator::gen_table_field_type(
                cfg_program,
                access.table,
                access.field,
            );
            table_var_types.insert(var_name, var_type);
        }

        let mut tables_read_b = HashSet::new();
        let mut tables_written_b = HashSet::new();
        let mut tables_written_last_hop_b_strings = HashSet::new();

        for access in table_reads_b {
            let table = &cfg_program.tables[access.table];
            let field = &cfg_program.fields[access.field];
            let var_name =
                BoogieProgramGenerator::gen_table_field_var_name(&table.name, &field.name);
            tables_read_b.insert(var_name.clone());
            let var_type = BoogieProgramGenerator::gen_table_field_type(
                cfg_program,
                access.table,
                access.field,
            );
            table_var_types.insert(var_name, var_type);
        }

        for access in table_writes_b {
            let table = &cfg_program.tables[access.table];
            let field = &cfg_program.fields[access.field];
            let var_name =
                BoogieProgramGenerator::gen_table_field_var_name(&table.name, &field.name);
            tables_written_b.insert(var_name.clone());
            let var_type = BoogieProgramGenerator::gen_table_field_type(
                cfg_program,
                access.table,
                access.field,
            );
            table_var_types.insert(var_name, var_type);
        }

        for access in table_writes_last_hop_b {
            let table = &cfg_program.tables[access.table];
            let field = &cfg_program.fields[access.field];
            let var_name =
                BoogieProgramGenerator::gen_table_field_var_name(&table.name, &field.name);
            tables_written_last_hop_b_strings.insert(var_name.clone());
            let var_type = BoogieProgramGenerator::gen_table_field_type(
                cfg_program,
                access.table,
                access.field,
            );
            table_var_types.insert(var_name, var_type);
        }

        Ok(SliceAnalysisInfo {
            live_in_a,
            live_out_a,
            live_in_b,
            live_out_b,
            tables_read_a,
            tables_written_a,
            tables_read_b,
            tables_written_b,
            tables_written_last_hop_a: tables_written_last_hop_a_strings,
            tables_written_last_hop_b: tables_written_last_hop_b_strings,
            table_var_types,
        })
    }
}
