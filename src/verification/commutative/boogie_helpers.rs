use crate::verification::errors::Results;
use crate::verification::Boogie::{
    gen_Boogie::BoogieProgramGenerator, BoogieBinOp, BoogieExpr, BoogieExprKind, BoogieLine,
    BoogieType, ErrorMessage,
};
use crate::cfg::{CfgProgram, HopId, VarId};
use std::collections::{HashMap, HashSet};

use super::slice_analyzer::SliceAnalysisInfo;

/// Variable snapshot names for tracking state
pub struct VariableSnapshots {
    /// Names for table state snapshots
    pub table_snapshots: HashMap<String, String>,
    /// Names for variable snapshots
    pub var_snapshots: HashMap<VarId, String>,
}

impl VariableSnapshots {
    pub fn empty() -> Self {
        VariableSnapshots {
            table_snapshots: HashMap::new(),
            var_snapshots: HashMap::new(),
        }
    }
}

pub struct BoogieStateManager;

impl BoogieStateManager {
    pub fn new() -> Self {
        BoogieStateManager
    }

    /// Havoc all tables and live-in variables to create initial state (working on current procedure)
    pub fn havoc_initial_state(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        analysis_info: &SliceAnalysisInfo,
    ) -> Results<()> {
        generator.add_comment_to_current_procedure(
            "--- Step 1: Havoc initial state ---".to_string(),
        );

        // Havoc tables read/written by both slices (exclude primary keys)
        let mut tables_to_havoc = HashSet::new();
        tables_to_havoc.extend(&analysis_info.tables_read_a);
        tables_to_havoc.extend(&analysis_info.tables_written_a);
        tables_to_havoc.extend(&analysis_info.tables_read_b);
        tables_to_havoc.extend(&analysis_info.tables_written_b);

        for table_var_name in tables_to_havoc {
            generator.add_line_to_current_procedure(BoogieLine::Havoc(table_var_name.clone()));
        }

        // Havoc live-IN variables only
        let mut live_in_vars = HashSet::new();
        live_in_vars.extend(&analysis_info.live_in_a);
        live_in_vars.extend(&analysis_info.live_in_b);

        for &var_id in &live_in_vars {
            let var_name = generator.gen_var_name(cfg_program, var_id, None);
            generator.add_line_to_current_procedure(BoogieLine::Havoc(var_name));
        }

        Ok(())
    }

    /// Save initial state by copying variables to snapshot versions (working on current procedure)
    pub fn save_initial_state(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        analysis_info: &SliceAnalysisInfo,
    ) -> Results<()> {
        // TODO: Need to create new variables so that the variable declarations should be done at beginning of procedure
        generator.add_comment_to_current_procedure(
            "--- Step 2: Save initial state ---".to_string(),
        );

        // Save tables read/written by both slices
        let mut tables_to_save = HashSet::new();
        tables_to_save.extend(&analysis_info.tables_read_a);
        tables_to_save.extend(&analysis_info.tables_written_a);
        tables_to_save.extend(&analysis_info.tables_read_b);
        tables_to_save.extend(&analysis_info.tables_written_b);

        for table_var_name in tables_to_save {
            let snapshot_name = format!("{}_init", table_var_name);
            // TODO: check how gen_Boogie resolve the table type
            // Add the snapshot variable as a local variable
            let table_type = BoogieType::Map(vec![Box::new(BoogieType::Int)], Box::new(BoogieType::Int)); // Placeholder type
            generator.add_local_var(&snapshot_name, table_type);
            
            let assign_expr = BoogieExpr {
                kind: BoogieExprKind::Var(table_var_name.clone()),
            };
            generator.add_line_to_current_procedure(BoogieLine::Assign(snapshot_name, assign_expr));
        }

        // Save live-IN variables only
        let mut live_in_vars = HashSet::new();
        live_in_vars.extend(&analysis_info.live_in_a);
        live_in_vars.extend(&analysis_info.live_in_b);

        for &var_id in &live_in_vars {
            let var_name = generator.gen_var_name(cfg_program, var_id, None);
            let snapshot_name = format!("{}_init", var_name);
            // Add the snapshot variable as a local variable  
            let var_type = BoogieProgramGenerator::convert_type(&cfg_program.variables[var_id].ty);
            generator.add_local_var(&snapshot_name, var_type);
            
            let assign_expr = BoogieExpr {
                kind: BoogieExprKind::Var(var_name),
            };
            generator.add_line_to_current_procedure(BoogieLine::Assign(snapshot_name, assign_expr));
        }

        Ok(())
    }

    /// Restore initial state from snapshots (working on current procedure)
    pub fn restore_initial_state(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        analysis_info: &SliceAnalysisInfo,
    ) -> Results<()> {
        generator.add_comment_to_current_procedure("Restoring initial state:".to_string());

        // Restore tables read/written by both slices
        let mut tables_to_restore = HashSet::new();
        tables_to_restore.extend(&analysis_info.tables_read_a);
        tables_to_restore.extend(&analysis_info.tables_written_a);
        tables_to_restore.extend(&analysis_info.tables_read_b);
        tables_to_restore.extend(&analysis_info.tables_written_b);

        for table_var_name in tables_to_restore {
            let snapshot_name = format!("{}_init", table_var_name);
            let assign_expr = BoogieExpr {
                kind: BoogieExprKind::Var(snapshot_name),
            };
            generator.add_line_to_current_procedure(BoogieLine::Assign(table_var_name.clone(), assign_expr));
        }

        // Restore live-IN variables only
        let mut live_in_vars = HashSet::new();
        live_in_vars.extend(&analysis_info.live_in_a);
        live_in_vars.extend(&analysis_info.live_in_b);

        for &var_id in &live_in_vars {
            let var_name = generator.gen_var_name(cfg_program, var_id, None);
            let snapshot_name = format!("{}_init", var_name);
            let assign_expr = BoogieExpr {
                kind: BoogieExprKind::Var(snapshot_name),
            };
            generator.add_line_to_current_procedure(BoogieLine::Assign(var_name, assign_expr));
        }

        Ok(())
    }

    /// Execute a single hop with unique labels to avoid conflicts (working on current procedure)
    pub fn execute_hop_with_unique_labels(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        hop_id: HopId,
        suffix: &str,
        function_name: &str,
        is_last_hop: bool,
    ) -> Results<()> {
        let hop = &cfg_program.hops[hop_id];

        generator.add_comment_to_current_procedure(format!("Executing hop {}", hop_id.index()));

        // Process each basic block in the hop
        for &block_id in hop.blocks.iter() {
            let _block = &cfg_program.blocks[block_id];

            // Generate unique label using gen_basic_block_label
            let prefix_str = format!("exec_{}", suffix);
            let label = BoogieProgramGenerator::gen_basic_block_label(block_id, Some(&prefix_str), None);
            generator.add_line_to_current_procedure(BoogieLine::Label(label));

            // Convert statements
            for statement in &cfg_program.blocks[block_id].statements {
                let boogie_lines = generator.convert_statement(cfg_program, statement, None)?;
                generator.add_lines_to_current_procedure(boogie_lines);
            }

            if !is_last_hop {
                // Generate control flow edges directly to current procedure
                let mut temp_lines = Vec::new();
                generator.gen_basic_block_edges(
                    &mut temp_lines,
                    cfg_program,
                    block_id,
                    function_name,
                    Some(&prefix_str),
                    None,
                );
                generator.add_lines_to_current_procedure(temp_lines);
            } else {
                // Last hop, generate function end, abort, return labels
                let end_label = BoogieProgramGenerator::gen_function_end_label(
                    function_name,
                    Some(&prefix_str),
                    None,
                );
                generator.add_line_to_current_procedure(BoogieLine::Label(end_label));

                let abort_label = BoogieProgramGenerator::gen_function_abort_label(
                    function_name,
                    Some(&prefix_str),
                    None,
                );
                generator.add_line_to_current_procedure(BoogieLine::Label(abort_label));

                let return_label = BoogieProgramGenerator::gen_function_return_label(
                    function_name,
                    Some(&prefix_str),
                    None,
                );
                generator.add_line_to_current_procedure(BoogieLine::Label(return_label));
            }
        }

        Ok(())
    }

    /// Snapshot the final state of variables after execution (working on current procedure)
    pub fn snapshot_final_state(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        analysis_info: &SliceAnalysisInfo,
        suffix: &str,
    ) -> Results<VariableSnapshots> {
        let mut table_snapshots = HashMap::new();
        let mut var_snapshots = HashMap::new();

        generator.add_comment_to_current_procedure(format!("Snapshotting final state for {}", suffix));

        // Snapshot tables written by last hop only
        let mut tables_written_last_hop = HashSet::new();
        tables_written_last_hop.extend(&analysis_info.tables_written_last_hop_a);
        tables_written_last_hop.extend(&analysis_info.tables_written_last_hop_b);

        for table_var_name in tables_written_last_hop {
            let snapshot_name = format!("{}_{}", table_var_name, suffix);
            table_snapshots.insert(table_var_name.clone(), snapshot_name.clone());
            
            // Add the snapshot variable as a local variable
            // TODO: resolve table type
            let table_type = BoogieType::Map(vec![Box::new(BoogieType::Int)], Box::new(BoogieType::Int)); // Placeholder type
            generator.add_local_var(&snapshot_name, table_type);
            
            let assign_expr = BoogieExpr {
                kind: BoogieExprKind::Var(table_var_name.clone()),
            };
            generator.add_line_to_current_procedure(BoogieLine::Assign(snapshot_name, assign_expr));
        }

        // Snapshot live-OUT variables only
        let mut live_out_vars = HashSet::new();
        live_out_vars.extend(&analysis_info.live_out_a);
        live_out_vars.extend(&analysis_info.live_out_b);

        for &var_id in &live_out_vars {
            let var_name = generator.gen_var_name(cfg_program, var_id, None);
            let snapshot_name = format!("{}_{}", var_name, suffix);
            var_snapshots.insert(var_id, snapshot_name.clone());
            
            // Add the snapshot variable as a local variable  
            let var_type = BoogieProgramGenerator::convert_type(&cfg_program.variables[var_id].ty);
            generator.add_local_var(&snapshot_name, var_type);
            
            let assign_expr = BoogieExpr {
                kind: BoogieExprKind::Var(var_name),
            };
            generator.add_line_to_current_procedure(BoogieLine::Assign(snapshot_name, assign_expr));
        }

        Ok(VariableSnapshots {
            table_snapshots,
            var_snapshots,
        })
    }

    /// Assert that current state equals one of the special interleaving states (working on current procedure)
    pub fn assert_equivalence_to_special_interleavings(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        analysis_info: &SliceAnalysisInfo,
        a_then_b_vars: &VariableSnapshots,
        b_then_a_vars: &VariableSnapshots,
    ) -> Results<()> {
        let mut equality_conditions_a_then_b = Vec::new();
        let mut equality_conditions_b_then_a = Vec::new();

        // Compare tables written by last hop only
        let mut tables_written_last_hop = HashSet::new();
        tables_written_last_hop.extend(&analysis_info.tables_written_last_hop_a);
        tables_written_last_hop.extend(&analysis_info.tables_written_last_hop_b);

        for table_var_name in tables_written_last_hop {
            // Current var == a_then_b snapshot
            if let Some(a_then_b_snapshot) = a_then_b_vars.table_snapshots.get(table_var_name) {
                let current_expr = BoogieExpr {
                    kind: BoogieExprKind::Var(table_var_name.clone()),
                };
                let a_then_b_expr = BoogieExpr {
                    kind: BoogieExprKind::Var(a_then_b_snapshot.clone()),
                };
                equality_conditions_a_then_b.push(BoogieExpr {
                    kind: BoogieExprKind::BinOp(
                        Box::new(current_expr.clone()),
                        BoogieBinOp::Eq,
                        Box::new(a_then_b_expr),
                    ),
                });
            }

            // Current var == b_then_a snapshot
            if let Some(b_then_a_snapshot) = b_then_a_vars.table_snapshots.get(table_var_name) {
                let current_expr = BoogieExpr {
                    kind: BoogieExprKind::Var(table_var_name.clone()),
                };
                let b_then_a_expr = BoogieExpr {
                    kind: BoogieExprKind::Var(b_then_a_snapshot.clone()),
                };
                equality_conditions_b_then_a.push(BoogieExpr {
                    kind: BoogieExprKind::BinOp(
                        Box::new(current_expr),
                        BoogieBinOp::Eq,
                        Box::new(b_then_a_expr),
                    ),
                });
            }
        }

        // Compare live-OUT variables only
        let mut live_out_vars = HashSet::new();
        live_out_vars.extend(&analysis_info.live_out_a);
        live_out_vars.extend(&analysis_info.live_out_b);

        for &var_id in &live_out_vars {
            let var_name = generator.gen_var_name(cfg_program, var_id, None);

            // Current var == a_then_b snapshot
            if let Some(a_then_b_snapshot) = a_then_b_vars.var_snapshots.get(&var_id) {
                let current_expr = BoogieExpr {
                    kind: BoogieExprKind::Var(var_name.clone()),
                };
                let a_then_b_expr = BoogieExpr {
                    kind: BoogieExprKind::Var(a_then_b_snapshot.clone()),
                };
                equality_conditions_a_then_b.push(BoogieExpr {
                    kind: BoogieExprKind::BinOp(
                        Box::new(current_expr.clone()),
                        BoogieBinOp::Eq,
                        Box::new(a_then_b_expr),
                    ),
                });
            }

            // Current var == b_then_a snapshot
            if let Some(b_then_a_snapshot) = b_then_a_vars.var_snapshots.get(&var_id) {
                let current_expr = BoogieExpr {
                    kind: BoogieExprKind::Var(var_name),
                };
                let b_then_a_expr = BoogieExpr {
                    kind: BoogieExprKind::Var(b_then_a_snapshot.clone()),
                };
                equality_conditions_b_then_a.push(BoogieExpr {
                    kind: BoogieExprKind::BinOp(
                        Box::new(current_expr),
                        BoogieBinOp::Eq,
                        Box::new(b_then_a_expr),
                    ),
                });
            }
        }

        // Create final assertion: (current == a_then_b) OR (current == b_then_a)
        let a_then_b_equal = BoogieProgramGenerator::gen_conjunction(equality_conditions_a_then_b);
        let b_then_a_equal = BoogieProgramGenerator::gen_conjunction(equality_conditions_b_then_a);

        let final_assertion =
            BoogieProgramGenerator::gen_disjunction(vec![a_then_b_equal, b_then_a_equal]);

        let error_msg = ErrorMessage {
            msg: "Slice commutativity violation: interleaving produces different result than both special orderings".to_string(),
        };

        generator.add_assertion_to_current_procedure(final_assertion, error_msg);

        Ok(())
    }
}
