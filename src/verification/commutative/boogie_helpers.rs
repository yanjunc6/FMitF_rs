use crate::cfg::{Program as CfgProgram, VariableId};
use crate::verification::base_generator::BaseVerificationGenerator;
use crate::verification::errors::Results;
use crate::verification::scope::SliceId;
use crate::verification::Boogie::{
    gen_Boogie::BoogieProgramGenerator, BoogieBinOp, BoogieError, BoogieExpr, BoogieExprKind,
    BoogieLine, ErrorMessage, VerificationNodeId,
};
use std::collections::{HashMap, HashSet};

use super::{slice_analyzer::SliceAnalysisInfo, CommutativeUnit};

/// Variable snapshot names for tracking state
pub struct VariableSnapshots {
    /// Names for table state snapshots
    pub table_snapshots: HashMap<String, String>,
    /// Names for variable snapshots per slice
    pub var_snapshots: HashMap<SliceId, HashMap<VariableId, String>>, // slice -> (var -> name)
}

pub struct BoogieStateManager;

impl BoogieStateManager {
    pub fn new() -> Self {
        BoogieStateManager
    }

    /// Havoc all tables and live-in variables to create initial state (working on current procedure)
    pub fn havoc_initial_state(
        &self,
        base: &mut BaseVerificationGenerator,
        cfg_program: &CfgProgram,
        analysis_info: &SliceAnalysisInfo,
        unit: &CommutativeUnit,
    ) -> Results<()> {
        base.add_comment_to_current_procedure("--- Step 1: Havoc initial state ---".to_string());

        // Havoc tables read/written by both slices (exclude primary keys)
        let mut tables_to_havoc = HashSet::new();
        for set in analysis_info.tables_read.values() {
            tables_to_havoc.extend(set);
        }
        for set in analysis_info.tables_written.values() {
            tables_to_havoc.extend(set);
        }

        for table_var_name in tables_to_havoc {
            base.add_line(BoogieLine::Havoc(table_var_name.clone()));
        }

        // Havoc live-IN variables only
        for (slice, vars) in &analysis_info.live_in {
            for &var_id in vars {
                let var_name = format!("s{}_{}", slice, cfg_program.variables[var_id].name);
                base.add_line(BoogieLine::Havoc(var_name));
            }
        }

        // Initialize active flags to true (don't havoc them, set them directly)
        for (&slice_id, _) in &unit.hops_per_slice {
            let active_var_name = format!("s{}_active", slice_id);

            // Ensure the active flag variable exists as a local variable
            base.generator.ensure_local_variable_exists(
                &active_var_name,
                crate::verification::Boogie::BoogieType::Bool,
            );

            base.add_line(BoogieLine::Assign(
                active_var_name,
                BoogieExpr {
                    kind: BoogieExprKind::BoolConst(true),
                },
            ));
        }

        Ok(())
    }

    /// Save initial state by copying variables to snapshot versions (working on current procedure)
    pub fn save_initial_state(
        &self,
        base: &mut BaseVerificationGenerator,
        cfg_program: &CfgProgram,
        analysis_info: &SliceAnalysisInfo,
        _unit: &CommutativeUnit,
    ) -> Results<()> {
        base.add_comment_to_current_procedure("--- Step 2: Save initial state ---".to_string());

        // Save tables read/written by both slices
        let mut tables_to_save = HashSet::new();
        for set in analysis_info.tables_read.values() {
            tables_to_save.extend(set);
        }
        for set in analysis_info.tables_written.values() {
            tables_to_save.extend(set);
        }

        for table_var_name in tables_to_save {
            let snapshot_name = format!("{}_init", table_var_name);
            // Resolve table type using the simplified table_var_types map
            let table_type = analysis_info
                .table_var_types
                .get(table_var_name)
                .cloned()
                .unwrap();
            let _ = base
                .generator
                .ensure_local_variable_exists(&snapshot_name, table_type);

            let assign_expr = BoogieExpr {
                kind: BoogieExprKind::Var(table_var_name.clone()),
            };
            base.add_line(BoogieLine::Assign(snapshot_name, assign_expr));
        }

        // Save live-IN variables only
        for (slice, vars) in &analysis_info.live_in {
            for &var_id in vars {
                let var_name = format!("s{}_{}", slice, cfg_program.variables[var_id].name);
                let snapshot_name = format!("{}_init", var_name);
                // Add the snapshot variable as a local variable
                let var_type = BoogieProgramGenerator::convert_type_id(
                    cfg_program,
                    &cfg_program.variables[var_id].ty,
                );
                let _ = base
                    .generator
                    .ensure_local_variable_exists(&snapshot_name, var_type);

                let assign_expr = BoogieExpr {
                    kind: BoogieExprKind::Var(var_name),
                };
                base.add_line(BoogieLine::Assign(snapshot_name, assign_expr));
            }
        }

        Ok(())
    }

    /// Restore initial state from snapshots (working on current procedure)
    pub fn restore_initial_state(
        &self,
        base: &mut BaseVerificationGenerator,
        cfg_program: &CfgProgram,
        analysis_info: &SliceAnalysisInfo,
        unit: &CommutativeUnit,
    ) -> Results<()> {
        base.add_comment_to_current_procedure("Restoring initial state:".to_string());

        // Restore tables read/written by both slices
        let mut tables_to_restore = HashSet::new();
        for set in analysis_info.tables_read.values() {
            tables_to_restore.extend(set);
        }
        for set in analysis_info.tables_written.values() {
            tables_to_restore.extend(set);
        }

        for table_var_name in tables_to_restore {
            let snapshot_name = format!("{}_init", table_var_name);
            let assign_expr = BoogieExpr {
                kind: BoogieExprKind::Var(snapshot_name),
            };
            base.add_line(BoogieLine::Assign(table_var_name.clone(), assign_expr));
        }

        // Restore live-IN variables only
        for (slice, vars) in &analysis_info.live_in {
            for &var_id in vars {
                let var_name = format!("s{}_{}", slice, cfg_program.variables[var_id].name);
                let snapshot_name = format!("{}_init", var_name);
                let assign_expr = BoogieExpr {
                    kind: BoogieExprKind::Var(snapshot_name),
                };
                base.add_line(BoogieLine::Assign(var_name, assign_expr));
            }
        }

        // Reset active flags back to true (they should always be active between interleavings)
        for (&slice_id, _) in &unit.hops_per_slice {
            let active_var_name = format!("s{}_active", slice_id);
            let assign_expr = BoogieExpr {
                kind: BoogieExprKind::BoolConst(true),
            };
            base.add_line(BoogieLine::Assign(active_var_name, assign_expr));
        }

        Ok(())
    }

    /// Execute a single hop with unique labels to avoid conflicts (working on current procedure)
    // Removed old unique label hop execution helpers; interleaving now handled by InterleavingExecutor

    /// Snapshot the final state of variables after execution (working on current procedure)
    pub fn snapshot_final_state(
        &self,
        base: &mut BaseVerificationGenerator,
        cfg_program: &CfgProgram,
        analysis_info: &SliceAnalysisInfo,
        suffix: &str,
    ) -> Results<VariableSnapshots> {
        let mut table_snapshots = HashMap::new();
        let mut var_snapshots: HashMap<SliceId, HashMap<VariableId, String>> = HashMap::new();

        base.add_comment_to_current_procedure(format!("Snapshotting final state for {}", suffix));

        // Snapshot tables written by last hop only
        let mut tables_written_last_hop = HashSet::new();
        for set in analysis_info.tables_written_last_hop.values() {
            tables_written_last_hop.extend(set);
        }

        for table_var_name in tables_written_last_hop {
            let snapshot_name = format!("{}_{}", table_var_name, suffix);
            table_snapshots.insert(table_var_name.clone(), snapshot_name.clone());

            // Resolve table type using the simplified table_var_types map
            let table_type = analysis_info
                .table_var_types
                .get(table_var_name)
                .cloned()
                .unwrap();
            let _ = base
                .generator
                .ensure_local_variable_exists(&snapshot_name, table_type);

            let assign_expr = BoogieExpr {
                kind: BoogieExprKind::Var(table_var_name.clone()),
            };
            base.add_line(BoogieLine::Assign(snapshot_name, assign_expr));
        }

        // Snapshot live-OUT variables only
        for (slice, vars) in &analysis_info.live_out {
            for &var_id in vars {
                let var_name = format!("s{}_{}", slice, cfg_program.variables[var_id].name);
                let snapshot_name = format!("{}_{}", var_name, suffix);
                var_snapshots
                    .entry(*slice)
                    .or_default()
                    .insert(var_id, snapshot_name.clone());

                // Add the snapshot variable as a local variable
                let var_type = BoogieProgramGenerator::convert_type_id(
                    cfg_program,
                    &cfg_program.variables[var_id].ty,
                );
                let _ = base
                    .generator
                    .ensure_local_variable_exists(&snapshot_name, var_type);

                let assign_expr = BoogieExpr {
                    kind: BoogieExprKind::Var(var_name),
                };
                base.add_line(BoogieLine::Assign(snapshot_name, assign_expr));
            }
        }

        Ok(VariableSnapshots {
            table_snapshots,
            var_snapshots,
        })
    }

    /// Assert that the two special interleavings (A→B and B→A) produce equivalent results
    pub fn assert_special_interleavings_equivalence(
        &self,
        base: &mut BaseVerificationGenerator,
        _cfg_program: &CfgProgram,
        analysis_info: &SliceAnalysisInfo,
        a_then_b_vars: &VariableSnapshots,
        b_then_a_vars: &VariableSnapshots,
        node_a: (usize, u32, usize), // (function_id, instance, hop_id)
        node_b: (usize, u32, usize), // (function_id, instance, hop_id)
    ) -> Results<()> {
        base.add_comment_to_current_procedure("Verifying A->B === B->A equivalence:".to_string());

        let mut equality_conditions = Vec::new();

        // Compare tables written by last hop only
        let mut tables_written_last_hop = HashSet::new();
        for set in analysis_info.tables_written_last_hop.values() {
            tables_written_last_hop.extend(set);
        }

        for table_var_name in tables_written_last_hop {
            if let (Some(a_then_b_snapshot), Some(b_then_a_snapshot)) = (
                a_then_b_vars.table_snapshots.get(table_var_name),
                b_then_a_vars.table_snapshots.get(table_var_name),
            ) {
                let a_then_b_expr = BoogieExpr {
                    kind: BoogieExprKind::Var(a_then_b_snapshot.clone()),
                };
                let b_then_a_expr = BoogieExpr {
                    kind: BoogieExprKind::Var(b_then_a_snapshot.clone()),
                };
                equality_conditions.push(BoogieExpr {
                    kind: BoogieExprKind::BinOp(
                        Box::new(a_then_b_expr),
                        BoogieBinOp::Eq,
                        Box::new(b_then_a_expr),
                    ),
                });
            }
        }

        // Compare live-OUT variables only
        if let Some(vars_a) = analysis_info.live_out.get(&0) {
            for &var_id in vars_a {
                if let (Some(a_then_b_snapshot), Some(b_then_a_snapshot)) = (
                    a_then_b_vars
                        .var_snapshots
                        .get(&0)
                        .and_then(|m| m.get(&var_id)),
                    b_then_a_vars
                        .var_snapshots
                        .get(&0)
                        .and_then(|m| m.get(&var_id)),
                ) {
                    let a_then_b_expr = BoogieExpr {
                        kind: BoogieExprKind::Var(a_then_b_snapshot.clone()),
                    };
                    let b_then_a_expr = BoogieExpr {
                        kind: BoogieExprKind::Var(b_then_a_snapshot.clone()),
                    };
                    equality_conditions.push(BoogieExpr {
                        kind: BoogieExprKind::BinOp(
                            Box::new(a_then_b_expr),
                            BoogieBinOp::Eq,
                            Box::new(b_then_a_expr),
                        ),
                    });
                }
            }
        }
        if let Some(vars_b) = analysis_info.live_out.get(&1) {
            for &var_id in vars_b {
                if let (Some(a_then_b_snapshot), Some(b_then_a_snapshot)) = (
                    a_then_b_vars
                        .var_snapshots
                        .get(&1)
                        .and_then(|m| m.get(&var_id)),
                    b_then_a_vars
                        .var_snapshots
                        .get(&1)
                        .and_then(|m| m.get(&var_id)),
                ) {
                    let a_then_b_expr = BoogieExpr {
                        kind: BoogieExprKind::Var(a_then_b_snapshot.clone()),
                    };
                    let b_then_a_expr = BoogieExpr {
                        kind: BoogieExprKind::Var(b_then_a_snapshot.clone()),
                    };
                    equality_conditions.push(BoogieExpr {
                        kind: BoogieExprKind::BinOp(
                            Box::new(a_then_b_expr),
                            BoogieBinOp::Eq,
                            Box::new(b_then_a_expr),
                        ),
                    });
                }
            }
        }

        // Create assertion: (A→B result) == (B→A result)
        // Build conjunction of all equality conditions
        let equivalence_assertion = if equality_conditions.is_empty() {
            BoogieExpr {
                kind: BoogieExprKind::BoolConst(true),
            }
        } else {
            let mut iter = equality_conditions.into_iter();
            let first = iter.next().unwrap();
            iter.fold(first, |acc, e| BoogieExpr {
                kind: BoogieExprKind::BinOp(Box::new(acc), BoogieBinOp::And, Box::new(e)),
            })
        };

        let error_msg = ErrorMessage {
            boogie_error: BoogieError::SpecialInterleavingNonEquivalence {
                node_1: VerificationNodeId {
                    function_id: node_a.0,
                    instance: node_a.1,
                    hop_id: node_a.2,
                },
                node_2: VerificationNodeId {
                    function_id: node_b.0,
                    instance: node_b.1,
                    hop_id: node_b.2,
                },
            },
        };

        // Remove debug output

        base.add_assertion_to_current_procedure(equivalence_assertion, error_msg);

        Ok(())
    }
}
