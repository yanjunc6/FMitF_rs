use crate::cfg::{
    BasicBlockId, FunctionId, Instruction, InstructionKind, Operand, Program as CfgProgram,
    Terminator, VariableId,
};
use crate::verification::base_generator::BaseVerificationGenerator;
use crate::verification::errors::Results;
use crate::verification::scope::SliceId;
use crate::verification::Boogie::{
    gen_Boogie::BoogieProgramGenerator, BoogieBinOp, BoogieError, BoogieExpr, BoogieExprKind,
    BoogieLine, BoogieQuantifierKind, ErrorMessage, VerificationNodeId,
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
        let mut tables_to_havoc_sorted: Vec<&String> = tables_to_havoc.into_iter().collect();
        tables_to_havoc_sorted.sort();

        let mut havocked_vars: HashSet<String> = HashSet::new();

        for table_var_name in tables_to_havoc_sorted {
            base.add_line(BoogieLine::Havoc(table_var_name.clone()));
            havocked_vars.insert(table_var_name.clone());
        }

        // Havoc live-IN variables only
        let mut live_in_slices: Vec<_> = analysis_info.live_in.keys().cloned().collect();
        live_in_slices.sort();
        for slice in live_in_slices {
            let mut vars_sorted: Vec<_> = analysis_info.live_in[&slice].iter().cloned().collect();
            vars_sorted.sort_by_key(|v| v.index());
            for var_id in vars_sorted {
                let var = &cfg_program.variables[var_id];

                let var_name = format!("s{}_{}", slice, var.name);
                let var_type = BoogieProgramGenerator::convert_type_id(cfg_program, &var.ty);

                // Ensure the variable exists as a local variable
                base.generator
                    .ensure_local_variable_exists(&var_name, var_type);

                base.add_line(BoogieLine::Havoc(var_name.clone()));
                havocked_vars.insert(var_name);
            }
        }

        // Initialize active flags to true (don't havoc them, set them directly)
        let mut slice_ids: Vec<_> = unit.hops_per_slice.keys().cloned().collect();
        slice_ids.sort();
        for slice_id in slice_ids {
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

        // Add assumptions from table invariants
        for table_id in cfg_program.all_tables.iter() {
            let table = &cfg_program.tables[*table_id];
            for inv_func_id in &table.invariants {
                let inv_func = &cfg_program.functions[*inv_func_id];
                let arg_exprs: Vec<BoogieExpr> = inv_func
                    .params
                    .iter()
                    .map(|param_var_id| {
                        let param_var = &cfg_program.variables[*param_var_id];
                        BoogieExpr {
                            kind: BoogieExprKind::Var(param_var.name.clone()),
                        }
                    })
                    .collect();

                // Table invariants are global, use slice 0 as context for __slice__ resolution
                let body_expr =
                    Self::inline_special_function(base, cfg_program, *inv_func_id, &arg_exprs, 0);

                if inv_func.params.is_empty() {
                    base.add_line(BoogieLine::Assume(body_expr));
                } else {
                    let bound_vars = inv_func
                        .params
                        .iter()
                        .map(|param_var_id| {
                            let param_var = &cfg_program.variables[*param_var_id];
                            let var_type =
                                BoogieProgramGenerator::convert_type_id(cfg_program, &param_var.ty);
                            (param_var.name.clone(), var_type)
                        })
                        .collect();
                    let quant_expr = BoogieExpr {
                        kind: BoogieExprKind::Quantifier {
                            kind: BoogieQuantifierKind::Forall,
                            bound_vars,
                            body: Box::new(body_expr),
                        },
                    };
                    base.add_line(BoogieLine::Assume(quant_expr));
                }
            }
        }

        // Add assumptions from transaction parameters
        let mut slice_ids: Vec<_> = unit.hops_per_slice.keys().cloned().collect();
        slice_ids.sort();
        for slice_id in slice_ids {
            if let Some(first_hop_id) = unit
                .hops_per_slice
                .get(&slice_id)
                .and_then(|hops| hops.first())
            {
                let func_id = cfg_program.hops[*first_hop_id].function_id;
                let func = &cfg_program.functions[func_id];
                for assume_func_id in &func.assumptions {
                    let assume_func = &cfg_program.functions[*assume_func_id];
                    let mut args = Vec::new();
                    for param_id in &assume_func.params {
                        let expr = Self::ensure_slice_variable_initialized(
                            base,
                            cfg_program,
                            *param_id,
                            slice_id,
                            &mut havocked_vars,
                        );
                        args.push(expr);
                    }
                    let assume_expr = Self::inline_special_function(
                        base,
                        cfg_program,
                        *assume_func_id,
                        &args,
                        slice_id,
                    );
                    base.add_line(BoogieLine::Assume(assume_expr));
                }
            }
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

        let mut tables_to_save_sorted: Vec<&String> = tables_to_save.into_iter().collect();
        tables_to_save_sorted.sort();

        for table_var_name in tables_to_save_sorted {
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
        let mut live_in_slices: Vec<_> = analysis_info.live_in.keys().cloned().collect();
        live_in_slices.sort();
        for slice in live_in_slices {
            let mut vars_sorted: Vec<_> = analysis_info.live_in[&slice].iter().cloned().collect();
            vars_sorted.sort_by_key(|v| v.index());
            for var_id in vars_sorted {
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

        let mut tables_to_restore_sorted: Vec<&String> = tables_to_restore.into_iter().collect();
        tables_to_restore_sorted.sort();

        for table_var_name in tables_to_restore_sorted {
            let snapshot_name = format!("{}_init", table_var_name);
            let assign_expr = BoogieExpr {
                kind: BoogieExprKind::Var(snapshot_name),
            };
            base.add_line(BoogieLine::Assign(table_var_name.clone(), assign_expr));
        }

        // Restore live-IN variables only
        let mut live_in_slices: Vec<_> = analysis_info.live_in.keys().cloned().collect();
        live_in_slices.sort();
        for slice in live_in_slices {
            let mut vars_sorted: Vec<_> = analysis_info.live_in[&slice].iter().cloned().collect();
            vars_sorted.sort_by_key(|v| v.index());
            for var_id in vars_sorted {
                let var_name = format!("s{}_{}", slice, cfg_program.variables[var_id].name);
                let snapshot_name = format!("{}_init", var_name);
                let assign_expr = BoogieExpr {
                    kind: BoogieExprKind::Var(snapshot_name),
                };
                base.add_line(BoogieLine::Assign(var_name, assign_expr));
            }
        }

        // Reset active flags back to true (they should always be active between interleavings)
        let mut slice_ids: Vec<_> = unit.hops_per_slice.keys().cloned().collect();
        slice_ids.sort();
        for slice_id in slice_ids {
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

        let mut tables_written_last_hop_sorted: Vec<&String> = tables_written_last_hop.into_iter().collect();
        tables_written_last_hop_sorted.sort();

        for table_var_name in tables_written_last_hop_sorted {
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
        let mut live_out_slices: Vec<_> = analysis_info.live_out.keys().cloned().collect();
        live_out_slices.sort();
        for slice in live_out_slices {
            let mut vars_sorted: Vec<_> = analysis_info.live_out[&slice].iter().cloned().collect();
            vars_sorted.sort_by_key(|v| v.index());
            for var_id in vars_sorted {
                let var_name = format!("s{}_{}", slice, cfg_program.variables[var_id].name);
                let snapshot_name = format!("{}_{}", var_name, suffix);
                var_snapshots
                    .entry(slice)
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

    fn inline_special_function(
        base: &mut BaseVerificationGenerator,
        cfg_program: &CfgProgram,
        func_id: FunctionId,
        args: &[BoogieExpr],
        slice_id: SliceId,
    ) -> BoogieExpr {
        let func = &cfg_program.functions[func_id];
        assert_eq!(
            func.params.len(),
            args.len(),
            "Special function {} expected {} arguments, got {}",
            func.name,
            func.params.len(),
            args.len()
        );

        let mut env: HashMap<VariableId, BoogieExpr> = HashMap::new();
        for (param_id, arg_expr) in func.params.iter().zip(args.iter()) {
            env.insert(*param_id, arg_expr.clone());
        }

        let mut current_block = func
            .entry_block
            .expect("Special function must have an entry block");

        enum InlineControl {
            Next(BasicBlockId),
            Return(BoogieExpr),
        }

        loop {
            let control = {
                let block = &cfg_program.basic_blocks[current_block];
                for instruction in &block.instructions {
                    Self::evaluate_special_instruction(
                        base,
                        cfg_program,
                        instruction,
                        &mut env,
                        slice_id,
                    );
                }

                match &block.terminator {
                    Terminator::Return(Some(op)) => {
                        let expr = Self::operand_to_expr(base, cfg_program, op, &env, slice_id);
                        InlineControl::Return(expr)
                    }
                    Terminator::Return(None) => InlineControl::Return(BoogieExpr {
                        kind: BoogieExprKind::BoolConst(true),
                    }),
                    Terminator::Jump(next) => InlineControl::Next(*next),
                    Terminator::Abort => {
                        panic!("Abort terminator not supported in special expression function")
                    }
                    Terminator::Branch { .. } => {
                        panic!("Branch terminator not supported in special expression function")
                    }
                    Terminator::HopExit { .. } => {
                        panic!("Hop exit terminator not supported in special expression function")
                    }
                }
            };

            match control {
                InlineControl::Return(expr) => return expr,
                InlineControl::Next(next_block) => current_block = next_block,
            }
        }
    }

    fn evaluate_special_instruction(
        base: &mut BaseVerificationGenerator,
        cfg_program: &CfgProgram,
        instruction: &Instruction,
        env: &mut HashMap<VariableId, BoogieExpr>,
        slice_id: SliceId,
    ) {
        match &instruction.kind {
            InstructionKind::Assign { dest, src } => {
                let expr = Self::operand_to_expr(base, cfg_program, src, env, slice_id);
                env.insert(*dest, expr);
            }
            InstructionKind::BinaryOp {
                dest,
                op,
                left,
                right,
            } => {
                let left_expr = Self::operand_to_expr(base, cfg_program, left, env, slice_id);
                let right_expr = Self::operand_to_expr(base, cfg_program, right, env, slice_id);
                let bin_op = BoogieProgramGenerator::convert_binary_op(op);
                let result = BoogieExpr {
                    kind: BoogieExprKind::BinOp(Box::new(left_expr), bin_op, Box::new(right_expr)),
                };
                env.insert(*dest, result);
            }
            InstructionKind::UnaryOp { dest, op, operand } => {
                let operand_expr = Self::operand_to_expr(base, cfg_program, operand, env, slice_id);
                let un_op = match base.generator.convert_unary_op(op) {
                    Ok(op) => op,
                    Err(_) => {
                        panic!("Failed to convert unary op in special expression function")
                    }
                };
                let result = BoogieExpr {
                    kind: BoogieExprKind::UnOp(un_op, Box::new(operand_expr)),
                };
                env.insert(*dest, result);
            }
            InstructionKind::Call { dest, func, args } => {
                let func_name = base.resolve_function_name(cfg_program, *func);
                let mut boogie_args = Vec::with_capacity(args.len());
                for arg in args {
                    boogie_args.push(Self::operand_to_expr(base, cfg_program, arg, env, slice_id));
                }
                let call_expr = BoogieExpr {
                    kind: BoogieExprKind::FunctionCall {
                        name: func_name,
                        args: boogie_args,
                    },
                };
                if let Some(dest_id) = dest {
                    env.insert(*dest_id, call_expr);
                } else {
                    panic!("Procedure call without destination in special expression function");
                }
            }
            InstructionKind::TableGet {
                dest,
                table,
                keys,
                field,
            } => {
                let table_decl = &cfg_program.tables[*table];
                let key_exprs: Vec<BoogieExpr> = keys
                    .iter()
                    .map(|key| Self::operand_to_expr(base, cfg_program, key, env, slice_id))
                    .collect();

                if let Some(field_id) = field {
                    let field_decl = &cfg_program.table_fields[*field_id];
                    let var_name = BoogieProgramGenerator::gen_table_field_var_name(
                        &table_decl.name,
                        &field_decl.name,
                    );
                    let result = BoogieExpr {
                        kind: BoogieExprKind::MapSelect {
                            base: Box::new(BoogieExpr {
                                kind: BoogieExprKind::Var(var_name),
                            }),
                            indices: key_exprs,
                        },
                    };
                    env.insert(*dest, result);
                } else {
                    let all_field_ids: Vec<_> = table_decl
                        .primary_key_fields
                        .iter()
                        .chain(table_decl.other_fields.iter())
                        .copied()
                        .collect();
                    let mut field_value_exprs = Vec::new();
                    for field_id in all_field_ids {
                        let field = &cfg_program.table_fields[field_id];
                        let var_name = BoogieProgramGenerator::gen_table_field_var_name(
                            &table_decl.name,
                            &field.name,
                        );
                        field_value_exprs.push(BoogieExpr {
                            kind: BoogieExprKind::MapSelect {
                                base: Box::new(BoogieExpr {
                                    kind: BoogieExprKind::Var(var_name),
                                }),
                                indices: key_exprs.clone(),
                            },
                        });
                    }
                    let constructor_name = format!("construct_Row_{}", table_decl.name);
                    let row_expr = BoogieExpr {
                        kind: BoogieExprKind::FunctionCall {
                            name: constructor_name,
                            args: field_value_exprs,
                        },
                    };
                    env.insert(*dest, row_expr);
                }
            }
            InstructionKind::TableSet { .. } => {
                panic!("TableSet instruction not supported in special expression function")
            }
            InstructionKind::Assert { .. } => {
                panic!("Assertions are not supported in special expression function lowering")
            }
        }
    }

    fn operand_to_expr(
        base: &mut BaseVerificationGenerator,
        cfg_program: &CfgProgram,
        operand: &Operand,
        env: &HashMap<VariableId, BoogieExpr>,
        slice_id: SliceId,
    ) -> BoogieExpr {
        match operand {
            Operand::Variable(var_id) => env.get(var_id).cloned().unwrap_or_else(|| {
                panic!("Uninitialised variable {:?} in special function", var_id)
            }),
            Operand::Constant(constant) => match base.generator.convert_constant(constant) {
                Ok(expr) => expr,
                Err(_) => panic!("Failed to convert constant in special expression function"),
            },
            Operand::Global(global_id) => {
                let global_const = &cfg_program.global_consts[*global_id];
                // Special handling for __slice__ constant - replace with actual slice ID
                let name = if global_const.name == "__slice__" {
                    slice_id.to_string()
                } else {
                    global_const.name.clone()
                };
                BoogieExpr {
                    kind: BoogieExprKind::Var(name),
                }
            }
            Operand::Table(table_id) => {
                let table = &cfg_program.tables[*table_id];
                BoogieExpr {
                    kind: BoogieExprKind::Var(format!("TBL_{}", table.name)),
                }
            }
        }
    }

    fn ensure_slice_variable_initialized(
        base: &mut BaseVerificationGenerator,
        cfg_program: &CfgProgram,
        var_id: VariableId,
        slice_id: SliceId,
        havocked_vars: &mut HashSet<String>,
    ) -> BoogieExpr {
        let param = &cfg_program.variables[var_id];
        let var_name = format!("s{}_{}", slice_id, param.name);
        let var_type = BoogieProgramGenerator::convert_type_id(cfg_program, &param.ty);
        base.generator
            .ensure_local_variable_exists(&var_name, var_type);
        if !havocked_vars.contains(&var_name) {
            base.add_line(BoogieLine::Havoc(var_name.clone()));
            havocked_vars.insert(var_name.clone());
        }
        BoogieExpr {
            kind: BoogieExprKind::Var(var_name),
        }
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

        let mut tables_written_last_hop_sorted: Vec<&String> = tables_written_last_hop.into_iter().collect();
        tables_written_last_hop_sorted.sort();

        for table_var_name in tables_written_last_hop_sorted {
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
            let mut vars_sorted: Vec<_> = vars_a.iter().cloned().collect();
            vars_sorted.sort_by_key(|v| v.index());
            for var_id in vars_sorted {
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
            let mut vars_sorted: Vec<_> = vars_b.iter().cloned().collect();
            vars_sorted.sort_by_key(|v| v.index());
            for var_id in vars_sorted {
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
