use super::{
    boogie_helpers::{BoogieStateManager, VariableSnapshots},
    interleaving_generator::{InterleavingGenerator, SpecialInterleavings},
    CommutativeUnit,
};
use crate::cfg::Program as CfgProgram;
use crate::verification::base_generator::BaseVerificationGenerator;
use crate::verification::errors::Results;
use crate::verification::scope::SliceId;
use crate::verification::strategy::VerificationStrategy;
use crate::verification::Boogie::BoogieProcedure;
use std::collections::HashSet;

use super::slice_analyzer::{SliceAnalysisInfo, SliceAnalyzer};

/// Commutative verification strategy that uses interleaving execution and active flags
pub struct CommutativeStrategy {
    // Store raw pointers to avoid explicit lifetimes; these are borrowed-only during run()
    base: *mut BaseVerificationGenerator,
    cfg_program: *const CfgProgram,
    unit: *const CommutativeUnit,
}

impl CommutativeStrategy {
    pub fn new(
        base: &mut BaseVerificationGenerator,
        cfg_program: &CfgProgram,
        unit: &CommutativeUnit,
    ) -> Self {
        Self {
            base: base as *mut _,
            cfg_program: cfg_program as *const _,
            unit: unit as *const _,
        }
    }

    /// Run the commutative verification for the unit and return the generated procedure
    pub fn run(&mut self) -> Results<BoogieProcedure> {
        // SAFETY: Caller ensures these references remain valid during strategy execution
        let base: &mut BaseVerificationGenerator = unsafe { &mut *self.base };
        let cfg_program: &CfgProgram = unsafe { &*self.cfg_program };
        let unit: &CommutativeUnit = unsafe { &*self.unit };
        let procedure_name = format!(
            "Check_SliceCommut_Hop{}_vs_Hop{}",
            unit.c_edge.source.hop_id.index(),
            unit.c_edge.target.hop_id.index()
        );

        // Generators/helpers
        let interleaving_gen = InterleavingGenerator::new();
        let analyzer = SliceAnalyzer::new();
        let state_manager = BoogieStateManager::new();

        // Special interleavings
        let special = interleaving_gen.extract_special_interleavings(unit);

        // Analyze slices
        let analysis_info = analyzer.analyze_slice_info(cfg_program, unit)?;

        // Modifies set from analysis
        let mut modifies = HashSet::new();
        for set in analysis_info.tables_read.values() {
            modifies.extend(set.iter().cloned());
        }
        for set in analysis_info.tables_written.values() {
            modifies.extend(set.iter().cloned());
        }

        // Create empty procedure shell and set as current
        let procedure = BoogieProcedure {
            name: procedure_name,
            params: Vec::new(),
            local_vars: Vec::new(),
            modifies: modifies.into_iter().collect(),
            lines: Vec::new(),
        };
        base.generator.program.procedures.push(procedure);
        let proc_index = base.generator.program.procedures.len() - 1;
        base.generator.set_current_procedure(proc_index);

        // Header comment
        base.add_comment_to_current_procedure(format!(
            "Slice commutativity verification: hop {} vs hop {}",
            unit.c_edge.source.hop_id.index(),
            unit.c_edge.target.hop_id.index()
        ));

        // Havoc and save initial state
        state_manager.havoc_initial_state(base, cfg_program, &analysis_info, unit)?;
        state_manager.save_initial_state(base, cfg_program, &analysis_info, unit)?;

        // Interleaving execution
        let mut executor = InterleavingExecutor::new();

        // Execute A->B, snapshot, restore, execute B->A, snapshot
        let (a_then_b_vars, b_then_a_vars) = Self::execute_special_interleavings(
            base,
            cfg_program,
            &mut executor,
            &special,
            &analysis_info,
            &state_manager,
            unit,
        )?;

        // Assert equivalence A->B == B->A
        Self::verify_special_interleavings_equivalence_inner(
            base,
            cfg_program,
            &analysis_info,
            &a_then_b_vars,
            &b_then_a_vars,
            &state_manager,
            (
                unit.c_edge.source.function_id.index(),
                unit.c_edge.source.instance,
                unit.c_edge.source.hop_id.index(),
            ),
            (
                unit.c_edge.target.function_id.index(),
                unit.c_edge.target.instance,
                unit.c_edge.target.hop_id.index(),
            ),
        )?;

        // Finalize and return the constructed procedure
        base.generator.clear_current_procedure();
        Ok(base.generator.program.procedures.pop().unwrap())
    }

    fn execute_special_interleavings(
        base: &mut BaseVerificationGenerator,
        cfg_program: &CfgProgram,
        executor: &mut InterleavingExecutor,
        special: &SpecialInterleavings,
        analysis_info: &SliceAnalysisInfo,
        state_manager: &BoogieStateManager,
        unit: &CommutativeUnit,
    ) -> Results<(VariableSnapshots, VariableSnapshots)> {
        base.add_comment_to_current_procedure(
            "--- Step 3: Execute special interleavings ---".to_string(),
        );

        // Execute A then B
        base.add_comment_to_current_procedure("Executing A then B:".to_string());
        executor.execute_interleaving(base, cfg_program, &special.a_then_b, "ab")?;
        let a_then_b_vars =
            state_manager.snapshot_final_state(base, cfg_program, analysis_info, "a_then_b")?;

        // Reset state
        state_manager.restore_initial_state(base, cfg_program, analysis_info, unit)?;

        // Execute B then A
        base.add_comment_to_current_procedure("Executing B then A:".to_string());
        executor.execute_interleaving(base, cfg_program, &special.b_then_a, "ba")?;
        let b_then_a_vars =
            state_manager.snapshot_final_state(base, cfg_program, analysis_info, "b_then_a")?;

        Ok((a_then_b_vars, b_then_a_vars))
    }

    fn verify_special_interleavings_equivalence_inner(
        base: &mut BaseVerificationGenerator,
        cfg_program: &CfgProgram,
        analysis_info: &SliceAnalysisInfo,
        a_then_b_vars: &VariableSnapshots,
        b_then_a_vars: &VariableSnapshots,
        state_manager: &BoogieStateManager,
        node_a: (usize, u32, usize), // (function_id, instance, hop_id)
        node_b: (usize, u32, usize), // (function_id, instance, hop_id)
    ) -> Results<()> {
        base.add_comment_to_current_procedure(
            "--- Step 4: Verify A→B ≡ B→A (Special interleavings equivalence) ---".to_string(),
        );

        state_manager.assert_special_interleavings_equivalence(
            base,
            cfg_program,
            analysis_info,
            a_then_b_vars,
            b_then_a_vars,
            node_a,
            node_b,
        )
    }
}

impl VerificationStrategy for CommutativeStrategy {
    fn base(&mut self) -> &mut BaseVerificationGenerator {
        unsafe { &mut *self.base }
    }

    fn after_instruction(
        &mut self,
        _instruction: &crate::cfg::Instruction,
        _slice_id: SliceId,
    ) -> Results<()> {
        // No-op for commutative verification; assertions are handled at interleaving boundaries
        Ok(())
    }
}

// -----------------------------------------------------------------------------
// Inline interleaving executor implementation (previously in executor.rs)
// -----------------------------------------------------------------------------

struct InterleavingExecutor;

impl InterleavingExecutor {
    pub fn new() -> Self {
        Self
    }

    pub fn execute_interleaving(
        &mut self,
        base: &mut BaseVerificationGenerator,
        cfg_program: &CfgProgram,
        interleaving: &super::Interleaving,
        label_suffix: &str,
    ) -> Results<()> {
        for &(slice_id, hop_id) in interleaving.iter() {
            // Guard condition s{slice}_active
            let active_name = format!("s{}_active", slice_id);
            let cond = crate::verification::Boogie::BoogieExpr {
                kind: crate::verification::Boogie::BoogieExprKind::Var(active_name.clone()),
            };

            let mut guarded_lines: Vec<Box<crate::verification::Boogie::BoogieLine>> = Vec::new();

            let hop = &cfg_program.hops[hop_id];
            for &block_id in hop.blocks.iter() {
                base.get_mut_scope().set_current_slice(slice_id);
                let base_label = base.get_mut_scope().get_scoped_label(block_id);
                let label = format!("{}__{}", base_label, label_suffix);
                guarded_lines.push(Box::new(crate::verification::Boogie::BoogieLine::Label(
                    label,
                )));

                let block = cfg_program.basic_blocks[block_id].clone();
                // Instructions
                for instr in &block.instructions {
                    let lines = base.convert_instruction(instr, slice_id, cfg_program)?;
                    for l in lines {
                        guarded_lines.push(Box::new(l));
                    }
                }

                // Terminator handling per spec
                match &block.terminator {
                    crate::cfg::Terminator::Jump(target) => {
                        base.get_mut_scope().set_current_slice(slice_id);
                        let base_target_label = base.get_mut_scope().get_scoped_label(*target);
                        let target_label = format!("{}__{}", base_target_label, label_suffix);
                        guarded_lines.push(Box::new(
                            crate::verification::Boogie::BoogieLine::Goto(target_label),
                        ));
                    }
                    crate::cfg::Terminator::Branch {
                        condition,
                        if_true,
                        if_false,
                    } => {
                        base.get_mut_scope().set_current_slice(slice_id);
                        let cond_name = match condition {
                            crate::cfg::Operand::Variable(var_id) => base
                                .get_mut_scope()
                                .get_scoped_variable_name(cfg_program, *var_id),
                            crate::cfg::Operand::Constant(_) => "".to_string(),
                            crate::cfg::Operand::Global(gid) => {
                                cfg_program.global_consts[*gid].name.clone()
                            }
                        };
                        let cond_expr =
                            base.generator
                                .convert_operand(cfg_program, condition, cond_name)?;
                        let t = {
                            base.get_mut_scope().set_current_slice(slice_id);
                            let bl = base.get_mut_scope().get_scoped_label(*if_true);
                            format!("{}__{}", bl, label_suffix)
                        };
                        let f = {
                            base.get_mut_scope().set_current_slice(slice_id);
                            let bl = base.get_mut_scope().get_scoped_label(*if_false);
                            format!("{}__{}", bl, label_suffix)
                        };
                        guarded_lines.push(Box::new(crate::verification::Boogie::BoogieLine::If {
                            cond: cond_expr,
                            then_body: vec![Box::new(
                                crate::verification::Boogie::BoogieLine::Goto(t),
                            )],
                            else_body: vec![Box::new(
                                crate::verification::Boogie::BoogieLine::Goto(f),
                            )],
                        }));
                    }
                    crate::cfg::Terminator::Return(_) => {
                        guarded_lines.push(Box::new(
                            crate::verification::Boogie::BoogieLine::Assign(
                                active_name.clone(),
                                crate::verification::Boogie::BoogieExpr {
                                    kind: crate::verification::Boogie::BoogieExprKind::BoolConst(
                                        false,
                                    ),
                                },
                            ),
                        ));
                    }
                    crate::cfg::Terminator::HopExit { .. } => {
                        // End of hop; do nothing special
                    }
                    crate::cfg::Terminator::Abort => {
                        guarded_lines.push(Box::new(
                            crate::verification::Boogie::BoogieLine::Assign(
                                active_name.clone(),
                                crate::verification::Boogie::BoogieExpr {
                                    kind: crate::verification::Boogie::BoogieExprKind::BoolConst(
                                        false,
                                    ),
                                },
                            ),
                        ));
                    }
                }
            }

            base.add_line(crate::verification::Boogie::BoogieLine::If {
                cond,
                then_body: guarded_lines,
                else_body: vec![],
            });
        }

        Ok(())
    }
}
