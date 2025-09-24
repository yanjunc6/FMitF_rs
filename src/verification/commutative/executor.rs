use super::{CommutativeUnit, Interleaving};
use crate::cfg::Program as CfgProgram;
use crate::verification::base_generator::BaseVerificationGenerator;
use crate::verification::errors::Results;
use crate::verification::scope::SliceId;
use crate::verification::Boogie::{BoogieExpr, BoogieExprKind, BoogieLine};

/// Executes a single interleaving path with per-slice active flags.
/// - Introduces boolean flags s{slice}_active.
/// - Wraps generated code for each hop under if (s{slice}_active) { ... }.
/// - Abort/Return set s{slice}_active := false instead of jumping to end.
/// - HopExit only ends the current hop; the executor proceeds to the next hop in the interleaving sequence.
pub struct InterleavingExecutor;

impl InterleavingExecutor {
    pub fn new() -> Self {
        Self
    }

    pub fn init_active_flags(
        &mut self,
        base: &mut BaseVerificationGenerator,
        unit: &CommutativeUnit,
    ) {
        // Create/ensure local bool vars for each slice: s{slice}_active
        for (&slice_id, _) in &unit.hops_per_slice {
            let var_name = format!("s{}_active", slice_id);
            base.generator.ensure_local_variable_exists(
                &var_name,
                crate::verification::Boogie::BoogieType::Bool,
            );
            base.add_line(BoogieLine::Assign(
                var_name,
                BoogieExpr {
                    kind: BoogieExprKind::BoolConst(true),
                },
            ));
        }
    }

    pub fn execute_interleaving(
        &mut self,
        base: &mut BaseVerificationGenerator,
        cfg_program: &CfgProgram,
        interleaving: &Interleaving,
    ) -> Results<()> {
        // For each (slice_id, hop_id), emit guarded execution
        for &(slice_id, hop_id) in interleaving.iter() {
            // Guard begin
            let active_name = format!("s{}_active", slice_id);
            let cond = BoogieExpr {
                kind: BoogieExprKind::Var(active_name.clone()),
            };

            // Inside the then-body we emit the hop's basic blocks and edges, but:
            // - Replace Abort/Return handling by setting s{slice}_active := false
            // - HopExit ends hop execution (no cross-hop gotos)
            // We'll emit content into a temporary vector and wrap it in an If.
            let mut guarded_lines: Vec<Box<BoogieLine>> = Vec::new();

            // Walk the basic blocks in this hop
            let hop = &cfg_program.hops[hop_id];
            for &block_id in hop.blocks.iter() {
                // Label
                base.get_mut_scope().set_current_slice(slice_id);
                let label = base.get_mut_scope().get_scoped_label(block_id);
                guarded_lines.push(Box::new(BoogieLine::Label(label)));

                let block = cfg_program.basic_blocks[block_id].clone();
                // Instructions
                for instr in &block.instructions {
                    let lines = base.convert_instruction(instr, slice_id, cfg_program)?;
                    for l in lines {
                        guarded_lines.push(Box::new(l));
                    }
                }

                // Terminator override per spec
                match &block.terminator {
                    crate::cfg::Terminator::Jump(target) => {
                        base.get_mut_scope().set_current_slice(slice_id);
                        let target_label = base.get_mut_scope().get_scoped_label(*target);
                        guarded_lines.push(Box::new(BoogieLine::Goto(target_label)));
                    }
                    crate::cfg::Terminator::Branch {
                        condition,
                        if_true,
                        if_false,
                    } => {
                        // Build the proper variable name for the condition based on slice scope
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
                            base.get_mut_scope().get_scoped_label(*if_true)
                        };
                        let f = {
                            base.get_mut_scope().set_current_slice(slice_id);
                            base.get_mut_scope().get_scoped_label(*if_false)
                        };
                        guarded_lines.push(Box::new(BoogieLine::If {
                            cond: cond_expr,
                            then_body: vec![Box::new(BoogieLine::Goto(t))],
                            else_body: vec![Box::new(BoogieLine::Goto(f))],
                        }));
                    }
                    crate::cfg::Terminator::Return(_ret) => {
                        // s{slice}_active := false
                        guarded_lines.push(Box::new(BoogieLine::Assign(
                            active_name.clone(),
                            BoogieExpr {
                                kind: BoogieExprKind::BoolConst(false),
                            },
                        )));
                        // Do not jump anywhere; next interleaving hop will proceed
                    }
                    crate::cfg::Terminator::HopExit { .. } => {
                        // End of hop; do not follow to next hop here
                    }
                    crate::cfg::Terminator::Abort => {
                        // s{slice}_active := false
                        guarded_lines.push(Box::new(BoogieLine::Assign(
                            active_name.clone(),
                            BoogieExpr {
                                kind: BoogieExprKind::BoolConst(false),
                            },
                        )));
                    }
                }
            }

            // Wrap in If(s{slice}_active)
            base.add_line(BoogieLine::If {
                cond,
                then_body: guarded_lines,
                else_body: vec![],
            });
        }

        Ok(())
    }
}
