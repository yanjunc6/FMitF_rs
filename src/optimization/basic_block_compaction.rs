//! Basic block compaction pass
//!
//! Ensures every basic block in a CFG function is maximal by merging
//! consecutive linear blocks (i.e., a block that unconditionally jumps to
//! another block which has a single predecessor). Hop boundaries are
//! treated as hard barriers and will never be crossed.

use super::OptimizationPass;
use crate::cfg::{BasicBlockId, FunctionId, FunctionKind, Program, Terminator};
use std::mem;

/// Merges linear chains of basic blocks so each block is maximal.
pub struct BasicBlockCompactionPass;

impl BasicBlockCompactionPass {
    pub fn new() -> Self {
        Self
    }

    /// Try to merge `block_id` with its jump target if possible.
    fn try_merge_block(program: &mut Program, func_id: FunctionId, block_id: BasicBlockId) -> bool {
        let target = match program.basic_blocks[block_id].terminator {
            Terminator::Jump(target) => target,
            _ => return false,
        };

        if block_id == target {
            return false; // Self-loop, nothing to merge
        }

        // Target must still be part of this function.
        if {
            let function = &program.functions[func_id];
            !function.all_blocks.contains(&target)
        } {
            return false;
        }

        // Can't merge across hops.
        if program.basic_blocks[block_id].hop_id != program.basic_blocks[target].hop_id {
            return false;
        }

        // Skip function or hop entry blocks.
        if program.functions[func_id].entry_block == Some(target) {
            return false;
        }
        let hop_id = program.basic_blocks[target].hop_id;
        if program.hops[hop_id].entry_block == Some(target) {
            return false;
        }

        let preds = &program.basic_blocks[target].predecessors;
        if preds.len() != 1 || preds[0] != block_id {
            return false;
        }

        let (target_insts, target_term) = {
            let target_block = &mut program.basic_blocks[target];
            let insts = mem::take(&mut target_block.instructions);
            let term = target_block.terminator.clone();
            (insts, term)
        };

        let successors = terminator_successors(&target_term);

        let block = &mut program.basic_blocks[block_id];
        block.instructions.extend(target_insts);
        block.terminator = target_term.clone();

        for succ_id in successors {
            let preds = &mut program.basic_blocks[succ_id].predecessors;
            for pred in preds.iter_mut() {
                if *pred == target {
                    *pred = block_id;
                }
            }
        }

        let func_blocks = &mut program.functions[func_id].all_blocks;
        if let Some(pos) = func_blocks.iter().position(|&id| id == target) {
            func_blocks.remove(pos);
        }

        let hop_blocks = &mut program.hops[hop_id].blocks;
        if let Some(pos) = hop_blocks.iter().position(|&id| id == target) {
            hop_blocks.remove(pos);
        }

        let target_block = &mut program.basic_blocks[target];
        target_block.terminator = Terminator::Abort;
        target_block.predecessors.clear();

        true
    }
}

impl OptimizationPass for BasicBlockCompactionPass {
    fn name(&self) -> &'static str {
        "basic-block-compaction"
    }

    fn optimize_function(&self, program: &mut Program, func_id: FunctionId) -> bool {
        if matches!(program.functions[func_id].kind, FunctionKind::Operator) {
            return false;
        }

        let mut changed = false;
        'outer: loop {
            let block_ids = {
                let function = &program.functions[func_id];
                function.all_blocks.clone()
            };

            for block_id in block_ids {
                let still_present = {
                    let function = &program.functions[func_id];
                    function.all_blocks.contains(&block_id)
                };
                if !still_present {
                    continue;
                }

                if Self::try_merge_block(program, func_id, block_id) {
                    changed = true;
                    continue 'outer;
                }
            }

            break;
        }

        changed
    }
}

fn terminator_successors(term: &Terminator) -> Vec<BasicBlockId> {
    match term {
        Terminator::Jump(target) => vec![*target],
        Terminator::Branch {
            if_true, if_false, ..
        } => vec![*if_true, *if_false],
        Terminator::Return(_) | Terminator::HopExit { .. } | Terminator::Abort => Vec::new(),
    }
}
