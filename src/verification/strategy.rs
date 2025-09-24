// In src/verification/strategy.rs

use super::{
    base_generator::BaseVerificationGenerator,
    errors::Results,
    scope::SliceId,
    Boogie::{BoogieLine, BoogieProcedure},
};
use crate::cfg::{BasicBlock, Function as FunctionCfg, Instruction, Program as CfgProgram};

pub trait VerificationStrategy {
    /// Provides access to the underlying base generator.
    fn base(&mut self) -> &mut BaseVerificationGenerator;

    // --- CUSTOMIZATION HOOKS (EVENTS) ---

    /// Called after an instruction has been converted and added to the procedure.
    /// This is the primary hook for inserting assertions.
    fn after_instruction(&mut self, instruction: &Instruction, slice_id: SliceId) -> Results<()>;

    /// Custom handler for generating a block's edges.
    /// The default implementation will call the base generator's default method.
    /// This allows strategies to easily override control flow.
    fn generate_block_edges(
        &mut self,
        block: &BasicBlock,
        slice_id: SliceId,
        cfg_program: &CfgProgram,
    ) -> Results<Vec<BoogieLine>> {
        // Default behavior: just use the base generator's logic.
        self.base()
            .default_generate_block_edges(block, slice_id, cfg_program)
    }

    // --- THE MAIN WORKFLOW METHOD ---

    /// The main entry point for running a verification.
    /// It walks the CFG and calls the hooks at the appropriate times.
    fn walk_and_verify(
        &mut self,
        function: &FunctionCfg,
        cfg_program: &CfgProgram,
    ) -> Results<BoogieProcedure> {
        // Standard CFG traversal loop
        if let Some(entry_hop) = function.entry_hop {
            let mut current_hop_id = entry_hop;
            loop {
                let (block_ids, next_hop_opt) = {
                    let hop = &cfg_program.hops[current_hop_id];
                    let last_block_id = *hop.blocks.last().unwrap();
                    let terminator = &cfg_program.basic_blocks[last_block_id].terminator;
                    let next_hop = if let crate::cfg::Terminator::HopExit { next_hop } = terminator
                    {
                        Some(*next_hop)
                    } else {
                        None
                    };
                    (hop.blocks.clone(), next_hop)
                };

                for &block_id in &block_ids {
                    let block = cfg_program.basic_blocks[block_id].clone();
                    let label;
                    {
                        label = self.base().get_mut_scope().get_scoped_label(block_id);
                    }
                    // Add the block's label
                    self.base().add_line(BoogieLine::Label(label));

                    // Process instructions
                    for instruction in &block.instructions {
                        // 1. Use the strategy's convert_instruction for default conversion
                        let boogie_lines = self.base().convert_instruction(
                            instruction,
                            current_hop_id.index(),
                            cfg_program,
                        )?;
                        self.base().add_lines(boogie_lines);

                        // 2. Call the strategy's AFTER hook for customization
                        self.after_instruction(instruction, current_hop_id.index())?;
                    }

                    // 3. Let the STRATEGY decide the control flow
                    let edge_lines =
                        self.generate_block_edges(&block, current_hop_id.index(), cfg_program)?;
                    self.base().add_lines(edge_lines);
                }

                if let Some(next_hop_id) = next_hop_opt {
                    current_hop_id = next_hop_id;
                } else {
                    break;
                }
            }
        }

        // ... return the completed procedure ...
        Ok(self
            .base()
            .generator
            .program
            .procedures
            .last()
            .unwrap()
            .clone())
    }
}
