use crate::cfg::{FunctionId, HopId, Program as CfgProgram};
use crate::sc_graph::{EdgeType, SCGraph, SCGraphEdge};
use crate::verification::base_generator::BaseVerificationGenerator;
use crate::verification::errors::Results;
use crate::verification::scope::SliceId;
use crate::verification::Boogie::{self, BoogieProcedure};
use rayon::prelude::*;
use std::collections::{HashMap, HashSet};

mod boogie_helpers;
mod interleaving_generator;
pub mod naming;
mod slice_analyzer;
pub mod splitter;
mod strategy;

// Re-exported modules used by strategy
use strategy::CommutativeStrategy;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InstructionWindow {
    pub start: usize,
    pub end: usize,
}

impl InstructionWindow {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn len(&self) -> usize {
        self.end.saturating_sub(self.start)
    }
}

#[derive(Debug, Clone)]
pub struct CommutativeUnit {
    pub c_edge: SCGraphEdge,
    /// Continuous hops per slice; slice 0 corresponds to c_edge.source, slice 1 to c_edge.target
    pub hops_per_slice: HashMap<SliceId, Vec<HopId>>,
    pub func_per_slice: HashMap<SliceId, FunctionId>,
    /// Optional block subset per slice for recursive splitting.
    /// If absent, the whole hop blocks are used.
    pub blocks_per_slice: HashMap<SliceId, Vec<crate::cfg::BasicBlockId>>,
    /// Optional instruction windows per block within each slice.
    /// If absent for a block, the full block instruction range is used.
    pub instruction_windows_per_slice:
        HashMap<SliceId, HashMap<crate::cfg::BasicBlockId, InstructionWindow>>,
}

/// Represents an interleaving as a sequence of (slice_id, hop_id)
pub type Interleaving = Vec<(SliceId, HopId)>;

pub struct CommutativeVerificationManager {
    pub commutative_units: Vec<CommutativeUnit>,
}

impl CommutativeVerificationManager {
    pub fn new() -> Self {
        CommutativeVerificationManager {
            commutative_units: Vec::new(),
        }
    }

    pub fn create_unit_for_edge(edge: &SCGraphEdge) -> CommutativeUnit {
        let mut unit = CommutativeUnit {
            c_edge: edge.clone(),
            hops_per_slice: HashMap::new(),
            func_per_slice: HashMap::new(),
            blocks_per_slice: HashMap::new(),
            instruction_windows_per_slice: HashMap::new(),
        };
        unit.hops_per_slice.insert(0, vec![edge.source.hop_id]);
        unit.hops_per_slice.insert(1, vec![edge.target.hop_id]);
        unit.func_per_slice.insert(0, edge.source.function_id);
        unit.func_per_slice.insert(1, edge.target.function_id);
        unit
    }

    /// Create simple commutative units from the SC-graph with only the hops on the ends
    pub fn create_simple_commutative_units(&mut self, sc_graph: &SCGraph) {
        // Analyze the SC-graph to identify commutative units
        for edge in &sc_graph.edges {
            if edge.edge_type == EdgeType::C {
                self.commutative_units
                    .push(Self::create_unit_for_edge(edge));
            }
        }
    }

    pub fn build_boogie_program_for_unit(
        cfg_program: &CfgProgram,
        unit: &CommutativeUnit,
    ) -> Results<Option<Boogie::BoogieProgram>> {
        let slice0 = unit.hops_per_slice.get(&0).cloned().unwrap_or_default();
        let slice1 = unit.hops_per_slice.get(&1).cloned().unwrap_or_default();

        if slice0.is_empty() || slice1.is_empty() {
            return Ok(None);
        }

        let mut unit_base = BaseVerificationGenerator::new(cfg_program);
        unit_base.generator.program.name = format!(
            "commutative_f{}_i{}_h{}_vs_f{}_i{}_h{}",
            unit.c_edge.source.function_id.index(),
            unit.c_edge.source.instance,
            unit.c_edge.source.hop_id.index(),
            unit.c_edge.target.function_id.index(),
            unit.c_edge.target.instance,
            unit.c_edge.target.hop_id.index()
        );

        let procedure =
            Self::create_commutative_verification_procedure(&mut unit_base, cfg_program, unit)?;
        unit_base.generator.program.procedures.push(procedure);
        let mut program = unit_base.generator.program;
        program.stats = compute_commutative_program_stats(cfg_program, unit, &program);
        Ok(Some(program))
    }

    /// Generate Commutative (Slice Commutativity) verification Boogie programs
    /// Each pair of conflicting transaction slices gets its own verification
    pub fn generate_commutative_verification(
        &mut self,
        cfg_program: &CfgProgram,
        sc_graph: &SCGraph,
    ) -> Results<Vec<Boogie::BoogieProgram>> {
        // Create simple commutative units from the SC-graph
        self.create_simple_commutative_units(sc_graph);

        // Process units in parallel using rayon
        let results: Result<
            Vec<Option<Boogie::BoogieProgram>>,
            Vec<crate::util::CompilerError>,
        > = self
            .commutative_units
            .par_iter()
            .map(|unit| -> Result<
                Option<Boogie::BoogieProgram>,
                Vec<crate::util::CompilerError>,
            > {
                Self::build_boogie_program_for_unit(cfg_program, unit)
            })
            .collect();

        // Filter out None values and collect results
        let programs = results?.into_iter().filter_map(|opt| opt).collect();

        Ok(programs)
    }

    /// Create a procedure to verify slice commutativity
    fn create_commutative_verification_procedure(
        base: &mut BaseVerificationGenerator,
        cfg_program: &CfgProgram,
        unit: &CommutativeUnit,
    ) -> Results<BoogieProcedure> {
        let mut strat = CommutativeStrategy::new(base, cfg_program, unit);
        strat.run()
    }
}

fn compute_commutative_program_stats(
    cfg_program: &CfgProgram,
    unit: &CommutativeUnit,
    program: &Boogie::BoogieProgram,
) -> Boogie::BoogieProgramStats {
    let mut stats = Boogie::BoogieProgramStats::default();

    let mut tmp_program = program.clone();
    tmp_program.refresh_text_length_stats();
    stats.total_boogie_file_len = tmp_program.stats.total_boogie_file_len;
    stats.real_procedure_len = tmp_program.stats.real_procedure_len;

    for slice_id in [0usize, 1usize] {
        let blocks = selected_blocks_for_slice(unit, slice_id, cfg_program);
        if has_cycle(cfg_program, &blocks) {
            stats.has_loop = true;
        }

        for block_id in &blocks {
            let block = &cfg_program.basic_blocks[*block_id];
            if matches!(block.terminator, crate::cfg::Terminator::Branch { .. }) {
                stats.branch_count += 1;
            }

            let (start, end) = selected_instruction_window(unit, slice_id, *block_id, block);
            for inst in &block.instructions[start..end] {
                match inst.kind {
                    crate::cfg::InstructionKind::TableGet { .. } => stats.db_read_count += 1,
                    crate::cfg::InstructionKind::TableSet { .. } => stats.db_write_count += 1,
                    _ => {}
                }
            }
        }
    }

    stats
}

fn selected_blocks_for_slice(
    unit: &CommutativeUnit,
    slice_id: usize,
    cfg_program: &CfgProgram,
) -> Vec<crate::cfg::BasicBlockId> {
    if let Some(blocks) = unit.blocks_per_slice.get(&slice_id) {
        return blocks.clone();
    }

    unit.hops_per_slice
        .get(&slice_id)
        .cloned()
        .unwrap_or_default()
        .into_iter()
        .flat_map(|hop_id| cfg_program.hops[hop_id].blocks.clone())
        .collect()
}

fn selected_instruction_window(
    unit: &CommutativeUnit,
    slice_id: usize,
    block_id: crate::cfg::BasicBlockId,
    block: &crate::cfg::BasicBlock,
) -> (usize, usize) {
    let full_end = block.instructions.len();
    if let Some(windows_by_block) = unit.instruction_windows_per_slice.get(&slice_id) {
        if let Some(window) = windows_by_block.get(&block_id) {
            let start = window.start.min(full_end);
            let end = window.end.min(full_end).max(start);
            return (start, end);
        }
    }
    (0, full_end)
}

fn has_cycle(cfg_program: &CfgProgram, blocks: &[crate::cfg::BasicBlockId]) -> bool {
    let block_set: HashSet<_> = blocks.iter().copied().collect();
    let mut visiting = HashSet::new();
    let mut visited = HashSet::new();

    fn dfs(
        cfg_program: &CfgProgram,
        block_set: &HashSet<crate::cfg::BasicBlockId>,
        node: crate::cfg::BasicBlockId,
        visiting: &mut HashSet<crate::cfg::BasicBlockId>,
        visited: &mut HashSet<crate::cfg::BasicBlockId>,
    ) -> bool {
        if visited.contains(&node) {
            return false;
        }
        if !visiting.insert(node) {
            return true;
        }

        let successors: Vec<crate::cfg::BasicBlockId> =
            match cfg_program.basic_blocks[node].terminator {
                crate::cfg::Terminator::Jump(target) => vec![target],
                crate::cfg::Terminator::Branch {
                    if_true, if_false, ..
                } => vec![if_true, if_false],
                _ => Vec::new(),
            };

        for succ in successors {
            if !block_set.contains(&succ) {
                continue;
            }
            if visiting.contains(&succ) {
                return true;
            }
            if dfs(cfg_program, block_set, succ, visiting, visited) {
                return true;
            }
        }

        visiting.remove(&node);
        visited.insert(node);
        false
    }

    for block_id in blocks {
        if dfs(
            cfg_program,
            &block_set,
            *block_id,
            &mut visiting,
            &mut visited,
        ) {
            return true;
        }
    }
    false
}
