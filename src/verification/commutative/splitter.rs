use super::{
    naming::{parse_commutative_edge_ids, split_child_program_name},
    CommutativeUnit, CommutativeVerificationManager, InstructionWindow,
};
use crate::cfg;
use crate::sc_graph::{EdgeType, SCGraph};
use crate::verification::Boogie::BoogieProgram;
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;

#[derive(Debug, Clone)]
pub struct SplitRuntimeOptions {
    pub enabled: bool,
    pub max_depth: u32,
    pub strategy: String,
    pub debug_cuts: bool,
}

#[derive(Default, Debug, Clone)]
pub struct SplitStatsSnapshot {
    pub total_units: usize,
    pub root_timeouts: usize,
    pub split_performed: usize,
    pub split_skipped_max_depth: usize,
    pub split_skipped_no_cutpoint: usize,
    pub split_budget_exhausted: usize,
    pub total_cuts_recorded: usize,
}

#[derive(Debug, Clone)]
pub struct AppliedCut {
    pub depth: u32,
    pub slice_id: usize,
    pub function_id: usize,
    pub block_id: usize,
    pub op_pos: usize,
}

#[derive(Debug, Clone)]
pub struct CommutativeTask {
    pub program: BoogieProgram,
    pub root_file_name: String,
    pub depth: u32,
    pub unit: CommutativeUnit,
    pub applied_cuts: Vec<AppliedCut>,
}

#[derive(Default)]
struct SplitStatsAtomic {
    total_units: AtomicUsize,
    root_timeouts: AtomicUsize,
    split_performed: AtomicUsize,
    split_skipped_max_depth: AtomicUsize,
    split_skipped_no_cutpoint: AtomicUsize,
    split_budget_exhausted: AtomicUsize,
    total_cuts_recorded: AtomicUsize,
}

impl SplitStatsAtomic {
    fn snapshot(&self) -> SplitStatsSnapshot {
        SplitStatsSnapshot {
            total_units: self.total_units.load(Ordering::Relaxed),
            root_timeouts: self.root_timeouts.load(Ordering::Relaxed),
            split_performed: self.split_performed.load(Ordering::Relaxed),
            split_skipped_max_depth: self.split_skipped_max_depth.load(Ordering::Relaxed),
            split_skipped_no_cutpoint: self.split_skipped_no_cutpoint.load(Ordering::Relaxed),
            split_budget_exhausted: self.split_budget_exhausted.load(Ordering::Relaxed),
            total_cuts_recorded: self.total_cuts_recorded.load(Ordering::Relaxed),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum SplitStrategy {
    MinState,
    Balanced,
    MinStateBalanced,
}

impl SplitStrategy {
    fn from_str(s: &str) -> Self {
        match s {
            "min-state" => SplitStrategy::MinState,
            "balanced" => SplitStrategy::Balanced,
            _ => SplitStrategy::MinStateBalanced,
        }
    }
}

#[derive(Debug, Clone)]
struct CandidateCut {
    slice_id: usize,
    block_id: cfg::BasicBlockId,
    op_pos: usize,
    score: (usize, usize, usize, usize),
    summary: String,
    candidate_log: String,
}

pub struct SplitterState {
    split_options: SplitRuntimeOptions,
    edge_map: HashMap<(usize, u32, usize, usize, u32, usize), crate::sc_graph::SCGraphEdge>,
    stats: SplitStatsAtomic,
    cut_history: Mutex<Vec<AppliedCut>>,
}

impl SplitterState {
    pub fn new(sc: &SCGraph, split_options: &SplitRuntimeOptions) -> Self {
        let edge_map = sc
            .edges
            .iter()
            .filter(|e| e.edge_type == EdgeType::C)
            .map(|e| {
                (
                    (
                        e.source.function_id.index(),
                        e.source.instance,
                        e.source.hop_id.index(),
                        e.target.function_id.index(),
                        e.target.instance,
                        e.target.hop_id.index(),
                    ),
                    e.clone(),
                )
            })
            .collect();

        Self {
            split_options: split_options.clone(),
            edge_map,
            stats: SplitStatsAtomic::default(),
            cut_history: Mutex::new(Vec::new()),
        }
    }

    pub fn create_root_task(&self, program: BoogieProgram) -> Option<CommutativeTask> {
        let root_file_name = format!("{}.bpl", program.name);
        let edge_ids = parse_commutative_edge_ids(&root_file_name)?;
        let edge = self.edge_map.get(&edge_ids)?;
        Some(CommutativeTask {
            program,
            root_file_name,
            depth: 0,
            unit: CommutativeVerificationManager::create_unit_for_edge(edge),
            applied_cuts: Vec::new(),
        })
    }

    pub fn stats(&self) -> Option<SplitStatsSnapshot> {
        if self.split_options.enabled {
            Some(self.stats.snapshot())
        } else {
            None
        }
    }

    pub fn record_total_unit(&self) {
        self.stats.total_units.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_root_timeout(&self) {
        self.stats.root_timeouts.fetch_add(1, Ordering::Relaxed);
    }

    pub fn split_strategy_name(&self) -> &str {
        &self.split_options.strategy
    }

    fn trace_all_cuts_enabled(&self) -> bool {
        if self.split_options.debug_cuts {
            return true;
        }
        match std::env::var("FMITF_SPLIT_TRACE_ALL_CUTS") {
            Ok(v) => {
                let v = v.to_ascii_lowercase();
                v == "1" || v == "true" || v == "yes" || v == "on"
            }
            Err(_) => false,
        }
    }

    pub fn recorded_cuts(&self) -> Vec<AppliedCut> {
        self.cut_history
            .lock()
            .map(|v| v.clone())
            .unwrap_or_default()
    }

    pub fn try_split_task(
        &self,
        cfg_program: &cfg::Program,
        task: &CommutativeTask,
    ) -> Result<(Option<Vec<CommutativeTask>>, Vec<String>), Box<dyn std::error::Error>> {
        let mut logs = Vec::new();
        if !self.split_options.enabled {
            return Ok((None, logs));
        }
        if task.depth >= self.split_options.max_depth {
            self.stats
                .split_skipped_max_depth
                .fetch_add(1, Ordering::Relaxed);
            logs.push(format!(
                "Split skipped for {} at depth {}: reached split_max_depth={}",
                task.program.name, task.depth, self.split_options.max_depth
            ));
            return Ok((None, logs));
        }

        let strategy = SplitStrategy::from_str(&self.split_options.strategy);
        let (sub_a, sub_b, cut, candidate_logs) =
            match choose_best_split(cfg_program, &task.unit, strategy) {
                Some(v) => v,
                None => {
                    self.stats
                        .split_skipped_no_cutpoint
                        .fetch_add(1, Ordering::Relaxed);
                    logs.push(format!(
                        "Split skipped for {} at depth {}: no valid cutpoint found",
                        task.program.name, task.depth
                    ));
                    return Ok((None, logs));
                }
            };

        if self.trace_all_cuts_enabled() {
            logs.push(format!(
                "Split candidate cuts for {} at depth {} using strategy={}",
                task.program.name, task.depth, self.split_options.strategy
            ));
            logs.extend(candidate_logs);
        }

        let p_a =
            CommutativeVerificationManager::build_boogie_program_for_unit(cfg_program, &sub_a)
                .map_err(|_| "Split sub-check generation failed")?;
        let p_b =
            CommutativeVerificationManager::build_boogie_program_for_unit(cfg_program, &sub_b)
                .map_err(|_| "Split sub-check generation failed")?;

        let applied_cut = AppliedCut {
            depth: task.depth,
            slice_id: cut.slice_id,
            function_id: cfg_program.basic_blocks[cut.block_id].function_id.index(),
            block_id: cut.block_id.index(),
            op_pos: cut.op_pos,
        };

        self.stats
            .total_cuts_recorded
            .fetch_add(1, Ordering::Relaxed);
        if let Ok(mut h) = self.cut_history.lock() {
            h.push(applied_cut.clone());
        }

        let mut out = Vec::new();
        if let Some(mut pa) = p_a {
            pa.name = split_child_program_name(&task.program.name, task.depth + 1, 'a');
            let mut cuts = task.applied_cuts.clone();
            cuts.push(applied_cut.clone());
            out.push(CommutativeTask {
                program: pa,
                root_file_name: task.root_file_name.clone(),
                depth: task.depth + 1,
                unit: sub_a,
                applied_cuts: cuts,
            });
        }
        if let Some(mut pb) = p_b {
            pb.name = split_child_program_name(&task.program.name, task.depth + 1, 'b');
            let mut cuts = task.applied_cuts.clone();
            cuts.push(applied_cut.clone());
            out.push(CommutativeTask {
                program: pb,
                root_file_name: task.root_file_name.clone(),
                depth: task.depth + 1,
                unit: sub_b,
                applied_cuts: cuts,
            });
        }

        if out.is_empty() {
            self.stats
                .split_skipped_no_cutpoint
                .fetch_add(1, Ordering::Relaxed);
            logs.push(format!(
                "Split skipped for {} at depth {}: no valid cutpoint found",
                task.program.name, task.depth
            ));
            return Ok((None, logs));
        }

        self.stats.split_performed.fetch_add(1, Ordering::Relaxed);
        logs.push(format!(
            "Split depth {} for {}: {}",
            task.depth, task.program.name, cut.summary
        ));

        if self.split_options.debug_cuts {
            logs.extend(debug_cut_context(
                cfg_program,
                &task.unit,
                cut.slice_id,
                cut.block_id,
                cut.op_pos,
            ));
        }

        Ok((Some(out), logs))
    }
}

fn current_blocks_for_slice(
    unit: &CommutativeUnit,
    slice: usize,
    program: &cfg::Program,
) -> Vec<cfg::BasicBlockId> {
    if let Some(v) = unit.blocks_per_slice.get(&slice) {
        return v.clone();
    }
    let hops = unit.hops_per_slice.get(&slice).cloned().unwrap_or_default();
    if hops.is_empty() {
        return Vec::new();
    }
    program.hops[hops[0]].blocks.clone()
}

fn instruction_window_for_block(
    unit: &CommutativeUnit,
    slice: usize,
    block_id: cfg::BasicBlockId,
    program: &cfg::Program,
) -> InstructionWindow {
    if let Some(slice_windows) = unit.instruction_windows_per_slice.get(&slice) {
        if let Some(window) = slice_windows.get(&block_id) {
            return *window;
        }
    }
    let len = program.basic_blocks[block_id].instructions.len();
    InstructionWindow::new(0, len)
}

fn table_access_signature(inst: &cfg::Instruction) -> Option<(cfg::TableId, bool)> {
    match &inst.kind {
        cfg::InstructionKind::TableGet { table, .. } => Some((*table, false)),
        cfg::InstructionKind::TableSet { table, .. } => Some((*table, true)),
        _ => None,
    }
}

fn db_rw_weight(inst: &cfg::Instruction) -> usize {
    match inst.kind {
        cfg::InstructionKind::TableGet { .. } | cfg::InstructionKind::TableSet { .. } => 1,
        _ => 0,
    }
}

fn cfg_successors_in_slice(
    program: &cfg::Program,
    block: cfg::BasicBlockId,
    in_slice: &HashSet<cfg::BasicBlockId>,
) -> Vec<cfg::BasicBlockId> {
    let succs = match &program.basic_blocks[block].terminator {
        cfg::Terminator::Jump(target) => vec![*target],
        cfg::Terminator::Branch {
            if_true, if_false, ..
        } => vec![*if_true, *if_false],
        _ => Vec::new(),
    };
    succs.into_iter().filter(|s| in_slice.contains(s)).collect()
}

fn blocks_in_cycles(
    program: &cfg::Program,
    blocks: &[cfg::BasicBlockId],
) -> HashSet<cfg::BasicBlockId> {
    let mut cycle_blocks = HashSet::new();
    if blocks.is_empty() {
        return cycle_blocks;
    }

    let block_set: HashSet<_> = blocks.iter().cloned().collect();
    let mut visited = HashSet::new();
    let mut finish_order = Vec::new();

    fn dfs_forward(
        program: &cfg::Program,
        node: cfg::BasicBlockId,
        block_set: &HashSet<cfg::BasicBlockId>,
        visited: &mut HashSet<cfg::BasicBlockId>,
        finish_order: &mut Vec<cfg::BasicBlockId>,
    ) {
        if !visited.insert(node) {
            return;
        }
        let succs = cfg_successors_in_slice(program, node, block_set);
        for succ in succs {
            dfs_forward(program, succ, block_set, visited, finish_order);
        }
        finish_order.push(node);
    }

    for &b in blocks {
        dfs_forward(program, b, &block_set, &mut visited, &mut finish_order);
    }

    let mut reverse_adj: HashMap<cfg::BasicBlockId, Vec<cfg::BasicBlockId>> = HashMap::new();
    for &b in blocks {
        reverse_adj.entry(b).or_default();
        for succ in cfg_successors_in_slice(program, b, &block_set) {
            reverse_adj.entry(succ).or_default().push(b);
        }
    }

    let mut assigned = HashSet::new();
    while let Some(node) = finish_order.pop() {
        if assigned.contains(&node) {
            continue;
        }
        let mut stack = vec![node];
        let mut component = Vec::new();
        assigned.insert(node);
        while let Some(cur) = stack.pop() {
            component.push(cur);
            if let Some(preds) = reverse_adj.get(&cur) {
                for &p in preds {
                    if assigned.insert(p) {
                        stack.push(p);
                    }
                }
            }
        }

        if component.len() > 1 {
            for c in component {
                cycle_blocks.insert(c);
            }
            continue;
        }

        let only = component[0];
        let has_self_loop = cfg_successors_in_slice(program, only, &block_set)
            .into_iter()
            .any(|s| s == only);
        if has_self_loop {
            cycle_blocks.insert(only);
        }
    }

    cycle_blocks
}

fn slice_operation_count(program: &cfg::Program, unit: &CommutativeUnit, slice: usize) -> usize {
    let blocks = current_blocks_for_slice(unit, slice, program);
    blocks
        .iter()
        .map(|&b| instruction_window_for_block(unit, slice, b, program).len())
        .sum()
}

fn cut_global_index(
    program: &cfg::Program,
    unit: &CommutativeUnit,
    slice: usize,
    blocks: &[cfg::BasicBlockId],
    cut_block: cfg::BasicBlockId,
    cut_pos: usize,
) -> usize {
    let mut acc = 0;
    for &b in blocks {
        let window = instruction_window_for_block(unit, slice, b, program);
        if b == cut_block {
            return acc + cut_pos.saturating_sub(window.start);
        }
        acc += window.len();
    }
    acc
}

#[derive(Default)]
struct SplitMetrics {
    pre_total_ops: usize,
    post_total_ops: usize,
    pre_db_ops: usize,
    post_db_ops: usize,
    pre_tables: HashSet<cfg::TableId>,
    post_tables: HashSet<cfg::TableId>,
}

fn compute_split_metrics(
    program: &cfg::Program,
    unit: &CommutativeUnit,
    slice: usize,
    blocks: &[cfg::BasicBlockId],
    cut_block: cfg::BasicBlockId,
    cut_pos: usize,
) -> SplitMetrics {
    let mut m = SplitMetrics::default();
    let mut before_cut = true;

    for &b in blocks {
        let window = instruction_window_for_block(unit, slice, b, program);
        let instructions = &program.basic_blocks[b].instructions;
        let start = window.start.min(instructions.len());
        let end = window.end.min(instructions.len());
        if start >= end {
            continue;
        }

        for i in start..end {
            if b == cut_block && i >= cut_pos {
                before_cut = false;
            }
            let inst = &instructions[i];
            if before_cut {
                m.pre_total_ops += 1;
                m.pre_db_ops += db_rw_weight(inst);
                if let Some((table, _)) = table_access_signature(inst) {
                    m.pre_tables.insert(table);
                }
            } else {
                m.post_total_ops += 1;
                m.post_db_ops += db_rw_weight(inst);
                if let Some((table, _)) = table_access_signature(inst) {
                    m.post_tables.insert(table);
                }
            }
        }

        if b == cut_block {
            before_cut = false;
        }
    }

    m
}

fn build_child_unit(
    program: &cfg::Program,
    unit: &CommutativeUnit,
    slice: usize,
    cut_block: cfg::BasicBlockId,
    cut_pos: usize,
    take_prefix: bool,
) -> CommutativeUnit {
    let mut child = unit.clone();
    let blocks = current_blocks_for_slice(unit, slice, program);
    let mut out_blocks = Vec::new();
    let mut out_windows: HashMap<cfg::BasicBlockId, InstructionWindow> = HashMap::new();

    for &b in &blocks {
        let base_window = instruction_window_for_block(unit, slice, b, program);
        let adjusted = if b == cut_block {
            if take_prefix {
                InstructionWindow::new(base_window.start, cut_pos)
            } else {
                InstructionWindow::new(cut_pos, base_window.end)
            }
        } else {
            let is_before = blocks.iter().position(|bb| *bb == b).unwrap_or(usize::MAX)
                < blocks
                    .iter()
                    .position(|bb| *bb == cut_block)
                    .unwrap_or(usize::MAX);
            if (take_prefix && is_before) || (!take_prefix && !is_before) {
                base_window
            } else {
                InstructionWindow::new(0, 0)
            }
        };

        if adjusted.len() > 0 {
            out_blocks.push(b);
            out_windows.insert(b, adjusted);
        }
    }

    child.blocks_per_slice.insert(slice, out_blocks);
    child
        .instruction_windows_per_slice
        .insert(slice, out_windows);
    child
}

fn choose_best_split(
    program: &cfg::Program,
    unit: &CommutativeUnit,
    strategy: SplitStrategy,
) -> Option<(CommutativeUnit, CommutativeUnit, CandidateCut, Vec<String>)> {
    let ops_a = slice_operation_count(program, unit, 0);
    let ops_b = slice_operation_count(program, unit, 1);
    let split_slice = if ops_a >= ops_b { 0 } else { 1 };

    let blocks = current_blocks_for_slice(unit, split_slice, program);
    if blocks.is_empty() {
        return None;
    }

    let total_ops = slice_operation_count(program, unit, split_slice);
    if total_ops < 2 {
        return None;
    }

    let cycle_blocks = blocks_in_cycles(program, &blocks);
    let mut candidates = Vec::new();

    for &block_id in &blocks {
        if cycle_blocks.contains(&block_id) {
            continue;
        }

        let window = instruction_window_for_block(unit, split_slice, block_id, program);
        if window.len() < 2 {
            continue;
        }

        let max_pos = window
            .end
            .min(program.basic_blocks[block_id].instructions.len());
        let min_pos = window.start.saturating_add(1);
        if min_pos >= max_pos {
            continue;
        }

        for op_pos in min_pos..max_pos {
            let metrics =
                compute_split_metrics(program, unit, split_slice, &blocks, block_id, op_pos);
            if metrics.pre_total_ops == 0 || metrics.post_total_ops == 0 {
                continue;
            }

            let shared_tables = metrics
                .pre_tables
                .intersection(&metrics.post_tables)
                .count();

            let db_balance =
                (metrics.pre_db_ops as isize - metrics.post_db_ops as isize).unsigned_abs();
            let op_balance =
                (metrics.pre_total_ops as isize - metrics.post_total_ops as isize).unsigned_abs();
            let balance = if metrics.pre_db_ops + metrics.post_db_ops > 0 {
                db_balance
            } else {
                op_balance
            };

            let global_index =
                cut_global_index(program, unit, split_slice, &blocks, block_id, op_pos);
            let mid_bias = (total_ops as isize / 2 - global_index as isize).unsigned_abs();

            let score = match strategy {
                SplitStrategy::MinState => (shared_tables, balance, mid_bias, global_index),
                SplitStrategy::Balanced => (balance, shared_tables, mid_bias, global_index),
                SplitStrategy::MinStateBalanced => (shared_tables, balance, mid_bias, global_index),
            };

            let strategy_name = match strategy {
                SplitStrategy::MinState => "min-state",
                SplitStrategy::Balanced => "balanced",
                SplitStrategy::MinStateBalanced => "min-state-balanced",
            };

            let summary = format!(
                "slice={} function={} bb={} op={} score=({},{},{},{}) shared_tables={} pre_ops={} post_ops={} pre_db={} post_db={}",
                split_slice,
                program.basic_blocks[block_id].function_id.index(),
                block_id.index(),
                op_pos,
                score.0,
                score.1,
                score.2,
                score.3,
                shared_tables,
                metrics.pre_total_ops,
                metrics.post_total_ops,
                metrics.pre_db_ops,
                metrics.post_db_ops
            );

            let candidate_log = format!(
                "  candidate {} strategy={} non_loop=true",
                summary, strategy_name
            );

            candidates.push(CandidateCut {
                slice_id: split_slice,
                block_id,
                op_pos,
                score,
                summary,
                candidate_log,
            });
        }
    }

    let best = candidates.into_iter().min_by_key(|c| c.score)?;
    let sub_a = build_child_unit(
        program,
        unit,
        best.slice_id,
        best.block_id,
        best.op_pos,
        true,
    );
    let sub_b = build_child_unit(
        program,
        unit,
        best.slice_id,
        best.block_id,
        best.op_pos,
        false,
    );

    let mut candidate_logs = vec![format!(
        "non-loop blocks considered on slice {}: {}",
        split_slice,
        blocks
            .iter()
            .filter(|b| !cycle_blocks.contains(b))
            .map(|b| b.index().to_string())
            .collect::<Vec<_>>()
            .join(",")
    )];
    candidate_logs.push(best.candidate_log.clone());

    Some((sub_a, sub_b, best, candidate_logs))
}

fn debug_cut_context(
    program: &cfg::Program,
    unit: &CommutativeUnit,
    slice: usize,
    block_id: cfg::BasicBlockId,
    op_pos: usize,
) -> Vec<String> {
    let mut lines = Vec::new();
    let block = &program.basic_blocks[block_id];
    let window = instruction_window_for_block(unit, slice, block_id, program);
    let start = window.start.min(block.instructions.len());
    let end = window.end.min(block.instructions.len());

    lines.push(format!(
        "Split debug: function={} slice={} bb={} window=[{}, {}) cut_op_pos={}",
        block.function_id.index(),
        slice,
        block_id.index(),
        start,
        end,
        op_pos
    ));

    let ctx_start = op_pos.saturating_sub(2).max(start);
    let ctx_end = (op_pos + 2).min(end.saturating_sub(1));
    for i in ctx_start..=ctx_end {
        let marker = if i < op_pos { "pre" } else { "post" };
        lines.push(format!(
            "  {} op[{}]: {:?}",
            marker, i, block.instructions[i].kind
        ));
    }

    lines
}
