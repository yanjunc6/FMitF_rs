use super::{CommutativeUnit, CommutativeVerificationManager};
use crate::cfg;
use crate::sc_graph::{EdgeType, SCGraph};
use crate::verification::Boogie::{BoogieProgram, VerificationNodeId};
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug, Clone)]
pub struct SplitRuntimeOptions {
    pub enabled: bool,
    pub max_depth: u32,
    pub strategy: String,
}

#[derive(Default, Debug, Clone)]
pub struct SplitStatsSnapshot {
    pub total_units: usize,
    pub root_timeouts: usize,
    pub split_performed: usize,
    pub split_skipped_max_depth: usize,
    pub split_skipped_no_cutpoint: usize,
    pub split_budget_exhausted: usize,
}

#[derive(Debug, Clone)]
pub struct CommutativeTask {
    pub program: BoogieProgram,
    pub root_file_name: String,
    pub depth: u32,
}

#[derive(Default)]
struct SplitStatsAtomic {
    total_units: AtomicUsize,
    root_timeouts: AtomicUsize,
    split_performed: AtomicUsize,
    split_skipped_max_depth: AtomicUsize,
    split_skipped_no_cutpoint: AtomicUsize,
    split_budget_exhausted: AtomicUsize,
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

pub struct SplitterState {
    split_options: SplitRuntimeOptions,
    edge_map: HashMap<(usize, u32, usize, usize, u32, usize), crate::sc_graph::SCGraphEdge>,
    stats: SplitStatsAtomic,
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
        }
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

    fn trace_all_cuts_enabled() -> bool {
        match std::env::var("FMITF_SPLIT_TRACE_ALL_CUTS") {
            Ok(v) => {
                let v = v.to_ascii_lowercase();
                v == "1" || v == "true" || v == "yes" || v == "on"
            }
            Err(_) => false,
        }
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

        let ids = match parse_commutative_edge_ids(&task.root_file_name) {
            Some(v) => v,
            None => return Ok((None, logs)),
        };
        let edge = match self.edge_map.get(&ids) {
            Some(e) => e,
            None => return Ok((None, logs)),
        };

        let unit = CommutativeVerificationManager::create_unit_for_edge(edge);
        let strategy = SplitStrategy::from_str(&self.split_options.strategy);
        let split = choose_best_split(cfg_program, &unit, strategy);
        let (sub_a, sub_b, summary, candidate_logs) = match split {
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

        if Self::trace_all_cuts_enabled() {
            logs.push(format!(
                "Split candidate cuts for {} at depth {} using strategy={}",
                task.program.name, task.depth, self.split_options.strategy
            ));
            logs.extend(candidate_logs);
        }

        let p_a = CommutativeVerificationManager::build_boogie_program_for_unit(cfg_program, &sub_a)
            .map_err(|_| "Split sub-check generation failed")?;
        let p_b = CommutativeVerificationManager::build_boogie_program_for_unit(cfg_program, &sub_b)
            .map_err(|_| "Split sub-check generation failed")?;

        let mut out = Vec::new();
        if let Some(mut pa) = p_a {
            pa.name = format!("{}__split_d{}_a", task.program.name, task.depth + 1);
            out.push(CommutativeTask {
                program: pa,
                root_file_name: task.root_file_name.clone(),
                depth: task.depth + 1,
            });
        }
        if let Some(mut pb) = p_b {
            pb.name = format!("{}__split_d{}_b", task.program.name, task.depth + 1);
            out.push(CommutativeTask {
                program: pb,
                root_file_name: task.root_file_name.clone(),
                depth: task.depth + 1,
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
            task.depth, task.program.name, summary
        ));
        Ok((Some(out), logs))
    }
}

fn parse_commutative_edge_ids(file_name: &str) -> Option<(usize, u32, usize, usize, u32, usize)> {
    let trimmed = file_name.strip_suffix(".bpl").unwrap_or(file_name);
    let canonical = trimmed.split("__split_").next().unwrap_or(trimmed);
    if !canonical.starts_with("commutative_") {
        return None;
    }

    let parts: Vec<&str> = canonical.split('_').collect();
    if parts.len() != 8 || parts.get(4).copied() != Some("vs") {
        return None;
    }

    let source = parse_node_from_parts(parts[1], parts[2], parts[3])?;
    let target = parse_node_from_parts(parts[5], parts[6], parts[7])?;

    Some((
        source.function_id,
        source.instance,
        source.hop_id,
        target.function_id,
        target.instance,
        target.hop_id,
    ))
}

fn parse_node_from_parts(f_part: &str, i_part: &str, h_part: &str) -> Option<VerificationNodeId> {
    let function_id = f_part.strip_prefix('f')?.parse().ok()?;
    let instance = i_part.strip_prefix('i')?.parse().ok()?;
    let hop_id = h_part.strip_prefix('h')?.parse().ok()?;

    Some(VerificationNodeId {
        function_id,
        instance,
        hop_id,
    })
}


fn current_blocks_for_slice(unit: &CommutativeUnit, slice: usize, program: &cfg::Program) -> Vec<cfg::BasicBlockId> {
    if let Some(v) = unit.blocks_per_slice.get(&slice) {
        return v.clone();
    }
    let hops = unit.hops_per_slice.get(&slice).cloned().unwrap_or_default();
    if hops.is_empty() {
        return Vec::new();
    }
    program.hops[hops[0]].blocks.clone()
}

fn terminator_successors(term: &cfg::Terminator) -> Vec<cfg::BasicBlockId> {
    match term {
        cfg::Terminator::Jump(target) => vec![*target],
        cfg::Terminator::Branch { if_true, if_false, .. } => vec![*if_true, *if_false],
        _ => Vec::new(),
    }
}

fn collect_operand_tables(op: &cfg::Operand, out: &mut HashSet<cfg::TableId>) {
    if let cfg::Operand::Table(t) = op {
        out.insert(*t);
    }
}

fn collect_table_access(inst: &cfg::Instruction, out: &mut HashSet<cfg::TableId>) {
    match &inst.kind {
        cfg::InstructionKind::Assign { src, .. } => collect_operand_tables(src, out),
        cfg::InstructionKind::BinaryOp { left, right, .. } => {
            collect_operand_tables(left, out);
            collect_operand_tables(right, out);
        }
        cfg::InstructionKind::UnaryOp { operand, .. } => collect_operand_tables(operand, out),
        cfg::InstructionKind::Call { args, .. } => {
            for a in args {
                collect_operand_tables(a, out);
            }
        }
        cfg::InstructionKind::TableGet { table, keys, .. } => {
            out.insert(*table);
            for k in keys {
                collect_operand_tables(k, out);
            }
        }
        cfg::InstructionKind::TableSet { table, keys, value, .. } => {
            out.insert(*table);
            for k in keys {
                collect_operand_tables(k, out);
            }
            collect_operand_tables(value, out);
        }
        cfg::InstructionKind::Assert { condition, .. } => collect_operand_tables(condition, out),
    }
}

fn choose_best_split(
    program: &cfg::Program,
    unit: &CommutativeUnit,
    strategy: SplitStrategy,
) -> Option<(CommutativeUnit, CommutativeUnit, String, Vec<String>)> {
    let blocks_a = current_blocks_for_slice(unit, 0, program);
    let blocks_b = current_blocks_for_slice(unit, 1, program);

    let split_slice = if blocks_a.len() >= blocks_b.len() { 0 } else { 1 };
    let blocks = if split_slice == 0 { blocks_a } else { blocks_b };
    if blocks.len() < 3 {
        return None;
    }

    let mut pos = HashMap::new();
    for (i, b) in blocks.iter().enumerate() {
        pos.insert(*b, i);
    }

    let mut loop_headers: HashSet<cfg::BasicBlockId> = HashSet::new();
    let mut back_edges: Vec<(usize, usize)> = Vec::new();
    for src in &blocks {
        for succ in terminator_successors(&program.basic_blocks[*src].terminator) {
            if let (Some(&s), Some(&t)) = (pos.get(src), pos.get(&succ)) {
                if s >= t {
                    loop_headers.insert(succ);
                    back_edges.push((s, t));
                }
            }
        }
    }

    let mut best: Option<(usize, (usize, usize, usize))> = None;
    let mut candidate_logs: Vec<String> = Vec::new();
    for cut_idx in 1..(blocks.len() - 1) {
        let cut = blocks[cut_idx];
        let pred_in: Vec<_> = program.basic_blocks[cut]
            .predecessors
            .iter()
            .filter(|p| pos.contains_key(p))
            .cloned()
            .collect();
        if pred_in.len() != 1 {
            continue;
        }

        let succ_in: Vec<_> = terminator_successors(&program.basic_blocks[cut].terminator)
            .into_iter()
            .filter(|s| pos.contains_key(s))
            .collect();
        if succ_in.len() != 1 {
            continue;
        }

        let pred = pred_in[0];
        let pred_succ_in: Vec<_> = terminator_successors(&program.basic_blocks[pred].terminator)
            .into_iter()
            .filter(|s| pos.contains_key(s))
            .collect();
        if pred_succ_in.len() != 1 {
            continue;
        }

        if loop_headers.contains(&cut) {
            continue;
        }

        let crosses_back_edge = back_edges.iter().any(|(s, t)| *s >= cut_idx && *t < cut_idx);
        if crosses_back_edge {
            continue;
        }

        let prefix = &blocks[..cut_idx];
        let suffix = &blocks[cut_idx..];

        let mut pre_tables = HashSet::new();
        let mut post_tables = HashSet::new();
        for b in prefix {
            for inst in &program.basic_blocks[*b].instructions {
                collect_table_access(inst, &mut pre_tables);
            }
        }
        for b in suffix {
            for inst in &program.basic_blocks[*b].instructions {
                collect_table_access(inst, &mut post_tables);
            }
        }
        let shared_tables = pre_tables.intersection(&post_tables).count();
        let balance = (blocks.len() as isize / 2 - cut_idx as isize).unsigned_abs();

        let score = match strategy {
            SplitStrategy::MinState => (shared_tables, balance, cut_idx),
            SplitStrategy::Balanced => (balance, shared_tables, cut_idx),
            SplitStrategy::MinStateBalanced => (shared_tables, balance, cut_idx),
        };

        let strategy_name = match strategy {
            SplitStrategy::MinState => "min-state",
            SplitStrategy::Balanced => "balanced",
            SplitStrategy::MinStateBalanced => "min-state-balanced",
        };

        candidate_logs.push(format!(
            "  candidate cut_idx={} block={} shared_tables={} balance={} strategy={} score=({},{},{})",
            cut_idx,
            cut.index(),
            shared_tables,
            balance,
            strategy_name,
            score.0,
            score.1,
            score.2
        ));

        if best.as_ref().map(|(_, s)| score < *s).unwrap_or(true) {
            best = Some((cut_idx, score));
        }
    }

    let (cut_idx, score) = best?;
    let prefix_blocks = blocks[..cut_idx].to_vec();
    let suffix_blocks = blocks[cut_idx..].to_vec();

    let mut first = unit.clone();
    first.blocks_per_slice.insert(split_slice, prefix_blocks);
    let mut second = unit.clone();
    second.blocks_per_slice.insert(split_slice, suffix_blocks);

    let summary = format!(
        "split slice={} at block_index={} score=({},{},{})",
        split_slice, cut_idx, score.0, score.1, score.2
    );
    Some((first, second, summary, candidate_logs))
}

