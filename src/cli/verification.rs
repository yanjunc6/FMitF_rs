//! Verification stage orchestration.
//!
//! This module owns Stage 5: Boogie program generation, cache lookup,
//! split-driven recursive execution, result aggregation, and verification
//! result collection.

use crate::cfg;
use crate::cli::cache_manager::{CacheManager, CacheResult, CacheRuntimeOptions};
use crate::cli::data::{
    self, CEdgeVerificationData, DataCollector, MemoryStats, VerificationResult,
};
use crate::cli::log::Logger;
use crate::verification::Boogie::{BoogieError, BoogieProgram, VerificationNodeId};
use crate::verification::{
    commutative::{
        naming::{parse_commutative_edge_ids, parse_commutative_nodes},
        splitter::{CommutativeTask, SplitRuntimeOptions, SplitterState},
    },
    verify_result_process::VerifyResultProcessor,
    VerificationManager, VerificationType,
};
use indicatif::ProgressBar;
use rayon::prelude::*;
use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::sync::Arc;
struct BoogieRunResult {
    file_name: String,
    duration_ms: f64,
    stdout: String,
    stderr: String,
    memory_stats: MemoryStats,
}

/// Check if GNU time is available (with -v support)
/// Returns true if /usr/bin/time -v works correctly
fn is_gnu_time_available() -> bool {
    let result = Command::new("/usr/bin/time").arg("-v").arg("echo").output();

    match result {
        Ok(output) => {
            output.status.success()
                && !String::from_utf8_lossy(&output.stderr).contains("illegal option")
        }
        Err(_) => false,
    }
}

#[derive(Clone)]
struct AggregateResult {
    duration_ms: f64,
    result_kind: CacheResult,
    stdout: String,
    stderr: String,
    memory_stats: MemoryStats,
}

enum VerificationTask {
    Partition(BoogieProgram),
    Commutative(CommutativeTask),
}

struct VerificationParallelReport {
    run_results: Vec<BoogieRunResult>,
    cache_stats: Option<crate::cli::cache_manager::CacheStatsSnapshot>,
    split_stats: Option<crate::verification::commutative::splitter::SplitStatsSnapshot>,
    feature_logs: Vec<String>,
    original_root_units: usize,
    split_depth_counts: BTreeMap<usize, usize>,
    max_depth_used: usize,
}

struct CommutativeProcessOutcome {
    result: Option<BoogieRunResult>,
    children: Vec<VerificationTask>,
    feature_logs: Vec<String>,
}

struct ParallelTaskResult {
    run_result: Option<BoogieRunResult>,
    children: Vec<VerificationTask>,
    feature_logs: Vec<String>,
    aggregated_entry: Option<(String, AggregateResult)>,
    root_max_depth: Option<(String, usize)>,
}

pub fn run_verification_stage(
    logger: &mut Logger,
    program: &cfg::Program,
    sc: &crate::sc_graph::SCGraph,
    _combined: &crate::sc_graph::CombinedSCGraph,
    output_dir: &PathBuf,
    loop_unroll: u32,
    timeout_secs: u32,
    cache_options: &CacheRuntimeOptions,
    split_options: &SplitRuntimeOptions,
    data_collector: &mut DataCollector,
) -> Result<(crate::sc_graph::SCGraph, crate::sc_graph::CombinedSCGraph), Box<dyn std::error::Error>>
{
    let has_gnu_time = is_gnu_time_available();
    let verifier = VerificationManager::new();
    let partition_programs = verifier
        .generate_verification_programs(program, sc, VerificationType::HopPartition)
        .map_err(|_errors| {
            Box::<dyn std::error::Error>::from("Partition verification generation failed")
        })?;

    let commutative_programs = verifier
        .generate_verification_programs(program, sc, VerificationType::Commutative)
        .map_err(|_errors| {
            Box::<dyn std::error::Error>::from("Commutative verification generation failed")
        })?;

    let mut programs: Vec<BoogieProgram> = Vec::new();
    programs.extend(partition_programs.clone());
    programs.extend(commutative_programs.clone());

    fs::create_dir_all(output_dir)?;
    let boogie_dir = output_dir.join("Boogie");
    if boogie_dir.exists() {
        fs::remove_dir_all(&boogie_dir)?;
    }
    fs::create_dir_all(&boogie_dir)?;

    let report = run_verification_parallel(
        logger,
        program,
        sc,
        &programs,
        &boogie_dir,
        loop_unroll,
        timeout_secs,
        cache_options,
        split_options,
        has_gnu_time,
    )?;

    if let Some(stats) = report.cache_stats {
        data_collector.set_verification_cache_stats(
            stats.hits,
            stats.misses,
            stats.stores,
            stats.evictions,
            stats.invalidations,
        );
    }

    if let Some(stats) = report.split_stats {
        data_collector.set_verification_split_stats(
            report.original_root_units,
            stats.total_units,
            stats.root_timeouts,
            stats.split_performed,
            stats.split_skipped_max_depth,
            stats.split_skipped_no_cutpoint,
            stats.split_budget_exhausted,
        );
    } else {
        data_collector.set_verification_split_stats(report.original_root_units, 0, 0, 0, 0, 0, 0);
    }

    data_collector
        .set_verification_split_depth_stats(report.split_depth_counts, report.max_depth_used);

    for line in report.feature_logs {
        logger.line(line)?;
    }

    let mut all_boogie_errors: Vec<BoogieError> = Vec::new();
    let mut exec_errors = Vec::new();

    for result in report.run_results {
        logger.line(format!("===== Boogie run for {} =====", result.file_name))?;
        logger.block("stdout:", &result.stdout)?;
        logger.block("stderr:", &result.stderr)?;

        let verification_result =
            process_boogie_result(&result, &mut all_boogie_errors, &mut exec_errors);

        if let Some((src_f, src_i, src_h, tgt_f, tgt_i, tgt_h)) =
            parse_commutative_edge_ids(&result.file_name)
        {
            let edge_data = CEdgeVerificationData {
                source_function_id: src_f,
                source_instance: src_i,
                source_hop_id: src_h,
                target_function_id: tgt_f,
                target_instance: tgt_i,
                target_hop_id: tgt_h,
                duration_ms: result.duration_ms,
                result: verification_result,
                eliminated: false,
                memory_stats: result.memory_stats,
                boogie_stdout: result.stdout,
                boogie_stderr: result.stderr,
                boogie_file: result.file_name,
            };
            data_collector.add_c_edge_verification(edge_data);
        }
    }

    let (verification_errors, simplified) =
        VerifyResultProcessor::process_boogie_errors(program, sc.clone(), all_boogie_errors);

    let simplified_sc_c_edges = simplified
        .edges
        .iter()
        .filter(|edge| matches!(edge.edge_type, crate::sc_graph::EdgeType::C))
        .count();

    data_collector.set_sc_stats(
        sc.edges
            .iter()
            .filter(|edge| matches!(edge.edge_type, crate::sc_graph::EdgeType::C))
            .count(),
        simplified_sc_c_edges,
    );

    let simplified_edges: HashSet<_> = simplified
        .edges
        .iter()
        .filter(|e| matches!(e.edge_type, crate::sc_graph::EdgeType::C))
        .map(|e| {
            (
                e.source.function_id.index(),
                e.source.instance,
                e.source.hop_id.index(),
                e.target.function_id.index(),
                e.target.instance,
                e.target.hop_id.index(),
            )
        })
        .collect();

    let eliminated_edges: HashSet<_> = sc
        .edges
        .iter()
        .filter(|e| matches!(e.edge_type, crate::sc_graph::EdgeType::C))
        .map(|e| {
            (
                e.source.function_id.index(),
                e.source.instance,
                e.source.hop_id.index(),
                e.target.function_id.index(),
                e.target.instance,
                e.target.hop_id.index(),
            )
        })
        .filter(|edge| !simplified_edges.contains(edge))
        .collect();

    data_collector.mark_eliminated_edges(&eliminated_edges);

    write_sc_graph_dots(logger, &simplified, program, output_dir, "simplified")?;

    let simplified_combined = crate::sc_graph::combine_for_deadlock_elimination(&simplified);

    if !verification_errors.is_empty() {
        logger.line("⚠️  Verification completed with errors")?;
        logger.line(format!("  Total errors: {}", verification_errors.len()))?;
    }
    if !exec_errors.is_empty() {
        for error in &exec_errors {
            logger.line(format!("⚠️  {}", error))?;
        }
    }

    Ok((simplified, simplified_combined))
}

fn run_verification_parallel(
    _logger: &mut Logger,
    program: &cfg::Program,
    sc: &crate::sc_graph::SCGraph,
    programs: &[BoogieProgram],
    boogie_dir: &PathBuf,
    loop_unroll: u32,
    timeout_secs: u32,
    cache_options: &CacheRuntimeOptions,
    split_options: &SplitRuntimeOptions,
    has_gnu_time: bool,
) -> Result<VerificationParallelReport, Box<dyn std::error::Error>> {
    let mut partition_programs = Vec::new();
    let mut commutative_programs = Vec::new();
    for program in programs {
        if program.name.starts_with("commutative_") {
            commutative_programs.push(program.clone());
        } else {
            partition_programs.push(program.clone());
        }
    }

    let cache_state = Arc::new(CacheManager::new(cache_options)?);
    let splitter_state = Arc::new(SplitterState::new(sc, split_options));

    let mut queue: VecDeque<VerificationTask> = partition_programs
        .into_iter()
        .map(VerificationTask::Partition)
        .chain(commutative_programs.into_iter().filter_map(|program| {
            splitter_state
                .create_root_task(program)
                .map(VerificationTask::Commutative)
        }))
        .collect();

    let original_root_units = queue.len();

    let mut run_results = Vec::new();
    let mut aggregated: HashMap<String, AggregateResult> = HashMap::new();
    let mut feature_logs = Vec::new();
    let mut split_count_for_message = 0usize;
    let mut root_max_depths: HashMap<String, usize> = HashMap::new();

    let pb = ProgressBar::new(original_root_units as u64);
    pb.set_style(
        indicatif::ProgressStyle::with_template(
            "[{elapsed_precise}] [{bar:40.cyan/blue}] {pos}/{len} {msg}",
        )
        .unwrap()
        .progress_chars("=>-"),
    );
    pb.set_message("running verification");

    while !queue.is_empty() {
        let batch: Vec<_> = queue.drain(..).collect();

        // Process tasks in parallel. Any worker failure must abort the stage;
        // silently dropping a task can invalidate split-child aggregation.
        let parallel_results: Vec<Result<ParallelTaskResult, String>> = batch
            .into_par_iter()
            .map(|task| {
                let progress_bar = pb.clone();
                let cache_state_clone = Arc::clone(&cache_state);
                let splitter_state_clone = Arc::clone(&splitter_state);

                // Record this unit in the split statistics
                splitter_state_clone.record_total_unit();

                let task_result = match task {
                    VerificationTask::Partition(program) => {
                        match process_single_partition_verification(
                            &program,
                            boogie_dir,
                            loop_unroll,
                            timeout_secs,
                            has_gnu_time,
                            split_options,
                            0,
                        ) {
                            Ok(result) => Ok(ParallelTaskResult {
                                run_result: Some(result),
                                children: Vec::new(),
                                feature_logs: Vec::new(),
                                aggregated_entry: None,
                                root_max_depth: None,
                            }),
                            Err(e) => Err(format!("partition verification failed: {}", e)),
                        }
                    }
                    VerificationTask::Commutative(task) => {
                        match process_single_commutative_verification(
                            program,
                            &task,
                            &cache_state_clone,
                            &splitter_state_clone,
                            boogie_dir,
                            loop_unroll,
                            timeout_secs,
                            has_gnu_time,
                            split_options,
                        ) {
                            Ok(outcome) => {
                                let aggregated_entry = if let Some(result) = outcome.result {
                                    let kind = classify_quick_result(&result.stdout);
                                    Some((
                                        task.root_file_name.clone(),
                                        AggregateResult {
                                            duration_ms: result.duration_ms,
                                            result_kind: kind,
                                            stdout: result.stdout.clone(),
                                            stderr: result.stderr.clone(),
                                            memory_stats: result.memory_stats.clone(),
                                        },
                                    ))
                                } else {
                                    None
                                };

                                let root_max_depth =
                                    Some((task.root_file_name.clone(), task.depth as usize));

                                Ok(ParallelTaskResult {
                                    run_result: None,
                                    children: outcome.children,
                                    feature_logs: outcome.feature_logs,
                                    aggregated_entry,
                                    root_max_depth,
                                })
                            }
                            Err(e) => Err(format!(
                                "commutative verification failed for {}: {}",
                                task.program.name, e
                            )),
                        }
                    }
                };

                // Update progress as each unit finishes to provide smoother feedback.
                progress_bar.inc(1);
                task_result
            })
            .collect();

        let mut parallel_results_ok = Vec::with_capacity(parallel_results.len());
        for result in parallel_results {
            match result {
                Ok(v) => parallel_results_ok.push(v),
                Err(msg) => return Err(msg.into()),
            }
        }

        // Aggregate results back into main state
        for result in parallel_results_ok {
            if let Some(run_result) = result.run_result {
                run_results.push(run_result);
            }

            feature_logs.extend(result.feature_logs);

            if let Some((root_file_name, agg)) = result.aggregated_entry {
                let rank = result_rank(agg.result_kind);
                aggregated
                    .entry(root_file_name)
                    .and_modify(|existing| {
                        existing.duration_ms += agg.duration_ms;
                        if rank > result_rank(existing.result_kind) {
                            existing.result_kind = agg.result_kind;
                            existing.stdout = agg.stdout.clone();
                            existing.stderr = agg.stderr.clone();
                            existing.memory_stats = agg.memory_stats.clone();
                        }
                    })
                    .or_insert(agg);
            }

            if let Some((root_file_name, depth)) = result.root_max_depth {
                root_max_depths
                    .entry(root_file_name)
                    .and_modify(|current| *current = (*current).max(depth))
                    .or_insert(depth);
            }

            for child in result.children {
                queue.push_back(child);
                pb.inc_length(1);
            }
        }

        split_count_for_message = splitter_state
            .stats()
            .map(|stats| stats.split_performed)
            .unwrap_or(0);
        pb.set_message(format!(
            "running commutative checks ({} splitted)",
            split_count_for_message
        ));
    }

    let mut split_depth_counts: BTreeMap<usize, usize> = BTreeMap::new();
    let mut max_depth_used = 0usize;
    for depth in root_max_depths.into_values() {
        *split_depth_counts.entry(depth).or_insert(0) += 1;
        max_depth_used = max_depth_used.max(depth);
    }

    for (root_file_name, agg) in aggregated {
        run_results.push(BoogieRunResult {
            file_name: root_file_name,
            duration_ms: agg.duration_ms,
            stdout: agg.stdout,
            stderr: agg.stderr,
            memory_stats: agg.memory_stats,
        });
    }

    pb.finish_with_message(format!(
        "commutative checks complete ({} splitted)",
        split_count_for_message
    ));

    Ok(VerificationParallelReport {
        run_results,
        cache_stats: cache_state.stats(cache_options.enabled),
        split_stats: (*splitter_state).stats(),
        feature_logs,
        original_root_units,
        split_depth_counts,
        max_depth_used,
    })
}

fn process_single_partition_verification(
    program: &BoogieProgram,
    boogie_dir: &PathBuf,
    loop_unroll: u32,
    timeout_secs: u32,
    has_gnu_time: bool,
    split_options: &SplitRuntimeOptions,
    current_depth: u32,
) -> Result<BoogieRunResult, Box<dyn std::error::Error>> {
    run_boogie_program(
        program,
        boogie_dir,
        loop_unroll,
        timeout_secs,
        has_gnu_time,
        split_options.debug_enforce_split,
        current_depth,
    )
}

fn process_single_commutative_verification(
    program: &cfg::Program,
    task: &CommutativeTask,
    cache_state: &CacheManager,
    splitter_state: &SplitterState,
    boogie_dir: &PathBuf,
    loop_unroll: u32,
    timeout_secs: u32,
    has_gnu_time: bool,
    split_options: &SplitRuntimeOptions,
) -> Result<CommutativeProcessOutcome, Box<dyn std::error::Error>> {
    let cache_key = cache_state.key_for_program(program, &task.program, loop_unroll, timeout_secs);
    let mut feature_logs = Vec::new();

    if let Some(hit) = cache_state.lookup(&cache_key) {
        let result = BoogieRunResult {
            file_name: task.program.name.clone() + ".bpl",
            duration_ms: 0.0,
            stdout: cache_hit_synthetic_stdout(&task.root_file_name, hit),
            stderr: format!("cache hit: {:?}", hit).to_lowercase(),
            memory_stats: MemoryStats::default(),
        };

        if hit == CacheResult::Timeout {
            splitter_state.record_root_timeout();
            let (split_tasks, logs) = splitter_state.try_split_task(program, task)?;
            feature_logs.extend(logs);
            cache_state.store(&cache_key, hit);
            if let Some(children) = split_tasks {
                let children = children
                    .into_iter()
                    .map(VerificationTask::Commutative)
                    .collect();
                return Ok(CommutativeProcessOutcome {
                    result: None,
                    children,
                    feature_logs,
                });
            }
        }

        cache_state.store(&cache_key, hit);
        return Ok(CommutativeProcessOutcome {
            result: Some(result),
            children: Vec::new(),
            feature_logs,
        });
    }

    let result = run_boogie_program(
        &task.program,
        boogie_dir,
        loop_unroll,
        timeout_secs,
        has_gnu_time,
        split_options.debug_enforce_split,
        task.depth,
    )?;
    let kind = classify_quick_result(&result.stdout);
    cache_state.store(&cache_key, kind);

    if kind == CacheResult::Timeout {
        splitter_state.record_root_timeout();
        let (split_tasks, logs) = splitter_state.try_split_task(program, task)?;
        feature_logs.extend(logs);
        cache_state.store(&cache_key, kind);
        if let Some(children) = split_tasks {
            let children = children
                .into_iter()
                .map(VerificationTask::Commutative)
                .collect();
            return Ok(CommutativeProcessOutcome {
                result: None,
                children,
                feature_logs,
            });
        }
    }

    Ok(CommutativeProcessOutcome {
        result: Some(result),
        children: Vec::new(),
        feature_logs,
    })
}

fn run_boogie_program(
    program: &BoogieProgram,
    boogie_dir: &PathBuf,
    loop_unroll: u32,
    timeout_secs: u32,
    has_gnu_time: bool,
    debug_enforce_split: u32,
    current_depth: u32,
) -> Result<BoogieRunResult, Box<dyn std::error::Error>> {
    // Debug mode: enforce splitting by returning fake timeout if not at target depth
    if debug_enforce_split > 0 && current_depth < debug_enforce_split {
        return Ok(BoogieRunResult {
            file_name: format!("{}.bpl", program.name),
            duration_ms: 0.0,
            stdout: "timed out".to_string(),
            stderr: format!(
                "DEBUG_ENFORCE_SPLIT: depth {} < target {}, forcing split",
                current_depth, debug_enforce_split
            ),
            memory_stats: MemoryStats::default(),
        });
    }

    let file_name = format!("{}.bpl", program.name);
    let file_path = boogie_dir.join(&file_name);

    let write_res = (|| -> Result<(), std::io::Error> {
        use std::io::Write;
        let mut f = fs::File::create(&file_path)?;
        write!(f, "{}", program)?;
        Ok(())
    })();

    match write_res {
        Ok(()) => {
            let start_time = std::time::Instant::now();

            let result = if has_gnu_time {
                Command::new("/usr/bin/time")
                    .arg("-v")
                    .arg("boogie")
                    .arg("/quiet")
                    .arg("/errorTrace:0")
                    .arg(format!("/loopUnroll:{}", loop_unroll))
                    .arg(format!("/timeLimit:{}", timeout_secs))
                    .arg(file_path.to_string_lossy().to_string())
                    .output()
            } else {
                Command::new("boogie")
                    .arg("/quiet")
                    .arg("/errorTrace:0")
                    .arg(format!("/loopUnroll:{}", loop_unroll))
                    .arg(format!("/timeLimit:{}", timeout_secs))
                    .arg(file_path.to_string_lossy().to_string())
                    .output()
            };

            let duration = start_time.elapsed();

            match result {
                Ok(output) => {
                    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
                    let stderr = String::from_utf8_lossy(&output.stderr).to_string();

                    // Only parse time output if we used GNU time
                    let memory_stats = if has_gnu_time {
                        data::parse_time_output(&stderr)
                    } else {
                        MemoryStats::default()
                    };

                    Ok(BoogieRunResult {
                        file_name,
                        duration_ms: duration.as_secs_f64() * 1000.0,
                        stdout,
                        stderr,
                        memory_stats,
                    })
                }
                Err(e) => Ok(BoogieRunResult {
                    file_name,
                    duration_ms: 0.0,
                    stdout: String::new(),
                    stderr: format!("Failed to execute: {}", e),
                    memory_stats: MemoryStats::default(),
                }),
            }
        }
        Err(e) => Ok(BoogieRunResult {
            file_name,
            duration_ms: 0.0,
            stdout: String::new(),
            stderr: format!("Failed to write .bpl file: {}", e),
            memory_stats: MemoryStats::default(),
        }),
    }
}

fn classify_quick_result(stdout: &str) -> CacheResult {
    if stdout.contains("parse errors detected")
        || stdout.contains("name resolution errors detected")
        || stdout.contains("type errors detected")
        || stdout.contains("type checking errors detected")
    {
        return CacheResult::CompilationError;
    }
    if stdout.contains("timed out") {
        return CacheResult::Timeout;
    }
    for line in stdout.lines() {
        let line = line.trim();
        if line.starts_with('(') && line.ends_with(')') {
            if BoogieError::from_boogie_string(line).is_some() {
                return CacheResult::Error;
            }
        }
    }
    CacheResult::Pass
}

fn cache_hit_synthetic_stdout(file_name: &str, hit: CacheResult) -> String {
    match hit {
        CacheResult::Pass => String::new(),
        CacheResult::Timeout => "timed out".to_string(),
        CacheResult::CompilationError => "parse errors detected".to_string(),
        CacheResult::Error => {
            if let Some((sf, si, sh, tf, ti, th)) = parse_commutative_edge_ids(file_name) {
                let err = BoogieError::SpecialInterleavingNonEquivalence {
                    node_1: VerificationNodeId {
                        function_id: sf,
                        instance: si,
                        hop_id: sh,
                    },
                    node_2: VerificationNodeId {
                        function_id: tf,
                        instance: ti,
                        hop_id: th,
                    },
                };
                serde_lexpr::to_string(&err).unwrap_or_else(|_| format!("{:?}", err))
            } else {
                "(error unknown)".to_string()
            }
        }
    }
}

fn result_rank(kind: CacheResult) -> u8 {
    match kind {
        CacheResult::Pass => 1,
        CacheResult::CompilationError => 2,
        CacheResult::Timeout => 3,
        CacheResult::Error => 4,
    }
}

fn process_boogie_result(
    result: &BoogieRunResult,
    all_boogie_errors: &mut Vec<BoogieError>,
    exec_errors: &mut Vec<String>,
) -> VerificationResult {
    if result.stdout.contains("parse errors detected")
        || result.stdout.contains("name resolution errors detected")
        || result.stdout.contains("type errors detected")
        || result.stdout.contains("type checking errors detected")
    {
        exec_errors.push(format!(
            "Boogie compilation failed for {}: Check compiler.log for details.",
            result.file_name
        ));
        return VerificationResult::CompilationError;
    }

    if result.stdout.contains("timed out") {
        if let Some(timeout_error) = build_timeout_error_from_commutative(&result.file_name) {
            all_boogie_errors.push(timeout_error);
            return VerificationResult::Timeout;
        } else {
            exec_errors.push(format!(
                "Boogie verification timed out for unknown file {}.",
                result.file_name
            ));
            return VerificationResult::CompilationError;
        }
    }

    let mut has_errors = false;
    for line in result.stdout.lines() {
        let line = line.trim();
        if line.starts_with('(') && line.ends_with(')') {
            if let Some(err) = BoogieError::from_boogie_string(line) {
                all_boogie_errors.push(err);
                has_errors = true;
            }
        }
    }

    if has_errors {
        VerificationResult::Error
    } else {
        VerificationResult::Pass
    }
}

fn build_timeout_error_from_commutative(file_name: &str) -> Option<BoogieError> {
    let (node_1, node_2) = parse_commutative_nodes(file_name)?;
    Some(BoogieError::SpecialInterleavingTimeout { node_1, node_2 })
}

fn write_sc_graph_dots(
    logger: &mut Logger,
    sc: &crate::sc_graph::SCGraph,
    program: &cfg::Program,
    output_dir: &PathBuf,
    prefix: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    use crate::pretty::{CombinedSCGraphDotPrinter, PrettyPrint, SCGraphDotPrinter};

    fs::create_dir_all(output_dir)?;

    let sc_filename = if prefix.is_empty() {
        "sc_graph.dot".to_string()
    } else {
        format!("{}_sc_graph.dot", prefix)
    };
    let combined_filename = if prefix.is_empty() {
        "sc_graph_combined.dot".to_string()
    } else {
        format!("{}_sc_graph_combined.dot", prefix)
    };

    let sc_path = output_dir.join(&sc_filename);
    let mut sc_file = fs::File::create(&sc_path)?;
    SCGraphDotPrinter::new(sc, program).pretty_print(&mut sc_file)?;

    let combined = crate::sc_graph::combine_for_deadlock_elimination(sc);
    let combined_path = output_dir.join(&combined_filename);
    let mut c_file = fs::File::create(&combined_path)?;
    CombinedSCGraphDotPrinter::new(&combined, program).pretty_print(&mut c_file)?;

    logger.line(format!(
        "📄 SC Graph DOT files written: {}, {}",
        sc_path.display(),
        combined_path.display()
    ))?;
    Ok(())
}
