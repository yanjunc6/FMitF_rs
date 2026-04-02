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
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::PathBuf;
use std::process::Command;

struct BoogieRunResult {
    file_name: String,
    duration_ms: f64,
    stdout: String,
    stderr: String,
    memory_stats: MemoryStats,
}

#[derive(Clone)]
struct AggregateResult {
    duration_ms: f64,
    result_kind: CacheResult,
    stdout: String,
    stderr: String,
    memory_stats: MemoryStats,
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

    let (run_results, cache_stats_for_log, split_stats_for_log, feature_logs, original_root_units) =
        run_boogie_task_box(
            logger,
            program,
            sc,
            &programs,
            &boogie_dir,
            loop_unroll,
            timeout_secs,
            cache_options,
            split_options,
        )?;

    if let Some(stats) = cache_stats_for_log {
        data_collector.set_verification_cache_stats(
            stats.hits,
            stats.misses,
            stats.stores,
            stats.evictions,
            stats.invalidations,
        );
    }

    if let Some(stats) = split_stats_for_log {
        data_collector.set_verification_split_stats(
            original_root_units,
            stats.total_units,
            stats.root_timeouts,
            stats.split_performed,
            stats.split_skipped_max_depth,
            stats.split_skipped_no_cutpoint,
            stats.split_budget_exhausted,
        );
    } else {
        data_collector.set_verification_split_stats(original_root_units, 0, 0, 0, 0, 0, 0);
    }

    for line in feature_logs {
        logger.line(line)?;
    }

    let mut all_boogie_errors: Vec<BoogieError> = Vec::new();
    let mut exec_errors = Vec::new();

    for result in run_results {
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

fn run_boogie_task_box(
    _logger: &mut Logger,
    program: &cfg::Program,
    sc: &crate::sc_graph::SCGraph,
    programs: &[BoogieProgram],
    boogie_dir: &PathBuf,
    loop_unroll: u32,
    timeout_secs: u32,
    cache_options: &CacheRuntimeOptions,
    split_options: &SplitRuntimeOptions,
) -> Result<
    (
        Vec<BoogieRunResult>,
        Option<crate::cli::cache_manager::CacheStatsSnapshot>,
        Option<crate::verification::commutative::splitter::SplitStatsSnapshot>,
        Vec<String>,
        usize,
    ),
    Box<dyn std::error::Error>,
> {
    use indicatif::ProgressStyle;

    // TODO: why you first combine them in run_verification_stage and then separate them here? can we just keep them separate?
    let mut partition_programs = Vec::new();
    let mut commutative_programs = Vec::new();
    for p in programs {
        if p.name.starts_with("commutative_") {
            commutative_programs.push(p.clone());
        } else {
            partition_programs.push(p.clone());
        }
    }

    let cache_state = CacheManager::new(cache_options)?;
    let splitter_state = SplitterState::new(sc, split_options);

    let mut queue: Vec<CommutativeTask> = commutative_programs
        .into_iter()
        .filter_map(|p| splitter_state.create_root_task(p))
        .collect();

    let original_root_units = partition_programs.len() + queue.len();

    let mut run_results = Vec::new();
    let pb = ProgressBar::new(original_root_units as u64);
    pb.set_style(
        ProgressStyle::with_template("[{elapsed_precise}] [{bar:40.cyan/blue}] {pos}/{len} {msg}")
            .unwrap()
            .progress_chars("=>-"),
    );
    pb.set_message("running partition checks");

    let partition_results = run_boogie_verifications_parallel(
        &partition_programs,
        boogie_dir,
        loop_unroll,
        timeout_secs,
        Some(&pb),
    )?;
    pb.inc(partition_results.len() as u64);
    run_results.extend(partition_results);
    let mut split_count_for_message = 0usize;
    pb.set_message(format!(
        "running commutative checks ({} splitted)",
        split_count_for_message
    ));

    let mut feature_logs = Vec::new();
    let mut aggregated: HashMap<String, AggregateResult> = HashMap::new();

    while !queue.is_empty() {
        let mut next_queue = Vec::new();
        let mut runnable = Vec::new();
        let mut runnable_keys: HashMap<String, (CommutativeTask, String)> = HashMap::new();

        for task in queue.drain(..) {
            splitter_state.record_total_unit();
            let cache_key =
                cache_state.key_for_program(program, &task.program, loop_unroll, timeout_secs);
            if let Some(hit) = cache_state.lookup(&cache_key) {
                let mut hit_result = BoogieRunResult {
                    file_name: task.program.name.clone() + ".bpl",
                    duration_ms: 0.0,
                    stdout: cache_hit_synthetic_stdout(&task.root_file_name, hit),
                    stderr: format!("cache hit: {:?}", hit).to_lowercase(),
                    memory_stats: MemoryStats::default(),
                };

                if hit == CacheResult::Timeout {
                    splitter_state.record_root_timeout();
                    let (split_tasks, logs) = splitter_state.try_split_task(program, &task)?;
                    feature_logs.extend(logs);
                    if let Some(children) = split_tasks {
                        pb.inc_length(children.len() as u64);
                        next_queue.extend(children);
                        pb.inc(1);
                        continue;
                    }
                }

                pb.inc(1);
                merge_commutative_leaf(&mut aggregated, task.root_file_name, &mut hit_result, hit);
                continue;
            }

            let file_name = format!("{}.bpl", task.program.name);
            runnable_keys.insert(file_name, (task.clone(), cache_key));
            runnable.push(task.program.clone());
        }

        if !runnable.is_empty() {
            let batch_results = run_boogie_verifications_parallel(
                &runnable,
                boogie_dir,
                loop_unroll,
                timeout_secs,
                Some(&pb),
            )?;

            for mut result in batch_results {
                let key = result.file_name.clone();
                let (task, cache_key) = match runnable_keys.remove(&key) {
                    Some(v) => v,
                    None => continue,
                };

                let kind = classify_quick_result(&result.stdout);
                cache_state.store(&cache_key, kind);

                if kind == CacheResult::Timeout {
                    splitter_state.record_root_timeout();
                    let (split_tasks, logs) = splitter_state.try_split_task(program, &task)?;
                    feature_logs.extend(logs);
                    if let Some(children) = split_tasks {
                        pb.inc_length(children.len() as u64);
                        next_queue.extend(children);
                        pb.inc(1);
                        continue;
                    }
                }

                pb.inc(1);
                merge_commutative_leaf(&mut aggregated, task.root_file_name, &mut result, kind);
            }
        }

        split_count_for_message = splitter_state
            .stats()
            .map(|s| s.split_performed)
            .unwrap_or(0);
        pb.set_message(format!(
            "running commutative checks ({} splitted)",
            split_count_for_message
        ));

        queue = next_queue;
    }

    pb.finish_with_message(format!(
        "commutative checks complete ({} splitted)",
        split_count_for_message
    ));

    for (root_file_name, agg) in aggregated {
        run_results.push(BoogieRunResult {
            file_name: root_file_name,
            duration_ms: agg.duration_ms,
            stdout: agg.stdout,
            stderr: agg.stderr,
            memory_stats: agg.memory_stats,
        });
    }

    Ok((
        run_results,
        cache_state.stats(cache_options.enabled),
        splitter_state.stats(),
        feature_logs,
        original_root_units,
    ))
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

fn merge_commutative_leaf(
    aggregated: &mut HashMap<String, AggregateResult>,
    root_file_name: String,
    result: &mut BoogieRunResult,
    kind: CacheResult,
) {
    let rank = result_rank(kind);
    aggregated
        .entry(root_file_name)
        .and_modify(|agg| {
            agg.duration_ms += result.duration_ms;
            if rank > result_rank(agg.result_kind) {
                agg.result_kind = kind;
                agg.stdout = result.stdout.clone();
                agg.stderr = result.stderr.clone();
                agg.memory_stats = result.memory_stats.clone();
            }
        })
        .or_insert_with(|| AggregateResult {
            duration_ms: result.duration_ms,
            result_kind: kind,
            stdout: result.stdout.clone(),
            stderr: result.stderr.clone(),
            memory_stats: result.memory_stats.clone(),
        });
}

fn result_rank(kind: CacheResult) -> u8 {
    match kind {
        CacheResult::Pass => 1,
        CacheResult::CompilationError => 2,
        CacheResult::Timeout => 3,
        CacheResult::Error => 4,
    }
}

fn run_boogie_verifications_parallel(
    programs: &[BoogieProgram],
    boogie_dir: &PathBuf,
    loop_unroll: u32,
    timeout_secs: u32,
    progress: Option<&indicatif::ProgressBar>,
) -> Result<Vec<BoogieRunResult>, Box<dyn std::error::Error>> {
    use rayon::prelude::*;
    let progress = progress.cloned();

    let run_results: Vec<BoogieRunResult> = programs
        .par_iter()
        .map(|program| {
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
                    let result = Command::new("/usr/bin/time")
                        .arg("-v")
                        .arg("boogie")
                        .arg("/quiet")
                        .arg("/errorTrace:0")
                        .arg(format!("/loopUnroll:{}", loop_unroll))
                        .arg(format!("/timeLimit:{}", timeout_secs))
                        .arg(file_path.to_string_lossy().to_string())
                        .output();
                    let duration = start_time.elapsed();
                    if let Some(pb) = &progress {
                        pb.inc(1);
                    }

                    match result {
                        Ok(output) => {
                            let stdout = String::from_utf8_lossy(&output.stdout).to_string();
                            let stderr = String::from_utf8_lossy(&output.stderr).to_string();
                            let memory_stats = data::parse_time_output(&stderr);
                            BoogieRunResult {
                                file_name,
                                duration_ms: duration.as_secs_f64() * 1000.0,
                                stdout,
                                stderr,
                                memory_stats,
                            }
                        }
                        Err(e) => BoogieRunResult {
                            file_name,
                            duration_ms: 0.0,
                            stdout: String::new(),
                            stderr: format!("Failed to execute: {}", e),
                            memory_stats: MemoryStats::default(),
                        },
                    }
                }
                Err(e) => BoogieRunResult {
                    file_name,
                    duration_ms: 0.0,
                    stdout: String::new(),
                    stderr: format!("Failed to write .bpl file: {}", e),
                    memory_stats: MemoryStats::default(),
                },
            }
        })
        .collect();

    Ok(run_results)
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
