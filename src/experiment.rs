//! Experiment module: run Boogie with various Z3/Boogie options to diagnose performance.

use std::path::PathBuf;
use std::process::Command;
use std::fs;

/// All options from the plan, organized by category.
/// Each entry: (name, vec of boogie cli args to add, description)

pub struct Experiment {
    pub name: String,
    pub extra_args: Vec<String>,  // extra Boogie CLI args
    pub category: String,
    pub description: String,
}

pub fn all_experiments() -> Vec<Experiment> {
    let mut experiments = vec![];

    // ========================================================================
    // Part 1: Boogie-Level Options
    // ========================================================================

    experiments.push(Experiment {
        name: "base_default".into(),
        extra_args: vec![],  // no extra args = default behavior
        category: "Boogie".into(),
        description: "Default Boogie + Z3 (baseline)".into(),
    });

    experiments.push(Experiment {
        name: "boogie_split_every_assert".into(),
        extra_args: vec!["/vcsSplitOnEveryAssert".into()],
        category: "Boogie-VC-Splitting".into(),
        description: "Split VC on every assertion — creates many small queries".into(),
    });

    experiments.push(Experiment {
        name: "boogie_split_max10".into(),
        extra_args: vec![
            "/vcsMaxSplits:10".into(),
            "/vcsSplitOnEveryAssert".into(),
        ],
        category: "Boogie-VC-Splitting".into(),
        description: "Split on every assert with max 10 splits".into(),
    });

    experiments.push(Experiment {
        name: "boogie_split_max20".into(),
        extra_args: vec![
            "/vcsMaxSplits:20".into(),
            "/vcsSplitOnEveryAssert".into(),
        ],
        category: "Boogie-VC-Splitting".into(),
        description: "Split on every assert with max 20 splits".into(),
    });

    experiments.push(Experiment {
        name: "boogie_vc_block".into(),
        extra_args: vec!["/vc:block".into()],
        category: "Boogie-VC-Encoding".into(),
        description: "Block-based VC encoding (different formula shape)".into(),
    });

    experiments.push(Experiment {
        name: "boogie_vc_dag".into(),
        extra_args: vec!["/vc:dag".into()],
        category: "Boogie-VC-Encoding".into(),
        description: "DAG-based VC encoding".into(),
    });

    experiments.push(Experiment {
        name: "boogie_resource_limit".into(),
        extra_args: vec!["/resourceCount:1000000".into()],
        category: "Boogie-Resource".into(),
        description: "Set Z3 resource limit to 1M (for reproducibility)".into(),
    });

    experiments.push(Experiment {
        name: "boogie_prover_log".into(),
        extra_args: vec!["/proverLog:prover_output.smt2".into()],
        category: "Boogie-Diagnostic".into(),
        description: "Dump the SMT2 file sent to Z3 (diagnostic only)".into(),
    });

    // ========================================================================
    // Part 2: Z3 Options
    // ========================================================================

    // Quantifier Instantiation Controls
    experiments.push(Experiment {
        name: "z3_qi_threshold_high".into(),
        extra_args: vec![
            "/proverOpt:O:smt.qi.eager_threshold=100".into(),
            "/proverOpt:O:smt.qi.lazy_threshold=50".into(),
        ],
        category: "Z3-Quantifier".into(),
        description: "High qi thresholds (100/50) — less eager instantiation".into(),
    });

    experiments.push(Experiment {
        name: "z3_qi_max_instances_1000".into(),
        extra_args: vec!["/proverOpt:O:smt.qi.max_instances=1000".into()],
        category: "Z3-Quantifier".into(),
        description: "Hard cap on quantifier instances = 1000 (incomplete but fast)".into(),
    });

    experiments.push(Experiment {
        name: "z3_qi_max_instances_10000".into(),
        extra_args: vec!["/proverOpt:O:smt.qi.max_instances=10000".into()],
        category: "Z3-Quantifier".into(),
        description: "Hard cap on quantifier instances = 10000".into(),
    });

    experiments.push(Experiment {
        name: "z3_qi_max_multi_patterns_0".into(),
        extra_args: vec!["/proverOpt:O:smt.qi.max_multi_patterns=0".into()],
        category: "Z3-Quantifier".into(),
        description: "Disable multi-pattern instantiation".into(),
    });

    experiments.push(Experiment {
        name: "z3_mbqi_false".into(),
        extra_args: vec!["/proverOpt:O:smt.mbqi=false".into()],
        category: "Z3-Quantifier".into(),
        description: "Disable model-based quantifier instantiation".into(),
    });

    experiments.push(Experiment {
        name: "z3_mbqi_true".into(),
        extra_args: vec!["/proverOpt:O:smt.mbqi=true".into()],
        category: "Z3-Quantifier".into(),
        description: "Force model-based quantifier instantiation on".into(),
    });

    experiments.push(Experiment {
        name: "z3_ematching_false".into(),
        extra_args: vec!["/proverOpt:O:smt.ematching=false".into()],
        category: "Z3-Quantifier".into(),
        description: "DRACASTIC: Disable E-matching entirely".into(),
    });

    experiments.push(Experiment {
        name: "z3_qi_profile".into(),
        extra_args: vec!["/proverOpt:O:smt.qi.profile=true".into()],
        category: "Z3-Quantifier-Profile".into(),
        description: "Enable quantifier instantiation profiling (diagnostic)".into(),
    });

    // Array Theory Options
    experiments.push(Experiment {
        name: "z3_array_ext_false".into(),
        extra_args: vec!["/proverOpt:O:smt.array.extensional=false".into()],
        category: "Z3-Array".into(),
        description: "DISABLE array extensionality — incomplete but diagnostic".into(),
    });

    experiments.push(Experiment {
        name: "z3_array_weak".into(),
        extra_args: vec!["/proverOpt:O:smt.array.weak=true".into()],
        category: "Z3-Array".into(),
        description: "Use weak array theory (less complete but faster)".into(),
    });

    experiments.push(Experiment {
        name: "z3_array_theory_branching_false".into(),
        extra_args: vec!["/proverOpt:O:smt.theory_aware_branching=false".into()],
        category: "Z3-Array".into(),
        description: "Disable theory-aware branching".into(),
    });

    // Case Splitting and Relevancy
    experiments.push(Experiment {
        name: "z3_case_split_0".into(),
        extra_args: vec!["/proverOpt:O:smt.case_split=0".into()],
        category: "Z3-CaseSplit".into(),
        description: "Case split strategy 0 (no guessing)".into(),
    });

    experiments.push(Experiment {
        name: "z3_case_split_1".into(),
        extra_args: vec!["/proverOpt:O:smt.case_split=1".into()],
        category: "Z3-CaseSplit".into(),
        description: "Case split strategy 1 (static)".into(),
    });

    experiments.push(Experiment {
        name: "z3_case_split_2".into(),
        extra_args: vec!["/proverOpt:O:smt.case_split=2".into()],
        category: "Z3-CaseSplit".into(),
        description: "Case split strategy 2 (dynamic)".into(),
    });

    experiments.push(Experiment {
        name: "z3_case_split_3".into(),
        extra_args: vec!["/proverOpt:O:smt.case_split=3".into()],
        category: "Z3-CaseSplit".into(),
        description: "Case split strategy 3 (mix)".into(),
    });

    experiments.push(Experiment {
        name: "z3_relevancy_0".into(),
        extra_args: vec!["/proverOpt:O:smt.relevancy=0".into()],
        category: "Z3-Relevancy".into(),
        description: "Disable relevancy propagation".into(),
    });

    experiments.push(Experiment {
        name: "z3_relevancy_2".into(),
        extra_args: vec!["/proverOpt:O:smt.relevancy=2".into()],
        category: "Z3-Relevancy".into(),
        description: "Strong relevancy propagation".into(),
    });

    // Arithmetic
    experiments.push(Experiment {
        name: "z3_arith_solver_6".into(),
        extra_args: vec!["/proverOpt:O:smt.arith.solver=6".into()],
        category: "Z3-Arithmetic".into(),
        description: "Use newer arithmetic solver (6)".into(),
    });

    // Random Seed Testing
    for seed in 1..=5 {
        experiments.push(Experiment {
            name: format!("z3_seed_{}", seed),
            extra_args: vec![format!("/proverOpt:O:smt.random_seed={}", seed)],
            category: "Z3-Seed".into(),
            description: format!("Random seed = {} (fragility test)", seed),
        });
    }

    // Preprocessing
    experiments.push(Experiment {
        name: "z3_pull_nested".into(),
        extra_args: vec!["/proverOpt:O:smt.pull_nested_quantifiers=true".into()],
        category: "Z3-Preprocessing".into(),
        description: "Pull out nested quantifiers".into(),
    });

    experiments.push(Experiment {
        name: "z3_elim_unconstrained".into(),
        extra_args: vec!["/proverOpt:O:smt.elim_unconstrained=true".into()],
        category: "Z3-Preprocessing".into(),
        description: "Eliminate unconstrained variables".into(),
    });

    experiments.push(Experiment {
        name: "z3_expand_store_eq".into(),
        extra_args: vec!["/proverOpt:O:rewriter.expand_store_eq=true".into()],
        category: "Z3-Preprocessing".into(),
        description: "Expand store equalities during rewriting".into(),
    });

    // Combinatorial: array ext off + qi profile (fast diagnostic combo)
    experiments.push(Experiment {
        name: "z3_array_ext_false_qi_profile".into(),
        extra_args: vec![
            "/proverOpt:O:smt.array.extensional=false".into(),
            "/proverOpt:O:smt.qi.profile=true".into(),
        ],
        category: "Z3-Combined-Diagnostic".into(),
        description: "DISABLE array ext + profile — quick diagnostic combo".into(),
    });

    experiments.push(Experiment {
        name: "z3_array_ext_false_split_assert".into(),
        extra_args: vec![
            "/proverOpt:O:smt.array.extensional=false".into(),
            "/vcsSplitOnEveryAssert".into(),
        ],
        category: "Z3-Combined-Diagnostic".into(),
        description: "DISABLE array ext + split on every assert".into(),
    });

    // Aggressive: disable everything + vc splitting
    experiments.push(Experiment {
        name: "z3_disable_everything_split".into(),
        extra_args: vec![
            "/proverOpt:O:smt.ematching=false".into(),
            "/proverOpt:O:smt.mbqi=false".into(),
            "/proverOpt:O:smt.array.extensional=false".into(),
            "/vcsSplitOnEveryAssert".into(),
        ],
        category: "Z3-Aggressive-Disable".into(),
        description: "DISABLE ematching, mbqi, array ext + split on every assert".into(),
    });

    experiments.push(Experiment {
        name: "z3_disable_everything_qi_cap".into(),
        extra_args: vec![
            "/proverOpt:O:smt.ematching=false".into(),
            "/proverOpt:O:smt.mbqi=false".into(),
            "/proverOpt:O:smt.array.extensional=false".into(),
            "/proverOpt:O:smt.qi.max_instances=500".into(),
        ],
        category: "Z3-Aggressive-Disable".into(),
        description: "DISABLE ematching, mbqi, array ext + qi cap at 500".into(),
    });

    experiments
}

/// Run a single experiment on a given Boogie file.
pub fn run_experiment(
    bpl_path: &PathBuf,
    exp: &Experiment,
    loop_unroll: u32,
    timeout_secs: u32,
    output_dir: &PathBuf,
) -> ExperimentResult {
    let exp_name = exp.name.clone();
    println!("[EXP] Running: {} — {}", exp_name, exp.description);

    let start_time = std::time::Instant::now();
    let result = Command::new("boogie")
        .arg("/quiet")
        .arg("/errorTrace:0")
        .arg(format!("/loopUnroll:{}", loop_unroll))
        .arg(format!("/timeLimit:{}", timeout_secs))
        .args(&exp.extra_args)
        .arg(bpl_path.to_string_lossy().to_string())
        .output();

    let duration = start_time.elapsed();

    match result {
        Ok(output) => {
            let stdout = String::from_utf8_lossy(&output.stdout).to_string();
            let stderr = String::from_utf8_lossy(&output.stderr).to_string();
            let timed_out = stdout.contains("timed out");

            // Try to dump the SMT2 file from the prover log
            let prover_log_path = output_dir.join("prover_output.smt2");
            let has_prover_log = prover_log_path.exists();

            println!("[EXP]   duration={:.2}s, timed_out={}, has_prover_log={}, exit_ok={}",
                duration.as_secs_f64(),
                timed_out,
                has_prover_log,
                output.status.success()
            );

            ExperimentResult {
                exp_name,
                duration_secs: duration.as_secs_f64(),
                timed_out,
                exit_success: output.status.success(),
                stdout: stdout.clone(),
                stderr,
                has_prover_log,
                output_dir: output_dir.clone(),
            }
        }
        Err(e) => {
            println!("[EXP]   ERROR: {}", e);
            ExperimentResult {
                exp_name,
                duration_secs: 0.0,
                timed_out: false,
                exit_success: false,
                stdout: String::new(),
                stderr: format!("Failed to execute boogie: {}", e),
                has_prover_log: false,
                output_dir: output_dir.clone(),
            }
        }
    }
}

pub struct ExperimentResult {
    pub exp_name: String,
    pub duration_secs: f64,
    pub timed_out: bool,
    pub exit_success: bool,
    pub stdout: String,
    pub stderr: String,
    pub has_prover_log: bool,
    pub output_dir: PathBuf,
}

impl ExperimentResult {
    pub fn is_fast(&self) -> bool {
        // Fast = finished within 1/4 of the timeout
        self.duration_secs < (30.0 * 0.25)
    }

    pub fn is_slow(&self) -> bool {
        // Slow = took more than 1/2 of the timeout
        self.duration_secs > (30.0 * 0.5) || self.timed_out
    }
}
