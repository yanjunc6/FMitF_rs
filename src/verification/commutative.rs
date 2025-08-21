use super::errors::Results;
pub use super::errors::{SpannedError, VerificationError};
use super::Boogie::{
    self, gen_Boogie::BoogieProgramGenerator, BoogieBinOp, BoogieExpr, BoogieExprKind, BoogieLine,
    BoogieProcedure, ErrorMessage,
};
use crate::cfg::{CfgProgram, FunctionId, HopId, VarId};
use crate::sc_graph::{EdgeType, SCGraph, SCGraphEdge};
use std::collections::{HashMap, HashSet};

pub struct CommutativeUnit {
    pub c_edge: SCGraphEdge,
    pub hops_A: Vec<HopId>, // continuous hops where the last hop is source of c_edge
    pub hops_B: Vec<HopId>, // continuous hops where the last hop is target of c_edge
    pub func_id_A: FunctionId,
    pub func_id_B: FunctionId,
}

#[derive(Clone, Copy, PartialEq)]
enum Direction {
    Unknown,
    ABeforeB,
    BBeforeA,
}

/// Represents an interleaving as a sequence of (hop_id, is_from_slice_a)
type Interleaving = Vec<(HopId, bool)>;

/// Special interleavings that need to be tracked
#[derive(Clone)]
struct SpecialInterleavings {
    /// slice_a followed by slice_b
    a_then_b: Interleaving,
    /// slice_b followed by slice_a  
    b_then_a: Interleaving,
}

/// Liveness analysis results for slice commutativity verification
/// TODO: change its name to analysis info would be better. After generate the live-in var, live-out var, please also record table written and read by slice A and B.
/// also, record the table written by last hop of slice A and B individually
struct SliceLivenessInfo {
    /// All variables that are live-in or live-out for either slice
    live_vars: Vec<VarId>,
    /// Variables live-in to slice A
    live_in_a: HashSet<VarId>,
    /// Variables live-out from slice A
    live_out_a: HashSet<VarId>,
    /// Variables live-in to slice B
    live_in_b: HashSet<VarId>,
    /// Variables live-out from slice B
    live_out_b: HashSet<VarId>,
}

/// Variable snapshot names for tracking state
struct VariableSnapshots {
    /// Names for table state snapshots
    table_snapshots: HashMap<String, String>,
    /// Names for variable snapshots
    var_snapshots: HashMap<VarId, String>,
}

pub struct CommutativeVerificationManager {
    pub commutative_units: Vec<CommutativeUnit>,
}

impl CommutativeVerificationManager {
    pub fn new() -> Self {
        CommutativeVerificationManager {
            commutative_units: Vec::new(),
        }
    }

    /// Create simple commutative units from the SC-graph with only the hops on the ends
    pub fn create_simple_commutative_units(&mut self, sc_graph: &SCGraph) {
        // Analyze the SC-graph to identify commutative units
        for edge in &sc_graph.edges {
            if edge.edge_type == EdgeType::C {
                let mut unit = CommutativeUnit {
                    c_edge: edge.clone(),
                    hops_A: Vec::new(),
                    hops_B: Vec::new(),
                    func_id_A: edge.source.function_id,
                    func_id_B: edge.target.function_id,
                };
                unit.hops_A.push(edge.source.hop_id);
                unit.hops_B.push(edge.target.hop_id);
                self.commutative_units.push(unit);
            }
        }
    }

    /// Generate Commutative (Slice Commutativity) verification Boogie programs
    /// Each pair of conflicting transaction slices gets its own verification
    pub fn generate_commutative_verification(
        &mut self,
        cfg_program: &CfgProgram,
        sc_graph: &SCGraph,
    ) -> Results<Vec<Boogie::BoogieProgram>> {
        let mut programs = Vec::new();
        let base_program =
            Boogie::gen_Boogie::BoogieProgramGenerator::gen_base_program(cfg_program)?;

        // Create simple commutative units from the SC-graph
        self.create_simple_commutative_units(sc_graph);

        for unit in self.commutative_units.iter() {
            let slice_a: &Vec<HopId> = &unit.hops_A;
            let slice_b: &Vec<HopId> = &unit.hops_B;

            if !slice_a.is_empty() && !slice_b.is_empty() {
                let mut program = base_program.clone();
                program.name = format!(
                    "commutative_Simple_Hop{}_vs_Hop{}",
                    unit.c_edge.source.hop_id.index(),
                    unit.c_edge.target.hop_id.index()
                );

                let mut generator =
                    Boogie::gen_Boogie::BoogieProgramGenerator::with_program(program);

                // Generate commutative verification procedure
                let procedure = self.create_commutative_verification_procedure(
                    &mut generator,
                    cfg_program,
                    &sc_graph,
                    &unit,
                )?;

                generator.program.procedures.push(procedure);
                programs.push(generator.program);
            }
        }
        Ok(programs)
    }

    /// Create a procedure to verify slice commutativity
    fn create_commutative_verification_procedure(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        sc_graph: &SCGraph,
        unit: &CommutativeUnit,
    ) -> Results<BoogieProcedure> {
        let procedure_name = format!(
            "Check_SliceCommut_Hop{}_vs_Hop{}",
            unit.c_edge.source.hop_id.index(),
            unit.c_edge.target.hop_id.index()
        );

        // Generate all legal interleavings
        let interleavings = self.generate_legal_interleavings(sc_graph, unit);
        let special = self.extract_special_interleavings(&unit.hops_A, &unit.hops_B);

        // Analyze liveness for both slices
        let liveness_info = self.analyze_slice_liveness(cfg_program, &unit.hops_A, &unit.hops_B)?;

        let mut lines = Vec::new();

        // Add procedure header comment
        BoogieProgramGenerator::add_comment(
            &mut lines,
            format!(
                "Slice commutativity verification: hop {} vs hop {}",
                unit.c_edge.source.hop_id.index(),
                unit.c_edge.target.hop_id.index()
            ),
        );

        // Step 1: Havoc all tables and live-in variables to create initial state
        self.havoc_initial_state(generator, cfg_program, &liveness_info, &mut lines)?;

        // Step 2: Save initial state for restoration
        self.save_initial_state(generator, cfg_program, &liveness_info, &mut lines)?;

        // Step 3: Execute the two special interleavings and save their final states
        let (a_then_b_vars, b_then_a_vars) = self.execute_special_interleavings(
            generator,
            cfg_program,
            &special,
            &liveness_info,
            &mut lines,
        )?;

        // Step 4: For each legal interleaving, verify it produces one of the special results
        for (i, interleaving) in interleavings.iter().enumerate() {
            self.verify_interleaving_equivalence(
                generator,
                cfg_program,
                interleaving,
                i,
                &liveness_info,
                &a_then_b_vars,
                &b_then_a_vars,
                &mut lines,
            )?;
        }

        // Generate procedure parameters (empty for this verification)
        let params = Vec::new();

        // Collect all modified globals
        // TODO: use better way to collect modifed globals
        let mut modifies = HashSet::new();

        // Add table variables that might be modified
        for &table_id in &cfg_program.root_tables {
            let table = &cfg_program.tables[table_id];
            for &field_id in &table.fields {
                let field = &cfg_program.fields[field_id];
                modifies.insert(BoogieProgramGenerator::gen_table_field_var_name(
                    &table.name,
                    &field.name,
                ));
            }
        }

        // Add live variables that might be modified
        // TODO: live variable should not be collected
        for var_id in liveness_info.live_vars.iter() {
            modifies.insert(BoogieProgramGenerator::gen_var_name(
                cfg_program,
                *var_id,
                None,
            ));
        }

        let procedure = BoogieProcedure {
            name: procedure_name,
            params,
            modifies: modifies.into_iter().collect(),
            lines,
        };

        Ok(procedure)
    }

    /// Generate all legal interleavings of two hop slices based on conflict constraints
    fn generate_legal_interleavings(
        &self,
        sc_graph: &SCGraph,
        unit: &CommutativeUnit,
    ) -> Vec<Interleaving> {
        // Find all conflict edges between the two slices (excluding the unit's c_edge)
        let conflicts =
            self.find_cross_slice_conflicts(sc_graph, &unit.hops_A, &unit.hops_B, &unit.c_edge);

        // Generate all legal interleavings using the algorithm from verification.md
        self.generate_all_merges(&unit.hops_A, &unit.hops_B, &conflicts)
    }

    /// Find conflict edges between two hop slices, excluding the given edge
    fn find_cross_slice_conflicts(
        &self,
        sc_graph: &SCGraph,
        slice_a: &[HopId],
        slice_b: &[HopId],
        exclude_edge: &SCGraphEdge,
    ) -> HashSet<(HopId, HopId)> {
        let mut conflicts = HashSet::new();
        let slice_a_set: HashSet<HopId> = slice_a.iter().cloned().collect();
        let slice_b_set: HashSet<HopId> = slice_b.iter().cloned().collect();

        for edge in &sc_graph.edges {
            if edge.edge_type == EdgeType::C && edge != exclude_edge {
                let source_hop = edge.source.hop_id;
                let target_hop = edge.target.hop_id;

                // Check if this is a cross-slice conflict
                if (slice_a_set.contains(&source_hop) && slice_b_set.contains(&target_hop))
                    || (slice_b_set.contains(&source_hop) && slice_a_set.contains(&target_hop))
                {
                    conflicts.insert((source_hop, target_hop));
                    conflicts.insert((target_hop, source_hop)); // Add both directions for lookup
                }
            }
        }

        conflicts
    }

    /// Generate all legal merges based on conflict constraints (implements Algorithm 1 from verification.md)
    fn generate_all_merges(
        &self,
        slice_a: &[HopId],
        slice_b: &[HopId],
        conflicts: &HashSet<(HopId, HopId)>,
    ) -> Vec<Interleaving> {
        let mut result = Vec::new();

        fn dfs(
            slice_a: &[HopId],
            slice_b: &[HopId],
            conflicts: &HashSet<(HopId, HopId)>,
            i: usize,
            j: usize,
            placed: &mut Vec<(HopId, bool)>,
            direction: Direction,
            result: &mut Vec<Interleaving>,
        ) {
            // Base case: both slices exhausted
            if i == slice_a.len() && j == slice_b.len() {
                result.push(placed.clone());
                return;
            }

            // Try placing next hop from slice A
            if i < slice_a.len() {
                let hop_a = slice_a[i];
                if let Some(new_dir) =
                    check_direction_constraint(hop_a, true, placed, conflicts, direction)
                {
                    placed.push((hop_a, true));
                    dfs(
                        slice_a,
                        slice_b,
                        conflicts,
                        i + 1,
                        j,
                        placed,
                        new_dir,
                        result,
                    );
                    placed.pop();
                }
            }

            // Try placing next hop from slice B
            if j < slice_b.len() {
                let hop_b = slice_b[j];
                if let Some(new_dir) =
                    check_direction_constraint(hop_b, false, placed, conflicts, direction)
                {
                    placed.push((hop_b, false));
                    dfs(
                        slice_a,
                        slice_b,
                        conflicts,
                        i,
                        j + 1,
                        placed,
                        new_dir,
                        result,
                    );
                    placed.pop();
                }
            }
        }

        fn check_direction_constraint(
            current_hop: HopId,
            is_from_a: bool,
            placed: &[(HopId, bool)],
            conflicts: &HashSet<(HopId, HopId)>,
            current_direction: Direction,
        ) -> Option<Direction> {
            // Scan placed hops from right to left to find the first conflicting hop from opposite slice
            for &(placed_hop, placed_is_from_a) in placed.iter().rev() {
                if placed_is_from_a != is_from_a && conflicts.contains(&(current_hop, placed_hop)) {
                    // Found conflicting pair
                    let required_direction = if is_from_a {
                        Direction::ABeforeB
                    } else {
                        Direction::BBeforeA
                    };

                    match current_direction {
                        Direction::Unknown => return Some(required_direction),
                        dir if dir == required_direction => return Some(dir),
                        _ => return None, // Constraint violation
                    }
                }
            }

            Some(current_direction) // No conflicts found, continue with current direction
        }

        let mut placed = Vec::new();
        dfs(
            slice_a,
            slice_b,
            conflicts,
            0,
            0,
            &mut placed,
            Direction::Unknown,
            &mut result,
        );

        result
    }

    /// Extract the two special interleavings (A+B and B+A)
    fn extract_special_interleavings(
        &self,
        slice_a: &[HopId],
        slice_b: &[HopId],
    ) -> SpecialInterleavings {
        let mut a_then_b = Vec::new();
        let mut b_then_a = Vec::new();

        // A followed by B
        for &hop in slice_a {
            a_then_b.push((hop, true));
        }
        for &hop in slice_b {
            a_then_b.push((hop, false));
        }

        // B followed by A
        for &hop in slice_b {
            b_then_a.push((hop, false));
        }
        for &hop in slice_a {
            b_then_a.push((hop, true));
        }

        SpecialInterleavings { a_then_b, b_then_a }
    }

    /// Analyze liveness for both slices to determine live-in/live-out variables, and also table written and read by each slice
    /// tODO: change its name
    fn analyze_slice_liveness(
        &self,
        cfg_program: &CfgProgram,
        slice_a: &[HopId],
        slice_b: &[HopId],
    ) -> Results<SliceLivenessInfo> {
        use crate::dataflow::{analyze_live_variables, LiveVar};

        let mut all_live_vars = HashSet::new();
        let mut live_in_a = HashSet::new();
        let mut live_out_a = HashSet::new();
        let mut live_in_b = HashSet::new();
        let mut live_out_b = HashSet::new();

        // Analyze liveness for each hop in both slices
        for &hop_id in slice_a.iter().chain(slice_b.iter()) {
            let hop = &cfg_program.hops[hop_id];

            //TODO: take commutative unit as function input so no need to find the function id then, it is stored in the edge
            let function = cfg_program
                .functions
                .iter()
                .find(|(_, func)| func.hops.contains(&hop_id))
                .map(|(_, func)| func)
                .ok_or_else(|| {
                    vec![SpannedError {
                        error: VerificationError::StringConstantNotSupported, // TODO: create another error in errors.rs
                        span: None,
                    }]
                })?;

            let liveness_results = analyze_live_variables(function, cfg_program);

            // Collect live variables from all basic blocks in this hop
            // TODO: for each slice, take the entry point of the beginning of slices and the exit point of the end of slices.
            // TODO: entry point should be HopCfg.entry_block. exit point should be all the BB's successors where the edge of
            //          type hopexit/return/abort, store them directly, the live var in between should not be stored
            for &block_id in &hop.blocks {
                if let Some(live_vars) = liveness_results.block_entry.get(&block_id) {
                    if let Some(var_set) = live_vars.as_set() {
                        for LiveVar(var_id) in var_set {
                            all_live_vars.insert(*var_id);

                            if slice_a.contains(&hop_id) {
                                if hop_id == slice_a[0] {
                                    live_in_a.insert(*var_id);
                                }
                                if hop_id == slice_a[slice_a.len() - 1] {
                                    live_out_a.insert(*var_id);
                                }
                            }

                            if slice_b.contains(&hop_id) {
                                if hop_id == slice_b[0] {
                                    live_in_b.insert(*var_id);
                                }
                                if hop_id == slice_b[slice_b.len() - 1] {
                                    live_out_b.insert(*var_id);
                                }
                            }
                        }
                    }
                }
            }
        }

        // TODO: table written and read by each slice
        // Please use the table_mod_ref.rs, it is a hop level analysis, you just go to the end of each hop, hence all the BB's successors where the edge of
            //          type hopexit/return/abort and union the read part and write parts.
        //            Make sure the sliceA and sliceB are stored separately
        Ok(SliceLivenessInfo {
            live_vars: all_live_vars.into_iter().collect(),
            live_in_a,
            live_out_a,
            live_in_b,
            live_out_b,
        })
    }

    /// Havoc all tables and live-in variables to create initial state
    fn havoc_initial_state(
        &self,
        _generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        liveness_info: &SliceLivenessInfo,
        lines: &mut Vec<BoogieLine>,
    ) -> Results<()> {
        BoogieProgramGenerator::add_comment(
            lines,
            "--- Step 1: Havoc initial state ---".to_string(),
        );

        // Havoc all table variables
        // TODO: just havoc table r/w by hops, use the analysis info, use gen_table_field_var_name() from gen_Boogie. Also, for the field that is primary, no variable should be generated.
        // please add BoogieLine::Havoc and use this not comments
        for &table_id in &cfg_program.root_tables {
            let table = &cfg_program.tables[table_id];
            for &field_id in &table.fields {
                let field = &cfg_program.fields[field_id];
                let var_name =
                    BoogieProgramGenerator::gen_table_field_var_name(&table.name, &field.name);
                lines.push(BoogieLine::Comment(format!("havoc {};", var_name)));
            }
        }

        // Havoc all live variables, please add BoogieLine::Havoc and use this not comments
        // only live-IN variable should be havoc
        for &var_id in &liveness_info.live_vars {
            let var_name = BoogieProgramGenerator::gen_var_name(cfg_program, var_id, None);
            lines.push(BoogieLine::Comment(format!("havoc {};", var_name)));
        }

        Ok(())
    }

    /// Save initial state by copying variables to snapshot versions
    fn save_initial_state(
        &self,
        _generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        liveness_info: &SliceLivenessInfo,
        lines: &mut Vec<BoogieLine>,
    ) -> Results<()> {
        BoogieProgramGenerator::add_comment(
            lines,
            "--- Step 2: Save initial state ---".to_string(),
        );

        // Save table variables
        for &table_id in &cfg_program.root_tables {
            let table = &cfg_program.tables[table_id];
            for &field_id in &table.fields {
                let field = &cfg_program.fields[field_id];
                let var_name =
                    BoogieProgramGenerator::gen_table_field_var_name(&table.name, &field.name);
                let snapshot_name = format!("{}_init", var_name);
                lines.push(BoogieLine::Comment(format!(
                    "{} := {};",
                    snapshot_name, var_name
                )));
            }
        }

        // Save live variables, only live-IN variable should be saved
        for &var_id in &liveness_info.live_vars {
            let var_name = BoogieProgramGenerator::gen_var_name(cfg_program, var_id, None);
            let snapshot_name = format!("{}_init", var_name);
            lines.push(BoogieLine::Comment(format!(
                "{} := {};",
                snapshot_name, var_name
            )));
        }

        Ok(())
    }

    /// Execute the two special interleavings and return their final state variable names
    fn execute_special_interleavings(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        special: &SpecialInterleavings,
        liveness_info: &SliceLivenessInfo,
        lines: &mut Vec<BoogieLine>,
    ) -> Results<(VariableSnapshots, VariableSnapshots)> {
        BoogieProgramGenerator::add_comment(
            lines,
            "--- Step 3: Execute special interleavings ---".to_string(),
        );

        // Execute A then B
        BoogieProgramGenerator::add_comment(lines, "Executing A then B:".to_string());
        let a_then_b_vars = self.execute_interleaving_and_snapshot(
            generator,
            cfg_program,
            &special.a_then_b,
            "a_then_b",
            liveness_info,
            lines,
            true,
        )?;

        // Reset state
        self.restore_initial_state(generator, cfg_program, liveness_info, lines)?;

        // Execute B then A
        BoogieProgramGenerator::add_comment(lines, "Executing B then A:".to_string());
        let b_then_a_vars = self.execute_interleaving_and_snapshot(
            generator,
            cfg_program,
            &special.b_then_a,
            "b_then_a",
            liveness_info,
            lines,
            true,
        )?;

        Ok((a_then_b_vars, b_then_a_vars))
    }

    /// Execute a specific interleaving and snapshot the final state
    fn execute_interleaving_and_snapshot(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        interleaving: &Interleaving,
        suffix: &str,
        liveness_info: &SliceLivenessInfo,
        lines: &mut Vec<BoogieLine>,
        is_special: bool,
    ) -> Results<VariableSnapshots> {
        // Execute each hop in the interleaving
        for (i, &(hop_id, _is_from_a)) in interleaving.iter().enumerate() {
            self.execute_hop_with_unique_labels(generator, cfg_program, hop_id, i, suffix, lines)?;
        }

        // Snapshot final state if this is a special interleaving
        if is_special {
            self.snapshot_final_state(generator, cfg_program, liveness_info, suffix, lines)
        } else {
            // For regular interleavings, just return empty snapshots
            Ok(VariableSnapshots {
                table_snapshots: HashMap::new(),
                var_snapshots: HashMap::new(),
            })
        }
    }

    /// Execute a single hop with unique labels to avoid conflicts
    fn execute_hop_with_unique_labels(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        hop_id: HopId,
        execution_index: usize,
        suffix: &str,
        lines: &mut Vec<BoogieLine>,
    ) -> Results<()> {
        let hop = &cfg_program.hops[hop_id];

        BoogieProgramGenerator::add_comment(
            lines,
            format!(
                "Executing hop {} (execution {})",
                hop_id.index(),
                execution_index
            ),
        );

        // Process each basic block in the hop
        for (block_idx, &block_id) in hop.blocks.iter().enumerate() {
            let block = &cfg_program.blocks[block_id];

            // Generate unique label
            let label = format!(
                "hop_{}_{}_exec_{}_{}",
                hop_id.index(),
                block_idx,
                execution_index,
                suffix
            );
            lines.push(BoogieLine::Label(label));

            // Convert statements
            for statement in &block.statements {
                let boogie_lines = generator.convert_statement(cfg_program, statement, None)?;
                lines.extend(boogie_lines);
            }

            // Add control flow edges with unique labels
            self.add_hop_control_flow_edges(
                generator,
                cfg_program,
                block_id,
                execution_index,
                suffix,
                lines,
            )?;
        }

        Ok(())
    }

    /// Add control flow edges for a hop execution with unique labels
    /// TODO: use gen_Boogie.rs ones, you might need to modify it, but don't create your own one
    fn add_hop_control_flow_edges(
        &self,
        _generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        block_id: crate::cfg::BasicBlockId,
        execution_index: usize,
        suffix: &str,
        lines: &mut Vec<BoogieLine>,
    ) -> Results<()> {
        let block = &cfg_program.blocks[block_id];

        for edge in &block.successors {
            match &edge.edge_type {
                crate::cfg::EdgeType::Unconditional => {
                    let target_hop = &cfg_program.blocks[edge.to].hop_id;
                    let target_label = format!(
                        "hop_{}_{}_exec_{}_{}",
                        target_hop.index(),
                        0,
                        execution_index,
                        suffix
                    );
                    lines.push(BoogieLine::Goto(target_label));
                }
                crate::cfg::EdgeType::ConditionalTrue { condition } => {
                    let cond_expr = _generator.convert_operand(cfg_program, condition, None)?;
                    let target_hop = &cfg_program.blocks[edge.to].hop_id;
                    let true_label = format!(
                        "hop_{}_{}_exec_{}_{}",
                        target_hop.index(),
                        0,
                        execution_index,
                        suffix
                    );
                    let false_label = format!(
                        "hop_{}_{}_exec_{}_{}_false",
                        target_hop.index(),
                        0,
                        execution_index,
                        suffix
                    );

                    lines.push(BoogieLine::If {
                        cond: cond_expr,
                        then_body: vec![Box::new(BoogieLine::Goto(true_label))],
                        else_body: vec![Box::new(BoogieLine::Goto(false_label))],
                    });
                }
                _ => {} // Other edge types handled implicitly
            }
        }

        Ok(())
    }

    /// Snapshot the final state of variables after execution
    fn snapshot_final_state(
        &self,
        _generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        liveness_info: &SliceLivenessInfo,
        suffix: &str,
        lines: &mut Vec<BoogieLine>,
    ) -> Results<VariableSnapshots> {
        let mut table_snapshots = HashMap::new();
        let mut var_snapshots = HashMap::new();

        BoogieProgramGenerator::add_comment(
            lines,
            format!("Snapshotting final state for {}", suffix),
        );

        // Snapshot table variables, you should just use analysis info
        for &table_id in &cfg_program.root_tables {
            let table = &cfg_program.tables[table_id];
            for &field_id in &table.fields {
                let field = &cfg_program.fields[field_id];
                let var_name =
                    BoogieProgramGenerator::gen_table_field_var_name(&table.name, &field.name);
                let snapshot_name = format!("{}_{}", var_name, suffix);
                table_snapshots.insert(var_name.clone(), snapshot_name.clone());
                lines.push(BoogieLine::Comment(format!(
                    "{} := {};",
                    snapshot_name, var_name
                )));
            }
        }

        // Snapshot live variables, only need to snapshot live-OUT variables
        for &var_id in &liveness_info.live_vars {
            let var_name = BoogieProgramGenerator::gen_var_name(cfg_program, var_id, None);
            let snapshot_name = format!("{}_{}", var_name, suffix);
            var_snapshots.insert(var_id, snapshot_name.clone());
            lines.push(BoogieLine::Comment(format!(
                "{} := {};",
                snapshot_name, var_name
            )));
        }

        Ok(VariableSnapshots {
            table_snapshots,
            var_snapshots,
        })
    }

    /// Restore initial state from snapshots
    fn restore_initial_state(
        &self,
        _generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        liveness_info: &SliceLivenessInfo,
        lines: &mut Vec<BoogieLine>,
    ) -> Results<()> {
        BoogieProgramGenerator::add_comment(lines, "Restoring initial state:".to_string());

        // Restore table variables, use analysis info
        for &table_id in &cfg_program.root_tables {
            let table = &cfg_program.tables[table_id];
            for &field_id in &table.fields {
                let field = &cfg_program.fields[field_id];
                let var_name =
                    BoogieProgramGenerator::gen_table_field_var_name(&table.name, &field.name);
                let snapshot_name = format!("{}_init", var_name);
                lines.push(BoogieLine::Comment(format!(
                    "{} := {};",
                    var_name, snapshot_name
                )));
            }
        }

        // Restore live variables, only need to restore live-IN variables
        for &var_id in &liveness_info.live_vars {
            let var_name = BoogieProgramGenerator::gen_var_name(cfg_program, var_id, None);
            let snapshot_name = format!("{}_init", var_name);
            lines.push(BoogieLine::Comment(format!(
                "{} := {};",
                var_name, snapshot_name
            )));
        }

        Ok(())
    }

    /// Verify that each interleaving produces results equivalent to one of the special interleavings
    fn verify_interleaving_equivalence(
        &self,
        generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        interleaving: &Interleaving,
        interleaving_index: usize,
        liveness_info: &SliceLivenessInfo,
        a_then_b_vars: &VariableSnapshots,
        b_then_a_vars: &VariableSnapshots,
        lines: &mut Vec<BoogieLine>,
    ) -> Results<()> {
        BoogieProgramGenerator::add_comment(
            lines,
            format!("--- Verifying interleaving {} ---", interleaving_index),
        );

        // Reset to initial state
        self.restore_initial_state(generator, cfg_program, liveness_info, lines)?;

        // Execute this interleaving
        let suffix = format!("interleaving_{}", interleaving_index);
        self.execute_interleaving_and_snapshot(
            generator,
            cfg_program,
            interleaving,
            &suffix,
            liveness_info,
            lines,
            false,
        )?;

        // Generate assertions comparing final state to special interleavings
        self.assert_equivalence_to_special_interleavings(
            generator,
            cfg_program,
            liveness_info,
            a_then_b_vars,
            b_then_a_vars,
            lines,
        )?;

        Ok(())
    }

    /// Assert that current state equals one of the special interleaving states, similarly, use analysis info, compare table written by "last hop" only, and compare live-OUT variable only
    fn assert_equivalence_to_special_interleavings(
        &self,
        _generator: &mut BoogieProgramGenerator,
        cfg_program: &CfgProgram,
        liveness_info: &SliceLivenessInfo,
        a_then_b_vars: &VariableSnapshots,
        b_then_a_vars: &VariableSnapshots,
        lines: &mut Vec<BoogieLine>,
    ) -> Results<()> {
        let mut equality_conditions_a_then_b = Vec::new();
        let mut equality_conditions_b_then_a = Vec::new();

        // Compare table variables
        for &table_id in &cfg_program.root_tables {
            let table = &cfg_program.tables[table_id];
            for &field_id in &table.fields {
                let field = &cfg_program.fields[field_id];
                let var_name =
                    BoogieProgramGenerator::gen_table_field_var_name(&table.name, &field.name);

                // Current var == a_then_b snapshot
                if let Some(a_then_b_snapshot) = a_then_b_vars.table_snapshots.get(&var_name) {
                    let current_expr = BoogieExpr {
                        kind: BoogieExprKind::Var(var_name.clone()),
                    };
                    let a_then_b_expr = BoogieExpr {
                        kind: BoogieExprKind::Var(a_then_b_snapshot.clone()),
                    };
                    equality_conditions_a_then_b.push(BoogieExpr {
                        kind: BoogieExprKind::BinOp(
                            Box::new(current_expr.clone()),
                            BoogieBinOp::Eq,
                            Box::new(a_then_b_expr),
                        ),
                    });
                }

                // Current var == b_then_a snapshot
                if let Some(b_then_a_snapshot) = b_then_a_vars.table_snapshots.get(&var_name) {
                    let current_expr = BoogieExpr {
                        kind: BoogieExprKind::Var(var_name),
                    };
                    let b_then_a_expr = BoogieExpr {
                        kind: BoogieExprKind::Var(b_then_a_snapshot.clone()),
                    };
                    equality_conditions_b_then_a.push(BoogieExpr {
                        kind: BoogieExprKind::BinOp(
                            Box::new(current_expr),
                            BoogieBinOp::Eq,
                            Box::new(b_then_a_expr),
                        ),
                    });
                }
            }
        }

        // Compare live variables
        for &var_id in &liveness_info.live_vars {
            let var_name = BoogieProgramGenerator::gen_var_name(cfg_program, var_id, None);

            // Current var == a_then_b snapshot
            if let Some(a_then_b_snapshot) = a_then_b_vars.var_snapshots.get(&var_id) {
                let current_expr = BoogieExpr {
                    kind: BoogieExprKind::Var(var_name.clone()),
                };
                let a_then_b_expr = BoogieExpr {
                    kind: BoogieExprKind::Var(a_then_b_snapshot.clone()),
                };
                equality_conditions_a_then_b.push(BoogieExpr {
                    kind: BoogieExprKind::BinOp(
                        Box::new(current_expr.clone()),
                        BoogieBinOp::Eq,
                        Box::new(a_then_b_expr),
                    ),
                });
            }

            // Current var == b_then_a snapshot
            if let Some(b_then_a_snapshot) = b_then_a_vars.var_snapshots.get(&var_id) {
                let current_expr = BoogieExpr {
                    kind: BoogieExprKind::Var(var_name),
                };
                let b_then_a_expr = BoogieExpr {
                    kind: BoogieExprKind::Var(b_then_a_snapshot.clone()),
                };
                equality_conditions_b_then_a.push(BoogieExpr {
                    kind: BoogieExprKind::BinOp(
                        Box::new(current_expr),
                        BoogieBinOp::Eq,
                        Box::new(b_then_a_expr),
                    ),
                });
            }
        }

        // Create final assertion: (current == a_then_b) OR (current == b_then_a)
        let a_then_b_equal = BoogieProgramGenerator::gen_conjunction(equality_conditions_a_then_b);
        let b_then_a_equal = BoogieProgramGenerator::gen_conjunction(equality_conditions_b_then_a);

        let final_assertion =
            BoogieProgramGenerator::gen_disjunction(vec![a_then_b_equal, b_then_a_equal]);

        let error_msg = ErrorMessage {
            msg: "Slice commutativity violation: interleaving produces different result than both special orderings".to_string(),
        };

        BoogieProgramGenerator::add_assertion(lines, final_assertion, error_msg);

        Ok(())
    }
}
