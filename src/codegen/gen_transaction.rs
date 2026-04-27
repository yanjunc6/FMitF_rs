use super::{
    gen_helper::{
        collect_used_labels, collect_var_decls_from_instruction, lower_optimized_op,
        lower_optimized_ops, lower_terminator_goto, operand_to_go, CodeGenContext, HopContext,
    },
    gen_table_optimizer::{optimize_instructions, OptimizedOp},
    util::{go_type_string, go_var_name, pascal_case, snake_case, write_go_file_header},
    GoProgram,
};
use crate::cfg::{self, FunctionKind, InstructionKind};
use crate::dataflow::{
    analyze_live_variables, analyze_live_variables_hop, analyze_reaching_definitions_hop,
    analyze_table_mod_ref, AccessType, LiveVar,
};
use crate::sc_graph::{
    calculate_conflicts, determine_hop_types, CombinedSCGraph, HopType, SCGraph,
};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::Write;

pub fn generate_transactions(
    program: &cfg::Program,
    sc_graph: &SCGraph,
    combined_graph: &CombinedSCGraph,
) -> Result<Vec<GoProgram>, Box<dyn Error>> {
    let mut files = Vec::new();

    // Determine hop types from the combined (merged) graph
    let hop_types = determine_hop_types(combined_graph);

    // Calculate conflicts from the normal SC-graph
    let conflicts = calculate_conflicts(sc_graph, program);

    for &function_id in &program.all_transactions {
        let function = &program.functions[function_id];
        if !matches!(function.kind, FunctionKind::Transaction) {
            continue;
        }

        let struct_name = format!("{}Chain", pascal_case(&function.name));
        let file_name = format!("{}_chain.go", snake_case(&function.name));

        let mut content = String::new();
        write_go_file_header(
            &mut content,
            &[
                "encoding/json",
                "github.com/boltdb/bolt",
                "ShardDB/proto",
                "ShardDB/util",
            ],
        )?;

        writeln!(content, "type {} struct {{", struct_name)?;
        writeln!(content, "\tdb   *bolt.DB")?;
        writeln!(content, "\thops []*Hop")?;
        writeln!(content, "}}")?;
        writeln!(content)?;

        // Generate each hop function
        for (hop_index, hop_id) in function.hops.iter().enumerate() {
            let next_hop_id = function.hops.get(hop_index + 1).copied();
            generate_hop_function(
                &mut content,
                program,
                function_id,
                hop_index,
                *hop_id,
                next_hop_id,
            )?;
        }

        // Generate partition hop functions
        for (hop_index, hop_id) in function.hops.iter().enumerate() {
            generate_partition_hop_function(
                &mut content,
                program,
                function_id,
                hop_index,
                *hop_id,
            )?;
        }

        // Generate aggregate functions for global hops
        for (hop_index, hop_id) in function.hops.iter().enumerate() {
            generate_aggregate_function(&mut content, program, function_id, hop_index, *hop_id)?;
        }

        // Generate NextReq function
        generate_next_req(&mut content, program, function_id)?;

        // Generate ChainImpl function
        generate_chain_impl(
            &mut content,
            program,
            sc_graph,
            function_id,
            &hop_types,
            &conflicts,
        )?;

        // Generate ToJson function
        generate_to_json(&mut content, program, function_id)?;

        files.push(GoProgram::new(file_name, content));
    }

    Ok(files)
}

fn generate_hop_function(
    out: &mut String,
    program: &cfg::Program,
    function_id: cfg::FunctionId,
    hop_index: usize,
    hop_id: cfg::HopId,
    next_hop_id: Option<cfg::HopId>,
) -> Result<(), std::fmt::Error> {
    generate_hop_function_impl(
        out,
        program,
        function_id,
        hop_index,
        hop_id,
        next_hop_id,
        CodeGenContext::Normal,
    )
}

fn generate_partition_hop_function(
    out: &mut String,
    program: &cfg::Program,
    function_id: cfg::FunctionId,
    hop_index: usize,
    hop_id: cfg::HopId,
) -> Result<(), std::fmt::Error> {
    // Check if hop has @global decorator
    let hop = &program.hops[hop_id];
    let is_global = hop.decorators.iter().any(|d| d.name == "global");

    // Skip generating partition function for global hops
    if is_global {
        return Ok(());
    }

    generate_hop_function_impl(
        out,
        program,
        function_id,
        hop_index,
        hop_id,
        None,
        CodeGenContext::TransactionPartition,
    )
}

fn generate_aggregate_function(
    out: &mut String,
    program: &cfg::Program,
    function_id: cfg::FunctionId,
    hop_index: usize,
    hop_id: cfg::HopId,
) -> Result<(), std::fmt::Error> {
    // Check if hop has @global decorator
    let hop = &program.hops[hop_id];
    let is_global = hop.decorators.iter().any(|d| d.name == "global");

    // Skip generating aggregate function for non-global hops
    if !is_global {
        return Ok(());
    }

    let function = &program.functions[function_id];
    let func_name = pascal_case(&function.name);
    let aggregate_name = format!("{}Hop{}Aggregate", func_name, hop_index);

    writeln!(
        out,
        "// {} aggregates results from multiple shards for hop {}",
        aggregate_name, hop_index
    )?;
    writeln!(
        out,
        "func {}(results [][]string, params map[string]string) map[string]string {{",
        aggregate_name
    )?;

    // Determine the return variables by analyzing what's written back
    // Run function-level liveness analysis
    let func_liveness = analyze_live_variables(function, program);
    let hop_reaching_def = analyze_reaching_definitions_hop(function, program);

    // Get live-out variables for the next hop
    // For global hops, if this is the last hop, we need to check function return instead
    let mut func_live_out = if let Some(next_hop_id) = function.hops.get(hop_index + 1) {
        let next_hop = &program.hops[*next_hop_id];
        if let Some(next_entry) = next_hop.entry_block {
            func_liveness
                .block_entry
                .get(&next_entry)
                .and_then(|lattice| lattice.as_set())
                .map(|set| set.iter().map(|LiveVar(v)| *v).collect::<Vec<_>>())
                .unwrap_or_default()
        } else {
            Vec::new()
        }
    } else {
        // For the last hop, check what's returned
        // Look for variables that reach the return or hop exit blocks
        let mut live_at_exit = Vec::new();
        for &block_id in &hop.blocks {
            let block = &program.basic_blocks[block_id];
            match &block.terminator {
                cfg::Terminator::HopExit { .. } | cfg::Terminator::Return(_) => {
                    if let Some(live_out) = func_liveness.block_exit.get(&block_id) {
                        if let Some(vars) = live_out.as_set() {
                            for LiveVar(var_id) in vars {
                                if !live_at_exit.contains(var_id) {
                                    live_at_exit.push(*var_id);
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        live_at_exit
    };
    func_live_out.sort_by_key(|var_id| var_id.index());

    // Get variables defined in this hop
    let mut vars_defined_in_hop = HashSet::new();
    for &block_id in &hop.blocks {
        if let Some(exit_state) = hop_reaching_def.block_exit.get(&block_id) {
            if let Some(defs) = exit_state.as_set() {
                for def in defs {
                    vars_defined_in_hop.insert(def.var);
                }
            }
        }
    }

    let explicit_return_vars = collect_explicit_return_vars(program, hop);

    // Variables to write back: live-out AND defined in this hop, plus explicit to_unit_return vars
    let mut vars_to_write_back: Vec<cfg::VariableId> = func_live_out
        .iter()
        .filter(|var_id| vars_defined_in_hop.contains(var_id))
        .copied()
        .collect();

    for var_id in explicit_return_vars {
        if !vars_to_write_back.contains(&var_id) {
            vars_to_write_back.push(var_id);
        }
    }

    vars_to_write_back.sort_by_key(|var_id| var_id.index());

    // Generate aggregation code for each return variable
    for (idx, var_id) in vars_to_write_back.iter().enumerate() {
        let var = &program.variables[*var_id];
        let var_name = go_var_name(program, *var_id);
        let go_type = go_type_string(program, var.ty);

        writeln!(out, "\t// Define typed list for {}", var_name)?;
        writeln!(out, "\tvar {} {}", var_name, go_type)?;
        writeln!(
            out,
            "\tparams[\"{}\"] = ListConcatenate(results, {})",
            var_name, var_name
        )?;

        // Add blank line between variables if there are multiple
        if idx < vars_to_write_back.len() - 1 {
            writeln!(out)?;
        }
    }

    writeln!(out, "\treturn params")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    Ok(())
}

fn collect_explicit_return_vars(program: &cfg::Program, hop: &cfg::Hop) -> Vec<cfg::VariableId> {
    let mut vars = Vec::new();
    let mut seen = HashSet::new();

    for &block_id in &hop.blocks {
        let block = &program.basic_blocks[block_id];
        for inst in &block.instructions {
            if let InstructionKind::Call { func, args, .. } = &inst.kind {
                let called = &program.functions[*func];
                if called.name == "to_unit_return" {
                    for arg in args {
                        if let cfg::Operand::Variable(var_id) = arg {
                            if seen.insert(*var_id) {
                                vars.push(*var_id);
                            }
                        }
                    }
                }
            }
        }
    }

    vars
}

fn generate_hop_function_impl(
    out: &mut String,
    program: &cfg::Program,
    function_id: cfg::FunctionId,
    hop_index: usize,
    hop_id: cfg::HopId,
    next_hop_id: Option<cfg::HopId>,
    ctx: CodeGenContext,
) -> Result<(), std::fmt::Error> {
    let function = &program.functions[function_id];
    let hop = &program.hops[hop_id];

    let hop_name = if ctx.is_partition() {
        format!("{}Hop{}Par", pascal_case(&function.name), hop_index)
    } else {
        format!("{}Hop{}", pascal_case(&function.name), hop_index)
    };

    if ctx.is_partition() {
        writeln!(
            out,
            "// {} calculates the partition for hop {} without database access.",
            hop_name, hop_index
        )?;
        writeln!(out, "func {}(params map[string]string) uint64 {{", hop_name)?;
    } else {
        writeln!(
            out,
            "// {} lowers hop {} containing {} basic block(s).",
            hop_name,
            hop_index,
            hop.blocks.len()
        )?;
        writeln!(
            out,
            "func {}(tx *bolt.Tx, in *proto.TrxReq) (*proto.TrxRes, error) {{",
            hop_name
        )?;
        writeln!(out, "\tvar rwSet []*proto.RWSet")?;
        writeln!(out, "\tparams := in.Params")?;
        writeln!(out)?;
    }

    // Run hop-level liveness analysis to determine which variables are used in this hop
    let hop_liveness = analyze_live_variables_hop(function, program);

    // Run function-level liveness analysis to determine live-out variables
    let func_liveness = analyze_live_variables(function, program);

    // Run hop-level reaching definitions to determine which variables are defined in this hop
    let hop_reaching_def = analyze_reaching_definitions_hop(function, program);

    // Get live-in variables for this hop (variables used in this hop)
    let entry_block = hop.entry_block.expect("Hop must have entry block");
    let mut live_in_set = hop_liveness
        .block_entry
        .get(&entry_block)
        .and_then(|lattice| lattice.as_set())
        .map(|set| set.iter().map(|LiveVar(v)| *v).collect::<Vec<_>>())
        .unwrap_or_default();
    live_in_set.sort_by_key(|var_id| var_id.index());

    // Get live-out variables at function level for the next hop
    // For partition mode, we still need to determine live-out even though next_hop_id is None
    let mut func_live_out = if let Some(next_id) = next_hop_id {
        let next_hop = &program.hops[next_id];
        if let Some(next_entry) = next_hop.entry_block {
            func_liveness
                .block_entry
                .get(&next_entry)
                .and_then(|lattice| lattice.as_set())
                .map(|set| set.iter().map(|LiveVar(v)| *v).collect::<Vec<_>>())
                .unwrap_or_default()
        } else {
            Vec::new()
        }
    } else if ctx.is_partition() {
        // For partition mode, find the next hop from the function's hop list
        if let Some(next_hop_id_from_list) = function.hops.get(hop_index + 1) {
            let next_hop = &program.hops[*next_hop_id_from_list];
            if let Some(next_entry) = next_hop.entry_block {
                func_liveness
                    .block_entry
                    .get(&next_entry)
                    .and_then(|lattice| lattice.as_set())
                    .map(|set| set.iter().map(|LiveVar(v)| *v).collect::<Vec<_>>())
                    .unwrap_or_default()
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        }
    } else {
        // No next hop - this is the last hop
        // Check if any block has a Return terminator with an operand
        let mut return_vars = Vec::new();
        for &block_id in &hop.blocks {
            let block = &program.basic_blocks[block_id];
            if let cfg::Terminator::Return(Some(cfg::Operand::Variable(var_id))) = block.terminator
            {
                if !return_vars.contains(&var_id) {
                    return_vars.push(var_id);
                }
            }
        }
        return_vars
    };
    func_live_out.sort_by_key(|var_id| var_id.index());

    // Get variables defined in this hop (from reaching definitions at hop exit)
    let mut vars_defined_in_hop = HashSet::new();
    for &block_id in &hop.blocks {
        if let Some(exit_state) = hop_reaching_def.block_exit.get(&block_id) {
            if let Some(defs) = exit_state.as_set() {
                for def in defs {
                    vars_defined_in_hop.insert(def.var);
                }
            }
        }
    }

    let explicit_return_vars = collect_explicit_return_vars(program, hop);

    // Variables to write back: live-out AND defined in this hop, plus explicit to_unit_return vars
    let mut vars_to_write_back: Vec<cfg::VariableId> = func_live_out
        .iter()
        .filter(|var_id| vars_defined_in_hop.contains(var_id))
        .copied()
        .collect();

    for var_id in explicit_return_vars {
        if !vars_to_write_back.contains(&var_id) {
            vars_to_write_back.push(var_id);
        }
    }

    vars_to_write_back.sort_by_key(|var_id| var_id.index());

    // Run table mod/ref analysis at HOP level to determine which tables are written
    let table_analysis = analyze_table_mod_ref(function, program);
    let mut tables_written = HashSet::new();
    for &block_id in &hop.blocks {
        if let Some(exit_state) = table_analysis.block_exit.get(&block_id) {
            if let Some(accesses) = exit_state.as_set() {
                for access in accesses {
                    if access.access_type == AccessType::Write {
                        tables_written.insert(access.table);
                    }
                }
            }
        }
    }

    // Collect all variables that need declaration
    let mut var_decls: HashMap<cfg::VariableId, String> = HashMap::new();
    let mut initialized_vars: HashSet<cfg::VariableId> = HashSet::new();

    // Declare live-in variables
    for var_id in &live_in_set {
        let var = &program.variables[*var_id];
        // Use go_type_string for all types to properly handle UUID, Row<T>, Iterator<T>, etc.
        let decl_type = go_type_string(program, var.ty);
        var_decls.insert(*var_id, decl_type);
    }

    // Collect variables from all instructions in the hop
    for &block_id in &hop.blocks {
        let block = &program.basic_blocks[block_id];
        for inst in &block.instructions {
            collect_var_decls_from_instruction(program, inst, &mut var_decls);
        }
    }

    // OPTIMIZATION PREPROCESSING: Transform instructions into optimized operations
    let mut all_optimized_ops: Vec<(cfg::BasicBlockId, Vec<OptimizedOp>)> = Vec::new();
    let mut table_var_count = 0;

    for &block_id in &hop.blocks {
        let block = &program.basic_blocks[block_id];
        let optimized_ops = optimize_instructions(&block.instructions);

        // Count table variables needed for this block
        for op in &optimized_ops {
            match op {
                OptimizedOp::CombinedTableAccess(_) => {
                    table_var_count += 1; // One get/put pair for optimized group
                }
                OptimizedOp::Single(inst) => {
                    if matches!(
                        inst.kind,
                        InstructionKind::TableGet { .. } | InstructionKind::TableSet { .. }
                    ) {
                        table_var_count += 1; // Individual table access
                    }
                }
            }
        }

        all_optimized_ops.push((block_id, optimized_ops));
    }

    // Output variable declarations
    if !var_decls.is_empty() {
        let mut decls: Vec<_> = var_decls.iter().collect();
        decls.sort_by_key(|(var_id, _)| var_id.index());
        for (var_id, ty) in decls {
            let var_name = go_var_name(program, *var_id);
            writeln!(out, "\tvar {} {}", var_name, ty)?;
        }
        writeln!(out)?;
    }

    // Declare keyBytes and row variables based on actual count needed
    if table_var_count > 0 {
        // Collect table types needed across all optimized operations
        let mut table_types_needed: Vec<String> = Vec::new();
        for (_, ops) in &all_optimized_ops {
            for op in ops {
                match op {
                    OptimizedOp::CombinedTableAccess(group) => {
                        let table_info = &program.tables[group.table_id];
                        table_types_needed.push(pascal_case(&table_info.name));
                    }
                    OptimizedOp::Single(inst) => {
                        if let InstructionKind::TableGet { table, .. }
                        | InstructionKind::TableSet { table, .. } = &inst.kind
                        {
                            let table_info = &program.tables[*table];
                            table_types_needed.push(pascal_case(&table_info.name));
                        }
                    }
                }
            }
        }

        for (i, table_name) in table_types_needed.iter().enumerate() {
            let idx = i + 1;
            writeln!(out, "\tvar keyBytes{} []byte", idx)?;
            writeln!(out, "\tvar row{} {}", idx, table_name)?;
        }
        writeln!(out)?;
    }

    // Extract live-in variables from params
    for var_id in &live_in_set {
        let var = &program.variables[*var_id];
        let var_name = go_var_name(program, *var_id);
        let var_type = &program.types[var.ty];

        match var_type {
            cfg::Type::Primitive(cfg::PrimitiveType::Int) => {
                writeln!(out, "\t{} = toUint64(params[\"{}\"])", var_name, var_name)?;
            }
            cfg::Type::Primitive(cfg::PrimitiveType::Float) => {
                writeln!(out, "\t{} = toFloat32(params[\"{}\"])", var_name, var_name)?;
            }
            cfg::Type::Primitive(cfg::PrimitiveType::Bool) => {
                writeln!(out, "\t{} = toBool(params[\"{}\"])", var_name, var_name)?;
            }
            cfg::Type::Primitive(cfg::PrimitiveType::String) => {
                writeln!(out, "\t{} = params[\"{}\"]", var_name, var_name)?;
            }
            cfg::Type::Declared { type_id, .. } => {
                let user_type = &program.user_defined_types[*type_id];
                if user_type.name == "UUID" {
                    // UUID is uint64, so use toUint64
                    writeln!(
                        out,
                        "\t{} = UUID(toUint64(params[\"{}\"]))",
                        var_name, var_name
                    )?;
                } else {
                    // For other user-defined types, use json.Unmarshal
                    writeln!(
                        out,
                        "\tjson.Unmarshal([]byte(params[\"{}\"]), &{})",
                        var_name, var_name
                    )?;
                }
            }
            cfg::Type::List(_) | cfg::Type::Row { .. } => {
                writeln!(
                    out,
                    "\tjson.Unmarshal([]byte(params[\"{}\"]), &{})",
                    var_name, var_name
                )?;
            }
            _ => {
                writeln!(
                    out,
                    "\tjson.Unmarshal([]byte(params[\"{}\"]), &{})",
                    var_name, var_name
                )?;
            }
        }

        initialized_vars.insert(*var_id);
    }

    if !live_in_set.is_empty() {
        writeln!(out)?;
    }

    // For partition mode, create fake variables for unreachable code
    if ctx.is_partition() {
        writeln!(
            out,
            "\tvar tx *bolt.Tx = nil // Fake tx for unreachable code"
        )?;
        writeln!(
            out,
            "\tvar rwSet []*proto.RWSet // Fake rwSet for unreachable code"
        )?;
        writeln!(out, "\t_ = tx // Suppress unused variable warning")?;
        writeln!(out, "\t_ = rwSet // Suppress unused variable warning")?;
        writeln!(out)?;
    }

    let mut table_access_count = 0;

    // Track which labels are actually used (targets of goto statements)
    let mut used_labels = HashSet::new();
    for &block_id in &hop.blocks {
        let block = &program.basic_blocks[block_id];
        collect_used_labels(&block.terminator, &mut used_labels);
    }

    // Add initial goto to entry block to ensure all labels are reachable
    writeln!(out, "\tgoto BB{}", entry_block.index())?;
    writeln!(out)?;

    // Generate code for entry block with label
    writeln!(out, "BB{}:", entry_block.index())?;
    let entry_block_obj = &program.basic_blocks[entry_block];

    // Find the optimized ops for the entry block
    let (_, entry_ops) = all_optimized_ops
        .iter()
        .find(|(bid, _)| *bid == entry_block)
        .unwrap();

    if ctx.is_partition() {
        // Partition mode: generate code with early returns after table accesses
        for op in entry_ops {
            if let Some(shard) = check_partition_from_op(
                out,
                program,
                op,
                "\t",
                &mut initialized_vars,
                &mut table_access_count,
            )? {
                writeln!(out, "\tif true {{ return {} }}", shard)?;
            }
            // Generate unreachable code (for Go compiler variable usage)
            lower_optimized_op(
                out,
                program,
                op,
                &mut initialized_vars,
                &mut table_access_count,
                "\t",
            )?;
        }
    } else {
        // Normal mode: generate regular code
        lower_optimized_ops(
            out,
            program,
            entry_ops,
            &mut initialized_vars,
            &mut table_access_count,
            "\t",
        )?;
    }

    lower_terminator_goto(
        out,
        program,
        &entry_block_obj.terminator,
        "\t",
        Some(HopContext {
            initialized_vars: &initialized_vars,
            vars_to_write_back: &vars_to_write_back,
            tables_written: &tables_written,
        }),
        ctx,
    )?; // Generate code for other blocks in the hop (only with label if used)
    for &block_id in &hop.blocks {
        if block_id == entry_block {
            continue; // Already generated
        }

        let block = &program.basic_blocks[block_id];

        // Only generate label if it's used
        if used_labels.contains(&block_id) {
            writeln!(out, "BB{}:", block_id.index())?;
        }

        // Find the optimized ops for this block
        let (_, block_ops) = all_optimized_ops
            .iter()
            .find(|(bid, _)| *bid == block_id)
            .unwrap();

        if ctx.is_partition() {
            // Partition mode: generate code with early returns after table accesses
            for op in block_ops {
                if let Some(shard) = check_partition_from_op(
                    out,
                    program,
                    op,
                    "\t",
                    &mut initialized_vars,
                    &mut table_access_count,
                )? {
                    writeln!(out, "\tif true {{ return {} }}", shard)?;
                }
                // Generate unreachable code (for Go compiler variable usage)
                lower_optimized_op(
                    out,
                    program,
                    op,
                    &mut initialized_vars,
                    &mut table_access_count,
                    "\t",
                )?;
            }
        } else {
            // Normal mode: generate regular code
            lower_optimized_ops(
                out,
                program,
                block_ops,
                &mut initialized_vars,
                &mut table_access_count,
                "\t",
            )?;
        }

        lower_terminator_goto(
            out,
            program,
            &block.terminator,
            "\t",
            Some(HopContext {
                initialized_vars: &initialized_vars,
                vars_to_write_back: &vars_to_write_back,
                tables_written: &tables_written,
            }),
            ctx,
        )?;
    }

    writeln!(out, "}}")?;
    writeln!(out)?;

    Ok(())
}

/// Check if an optimized op contains a table access for partition calculation
/// For partition mode: check if op is a table access and generate partition calculation + unreachable code
/// Returns Some(partition_expr) if this is a table access, None otherwise
fn check_partition_from_op(
    out: &mut String,
    program: &cfg::Program,
    op: &OptimizedOp,
    indent: &str,
    _initialized_vars: &mut HashSet<cfg::VariableId>,
    _table_access_count: &mut usize,
) -> Result<Option<String>, std::fmt::Error> {
    match op {
        OptimizedOp::Single(inst) => {
            // Check if it's a table access
            match &inst.kind {
                InstructionKind::TableGet { table, keys, .. }
                | InstructionKind::TableSet { table, keys, .. } => {
                    let table_info = &program.tables[*table];
                    let table_name = pascal_case(&table_info.name);

                    writeln!(
                        out,
                        "{}// First table access - calculate partition: {}",
                        indent, table_name
                    )?;

                    let key_args: Vec<String> = table_info
                        .primary_key_fields
                        .iter()
                        .zip(keys.iter())
                        .map(|(field_id, operand)| {
                            let field = &program.table_fields[*field_id];
                            let value = operand_to_go(program, operand);
                            format!("{}: {}", field.name, value)
                        })
                        .collect();

                    let call = if matches!(&inst.kind, InstructionKind::TableGet { .. }) {
                        format!(
                            "get{}Par({}Key{{{}}})",
                            table_name,
                            table_name,
                            key_args.join(", ")
                        )
                    } else {
                        format!(
                            "put{}Par({}Key{{{}}})",
                            table_name,
                            table_name,
                            key_args.join(", ")
                        )
                    };

                    Ok(Some(call))
                }
                _ => Ok(None),
            }
        }
        OptimizedOp::CombinedTableAccess(group) => {
            // Combined access - use the first access for partition
            let table_info = &program.tables[group.table_id];
            let table_name = pascal_case(&table_info.name);

            writeln!(
                out,
                "{}// First table access (optimized group) - calculate partition: {}",
                indent, table_name
            )?;

            let key_args: Vec<String> = table_info
                .primary_key_fields
                .iter()
                .zip(group.keys.iter())
                .map(|(field_id, operand)| {
                    let field = &program.table_fields[*field_id];
                    let value = operand_to_go(program, operand);
                    format!("{}: {}", field.name, value)
                })
                .collect();

            // Use getPar for reads, putPar for writes (both return same partition)
            let call = if group.has_writes {
                format!(
                    "put{}Par({}Key{{{}}})",
                    table_name,
                    table_name,
                    key_args.join(", ")
                )
            } else {
                format!(
                    "get{}Par({}Key{{{}}})",
                    table_name,
                    table_name,
                    key_args.join(", ")
                )
            };

            Ok(Some(call))
        }
    }
}

fn generate_next_req(
    out: &mut String,
    program: &cfg::Program,
    function_id: cfg::FunctionId,
) -> Result<(), std::fmt::Error> {
    let function = &program.functions[function_id];

    let func_name = pascal_case(&function.name);
    writeln!(
        out,
        "func {}NextReq(in *proto.TrxRes, params map[string]string, shards int) (*proto.TrxReq, map[string]string, int) {{",
        func_name
    )?;

    writeln!(out, "\thopId := in.Info.Hopid")?;
    writeln!(out, "\treq := &proto.TrxReq{{Info: in.Info}}")?;
    writeln!(out, "\tnextShard := 0")?;
    writeln!(out)?;

    // Run function-level liveness analysis to determine what variables need to be passed between hops
    let func_liveness = analyze_live_variables(function, program);

    // Run hop-level reaching definitions for each hop to determine which variables are defined
    let hop_reaching_def = analyze_reaching_definitions_hop(function, program);

    writeln!(out, "\tswitch hopId {{")?;

    // Generate cases for each hop
    for (hop_idx, _hop_id) in function.hops.iter().enumerate() {
        writeln!(out, "\tcase {}:", hop_idx)?;

        // Extract variables from the PREVIOUS hop results if this is not hop 0
        if hop_idx > 0 {
            if let Some(&prev_hop_id) = function.hops.get(hop_idx - 1) {
                let prev_hop = &program.hops[prev_hop_id];

                // Get live-in variables for THIS hop at function level
                if let Some(&this_hop_id) = function.hops.get(hop_idx) {
                    let this_hop = &program.hops[this_hop_id];
                    if let Some(this_entry) = this_hop.entry_block {
                        if let Some(live_in_lattice) = func_liveness.block_entry.get(&this_entry) {
                            if let Some(live_in_set) = live_in_lattice.as_set() {
                                // Get variables defined in previous hop
                                let mut vars_defined_in_prev_hop = HashSet::new();
                                for &block_id in &prev_hop.blocks {
                                    if let Some(exit_state) =
                                        hop_reaching_def.block_exit.get(&block_id)
                                    {
                                        if let Some(defs) = exit_state.as_set() {
                                            for def in defs {
                                                vars_defined_in_prev_hop.insert(def.var);
                                            }
                                        }
                                    }
                                }

                                // Variables to extract: live-in for this hop AND defined in previous hop
                                let mut vars_to_extract: Vec<_> = live_in_set
                                    .iter()
                                    .map(|LiveVar(v)| *v)
                                    .filter(|var_id| vars_defined_in_prev_hop.contains(var_id))
                                    .collect();
                                vars_to_extract.sort_by_key(|var_id| var_id.index());

                                if !vars_to_extract.is_empty() {
                                    writeln!(
                                        out,
                                        "\t\t// Extract variables from previous hop results"
                                    )?;
                                    for (idx, var_id) in vars_to_extract.iter().enumerate() {
                                        let var_name = go_var_name(program, *var_id);
                                        writeln!(
                                            out,
                                            "\t\tparams[\"{}\"] = in.Results[{}]",
                                            var_name, idx
                                        )?;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Check if this hop is global
        if let Some(&this_hop_id) = function.hops.get(hop_idx) {
            let this_hop = &program.hops[this_hop_id];
            let is_global = this_hop.decorators.iter().any(|d| d.name == "global");

            if is_global {
                // For global hops, no partition calculation needed
                writeln!(out, "\t\t// Global hop - no partition calculation needed")?;
                writeln!(out, "\t\tbreak")?;
            } else {
                // Calculate partition for the NEXT hop (hop_idx) for non-global hops
                writeln!(out, "\t\t// Deep copy params to avoid modification")?;
                writeln!(out, "\t\tparamsCopy := make(map[string]string)")?;
                writeln!(out, "\t\tfor k, v := range params {{")?;
                writeln!(out, "\t\t\tparamsCopy[k] = v")?;
                writeln!(out, "\t\t}}")?;
                writeln!(
                    out,
                    "\t\t// Calculate partition for next hop using partition function"
                )?;
                writeln!(
                    out,
                    "\t\tnextShard = int({}Hop{}Par(paramsCopy))",
                    func_name, hop_idx
                )?;
            }
        }
    }

    writeln!(out, "\tdefault:")?;
    writeln!(out, "\t\treturn nil, params, 0")?;
    writeln!(out, "\t}}")?;
    writeln!(out)?;

    writeln!(out, "\treq.Params = params")?;
    writeln!(out, "\treq.Info.Hopid += 1")?;
    writeln!(out, "\treq.Dependency = in.Dependency")?;
    writeln!(out, "\treturn req, params, nextShard")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    Ok(())
}

fn generate_chain_impl(
    out: &mut String,
    program: &cfg::Program,
    _sc_graph: &SCGraph,
    function_id: cfg::FunctionId,
    hop_types: &HashMap<(cfg::FunctionId, cfg::HopId), HopType>,
    conflicts: &HashMap<(cfg::FunctionId, cfg::HopId), HashMap<i32, i32>>,
) -> Result<(), std::fmt::Error> {
    let function = &program.functions[function_id];
    let func_name = pascal_case(&function.name);

    writeln!(out, "func {}ChainImpl(db *bolt.DB) *Chain {{", func_name)?;
    writeln!(out, "\t// Create the list of hops")?;
    writeln!(out, "\thops := make([]*Hop, {})", function.hops.len())?;
    writeln!(out)?;

    // Run table mod/ref analysis once for the entire function
    let table_analysis = analyze_table_mod_ref(function, program);

    // For each hop, determine its type
    for (hop_index, &hop_id) in function.hops.iter().enumerate() {
        // Determine if hop is read-only using dataflow analysis
        let hop = &program.hops[hop_id];
        let mut is_read_only = true;

        // Check all blocks in the hop for write accesses
        for &block_id in &hop.blocks {
            if let Some(exit_state) = table_analysis.block_exit.get(&block_id) {
                if let Some(accesses) = exit_state.as_set() {
                    for access in accesses {
                        if access.access_type == AccessType::Write {
                            is_read_only = false;
                            break;
                        }
                    }
                }
            }
            if !is_read_only {
                break;
            }
        }

        // Get hop type from the computed map
        // Check if hop has @global decorator
        let is_global = hop.decorators.iter().any(|d| d.name == "global");

        let hop_type_enum = hop_types
            .get(&(function_id, hop_id))
            .copied()
            .unwrap_or(HopType::NormalHop);
        let hop_type = if is_global {
            "util.GlobalHop"
        } else {
            match hop_type_enum {
                HopType::NormalHop => "util.NormalHop",
                HopType::MergedHopBegin => "util.MergedHopBegin",
                HopType::MergedHop => "util.MergedHop",
                HopType::MergedHopEnd => "util.MergedHopEnd",
            }
        };

        // Process function name
        let process_func = format!("{}Hop{}", func_name, hop_index);

        // Get conflicts map for this hop
        let conflicts_map = if let Some(hop_conflicts) = conflicts.get(&(function_id, hop_id)) {
            if hop_conflicts.is_empty() {
                "map[int32]int32{}".to_string()
            } else {
                // Build the conflicts map string
                let mut parts: Vec<String> = hop_conflicts
                    .iter()
                    .map(|(chain_idx, hop_idx)| format!("{}: {}", chain_idx, hop_idx))
                    .collect();
                parts.sort(); // Sort for consistent output
                format!("map[int32]int32{{{}}}", parts.join(", "))
            }
        } else {
            "map[int32]int32{}".to_string()
        };

        writeln!(out, "\thops[{}] = &Hop{{", hop_index)?;
        writeln!(out, "\t\tid:         {},", hop_index)?;
        writeln!(out, "\t\thopType:    {},", hop_type)?;
        writeln!(out, "\t\tisReadOnly: {},", is_read_only)?;
        writeln!(out, "\t\tprocess:    {},", process_func)?;

        // Add aggregate function for global hops
        if is_global {
            let aggregate_func = format!("{}Hop{}Aggregate", func_name, hop_index);
            writeln!(out, "\t\taggregate:  {},", aggregate_func)?;
        }

        writeln!(out, "\t\tconflicts:  {},", conflicts_map)?;
        writeln!(out, "\t}}")?;
        writeln!(out)?;
    }

    writeln!(out, "\t// add field names")?;
    writeln!(
        out,
        "\treturn &Chain{{db: db, hops: hops, NextReq: {}NextReq}}",
        func_name
    )?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    Ok(())
}

/// Use json package to eliminate unused import errors
fn generate_to_json(
    out: &mut String,
    program: &cfg::Program,
    function_id: cfg::FunctionId,
) -> Result<(), std::fmt::Error> {
    let function = &program.functions[function_id];
    let func_name = pascal_case(&function.name);

    writeln!(
        out,
        "// legacy: use json package to avoid unused import error"
    )?;
    writeln!(
        out,
        "func {}ToJSON(v interface{{}}) ([]byte, error) {{",
        func_name
    )?;
    writeln!(out, "\treturn json.Marshal(v)")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    Ok(())
}
