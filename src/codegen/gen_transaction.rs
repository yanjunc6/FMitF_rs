use super::{
    gen_table_optimizer::{group_table_accesses, create_instruction_to_group_map, TableAccess, TableAccessGroup},
    util::{go_type_string, go_var_name, pascal_case, snake_case, write_go_file_header},
    GoProgram,
};
use crate::cfg::{
    self, BinaryOp, FunctionKind, Instruction, InstructionKind, Operand, Terminator,
    UnaryOp,
};
use crate::dataflow::{analyze_live_variables, analyze_table_mod_ref, AccessType, LiveVar};
use crate::sc_graph::{calculate_conflicts, determine_hop_types, CombinedSCGraph, HopType, SCGraph};
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
                "fmt",
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

        // Generate NextReq function
        generate_next_req(&mut content, program, function_id)?;

        // Generate ChainImpl function
        generate_chain_impl(&mut content, program, sc_graph, function_id, &hop_types, &conflicts)?;

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
    let function = &program.functions[function_id];
    let hop = &program.hops[hop_id];
    let hop_name = format!("{}Hop{}", pascal_case(&function.name), hop_index);

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

    // Run liveness analysis on the entire function
    let liveness = analyze_live_variables(function, program);

    // Get live-in variables for this hop's entry block
    let entry_block = hop.entry_block.expect("Hop must have entry block");
    let mut live_in_set = liveness
        .block_entry
        .get(&entry_block)
        .and_then(|lattice| lattice.as_set())
        .map(|set| set.iter().map(|LiveVar(v)| *v).collect::<Vec<_>>())
        .unwrap_or_default();
    live_in_set.sort_by_key(|var_id| var_id.index());

    // Get live-in variables for the next hop (these are the live-out of current hop)
    let mut next_hop_live_in = if let Some(next_id) = next_hop_id {
        let next_hop = &program.hops[next_id];
        if let Some(next_entry) = next_hop.entry_block {
            liveness
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
    };
    next_hop_live_in.sort_by_key(|var_id| var_id.index());

    // Collect all variables that need declaration
    let mut var_decls: HashMap<cfg::VariableId, String> = HashMap::new();
    let mut initialized_vars: HashSet<cfg::VariableId> = HashSet::new();

    // Declare live-in variables
    for var_id in &live_in_set {
        let var = &program.variables[*var_id];
        let var_type = &program.types[var.ty];

        let decl_type = match var_type {
            cfg::Type::Primitive(cfg::PrimitiveType::Int)
            | cfg::Type::Primitive(cfg::PrimitiveType::Float)
            | cfg::Type::Primitive(cfg::PrimitiveType::Bool)
            | cfg::Type::Primitive(cfg::PrimitiveType::String)
            | cfg::Type::List(_) => go_type_string(program, var.ty),
            _ => String::from("interface{}"),
        };
        var_decls.insert(*var_id, decl_type);
    }

    // Collect variables from all instructions in the hop
    for &block_id in &hop.blocks {
        let block = &program.basic_blocks[block_id];
        for inst in &block.instructions {
            collect_var_decls_from_instruction(program, inst, &mut var_decls);
        }
    }

    // Group table accesses across ALL blocks to determine what we'll actually generate
    let mut all_groups = Vec::new();
    let mut table_var_count = 0;
    for &block_id in &hop.blocks {
        let block = &program.basic_blocks[block_id];
        let groups = group_table_accesses(&block.instructions);
        
        // Count how many table access variables we'll actually need for this block
        for group in &groups {
            if group.is_worth_combining() && group.all_field_accesses() {
                table_var_count += 1; // One get/put pair for the optimized group
            } else {
                table_var_count += group.accesses.len(); // Individual accesses
            }
        }
        
        all_groups.push((block_id, groups));
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
        // We'll declare variables dynamically as needed, but we need to know the table types
        // For now, collect all unique table types that will be accessed
        let mut table_types_needed: Vec<String> = Vec::new();
        for (_, groups) in &all_groups {
            for group in groups {
                if group.is_worth_combining() && group.all_field_accesses() {
                    let table_info = &program.tables[group.table_id];
                    table_types_needed.push(pascal_case(&table_info.name));
                } else {
                    for _ in &group.accesses {
                        let table_info = &program.tables[group.table_id];
                        table_types_needed.push(pascal_case(&table_info.name));
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
            cfg::Type::List(_) => {
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
    
    // Find the groups for the entry block
    let (_, entry_groups) = all_groups.iter().find(|(bid, _)| *bid == entry_block).unwrap();
    let inst_to_group = create_instruction_to_group_map(entry_groups);
    
    lower_instructions_with_optimization(
        out,
        program,
        &entry_block_obj.instructions,
        entry_groups,
        &inst_to_group,
        &mut initialized_vars,
        &mut table_access_count,
        "\t",
    )?;
    
    lower_terminator_goto(
        out,
        program,
        &entry_block_obj.terminator,
        "\t",
        &initialized_vars,
        &next_hop_live_in,
    )?;

    // Generate code for other blocks in the hop (only with label if used)
    for &block_id in &hop.blocks {
        if block_id == entry_block {
            continue; // Already generated
        }

        let block = &program.basic_blocks[block_id];

        // Only generate label if it's used
        if used_labels.contains(&block_id) {
            writeln!(out, "BB{}:", block_id.index())?;
        }

        // Find the groups for this block
        let (_, block_groups) = all_groups.iter().find(|(bid, _)| *bid == block_id).unwrap();
        let inst_to_group = create_instruction_to_group_map(block_groups);
        
        lower_instructions_with_optimization(
            out,
            program,
            &block.instructions,
            block_groups,
            &inst_to_group,
            &mut initialized_vars,
            &mut table_access_count,
            "\t",
        )?;

        lower_terminator_goto(
            out,
            program,
            &block.terminator,
            "\t",
            &initialized_vars,
            &next_hop_live_in,
        )?;
    }

    writeln!(out, "}}")?;
    writeln!(out)?;

    Ok(())
}

// Generate partition calculation hop function
fn generate_partition_hop_function(
    out: &mut String,
    program: &cfg::Program,
    function_id: cfg::FunctionId,
    hop_index: usize,
    hop_id: cfg::HopId,
) -> Result<(), std::fmt::Error> {
    let function = &program.functions[function_id];
    let hop = &program.hops[hop_id];
    let hop_name = format!("{}Hop{}Par", pascal_case(&function.name), hop_index);

    writeln!(
        out,
        "// {} calculates the partition for hop {} without database access.",
        hop_name, hop_index
    )?;

    // Function signature - takes a map of parameters, returns uint64 shard
    writeln!(out, "func {}(params map[string]string) uint64 {{", hop_name)?;

    // Run liveness analysis
    let liveness = analyze_live_variables(function, program);

    // Get live-in variables for this hop's entry block
    let entry_block = hop.entry_block.expect("Hop must have entry block");
    let mut live_in_set = liveness
        .block_entry
        .get(&entry_block)
        .and_then(|lattice| lattice.as_set())
        .map(|set| set.iter().map(|LiveVar(v)| *v).collect::<Vec<_>>())
        .unwrap_or_default();
    live_in_set.sort_by_key(|var_id| var_id.index());

    // Collect all variables that need declaration
    let mut var_decls: HashMap<cfg::VariableId, String> = HashMap::new();

    // Declare live-in variables
    for var_id in &live_in_set {
        let var = &program.variables[*var_id];
        let var_type = &program.types[var.ty];

        let decl_type = match var_type {
            cfg::Type::Primitive(cfg::PrimitiveType::Int)
            | cfg::Type::Primitive(cfg::PrimitiveType::Float)
            | cfg::Type::Primitive(cfg::PrimitiveType::Bool)
            | cfg::Type::Primitive(cfg::PrimitiveType::String)
            | cfg::Type::List(_) => go_type_string(program, var.ty),
            _ => String::from("interface{}"),
        };
        var_decls.insert(*var_id, decl_type);
    }

    // Collect variables from all instructions in the hop
    for &block_id in &hop.blocks {
        let block = &program.basic_blocks[block_id];
        for inst in &block.instructions {
            collect_var_decls_from_instruction(program, inst, &mut var_decls);
        }
    }

    // Count table accesses to pre-declare keyBytes and row variables
    let mut table_access_types_par: Vec<String> = Vec::new();
    for &block_id in &hop.blocks {
        let block = &program.basic_blocks[block_id];
        for inst in &block.instructions {
            match &inst.kind {
                InstructionKind::TableGet { table, .. }
                | InstructionKind::TableSet { table, .. } => {
                    let table_info = &program.tables[*table];
                    let table_name = pascal_case(&table_info.name);
                    table_access_types_par.push(table_name);
                }
                _ => {}
            }
        }
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

    // Declare keyBytes and row variables for table accesses (needed for unreachable code)
    if !table_access_types_par.is_empty() {
        for (i, table_name) in table_access_types_par.iter().enumerate() {
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
            cfg::Type::List(_) => {
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
    }

    if !live_in_set.is_empty() {
        writeln!(out)?;
    }

    // Track which labels are actually used
    let mut used_labels = HashSet::new();
    for &block_id in &hop.blocks {
        let block = &program.basic_blocks[block_id];
        collect_used_labels(&block.terminator, &mut used_labels);
    }

    // Create fake variables for unreachable code (will never be used since we return early)
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

    // Add initial goto to entry block to ensure all labels are reachable
    writeln!(out, "\tgoto BB{}", entry_block.index())?;
    writeln!(out)?;

    let mut initialized_vars: HashSet<cfg::VariableId> = HashSet::new();
    let mut table_access_count = 0usize;

    // Mark live-in variables as initialized
    for var_id in &live_in_set {
        initialized_vars.insert(*var_id);
    }

    // Generate code for entry block with label
    writeln!(out, "BB{}:", entry_block.index())?;
    let entry_block_obj = &program.basic_blocks[entry_block];
    
    // For partition functions, we iterate through instructions normally
    // The first table access determines the partition, rest is unreachable code
    for inst in &entry_block_obj.instructions {
        if let Some(shard) = lower_instruction_partition(
            out,
            program,
            inst,
            "\t",
            &mut initialized_vars,
            &mut table_access_count,
        )? {
            // First table access found - return the shard
            writeln!(out, "\tif true {{ return {} }}", shard)?;
        }
        // Continue generating unreachable code to use variables (prevents Go compiler errors)
        lower_single_instruction(
            out,
            program,
            inst,
            &mut initialized_vars,
            &mut table_access_count,
            "\t",
        )?;
    }
    lower_terminator_partition_goto(out, program, &entry_block_obj.terminator, "\t")?;

    // Generate code for other blocks in the hop
    for &block_id in &hop.blocks {
        if block_id == entry_block {
            continue;
        }

        let block = &program.basic_blocks[block_id];

        if used_labels.contains(&block_id) {
            writeln!(out, "BB{}:", block_id.index())?;
        }

        // For partition functions, iterate through instructions normally
        for inst in &block.instructions {
            if let Some(shard) = lower_instruction_partition(
                out,
                program,
                inst,
                "\t",
                &mut initialized_vars,
                &mut table_access_count,
            )? {
                writeln!(out, "\tif true {{ return {} }}", shard)?;
            }
            // Continue generating unreachable code to use variables
            lower_single_instruction(
                out,
                program,
                inst,
                &mut initialized_vars,
                &mut table_access_count,
                "\t",
            )?;
        }

        lower_terminator_partition_goto(out, program, &block.terminator, "\t")?;
    }

    // Fallback return
    // writeln!(out, "\treturn 0 // Default shard if no table access found")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    Ok(())
}

// Helper function to collect variable declarations from an instruction
fn collect_var_decls_from_instruction(
    program: &cfg::Program,
    inst: &Instruction,
    var_decls: &mut HashMap<cfg::VariableId, String>,
) {
    match &inst.kind {
        InstructionKind::Assign { dest, .. }
        | InstructionKind::BinaryOp { dest, .. }
        | InstructionKind::UnaryOp { dest, .. }
        | InstructionKind::TableGet { dest, .. } => {
            let ty = go_type_string(program, program.variables[*dest].ty);
            var_decls.insert(*dest, ty);
        }
        InstructionKind::Call {
            dest: Some(dest), ..
        } => {
            let ty = go_type_string(program, program.variables[*dest].ty);
            var_decls.insert(*dest, ty);
        }
        _ => {}
    }
}

// Helper function to collect labels that are actually used
fn collect_used_labels(term: &Terminator, used_labels: &mut HashSet<cfg::BasicBlockId>) {
    match term {
        Terminator::Jump(target) => {
            used_labels.insert(*target);
        }
        Terminator::Branch {
            if_true, if_false, ..
        } => {
            used_labels.insert(*if_true);
            used_labels.insert(*if_false);
        }
        _ => {}
    }
}

/// Internal instruction lowering implementation
/// This should not be called directly - use lower_single_instruction instead
fn lower_instruction_impl(
    out: &mut String,
    program: &cfg::Program,
    inst: &Instruction,
    initialized_vars: &mut HashSet<cfg::VariableId>,
    table_access_count: &mut usize,
    indent: &str,
) -> Result<(), std::fmt::Error> {
    match &inst.kind {
        InstructionKind::Assign { dest, src } => {
            let dest_name = go_var_name(program, *dest);
            let src_go = operand_to_go(program, src);

            writeln!(out, "{}{} = {}", indent, dest_name, src_go)?;
            initialized_vars.insert(*dest);
        }

        InstructionKind::BinaryOp {
            dest,
            op,
            left,
            right,
        } => {
            let dest_name = go_var_name(program, *dest);
            let left_go = operand_to_go(program, left);
            let right_go = operand_to_go(program, right);
            let op_str = binary_op_to_go(*op);

            writeln!(
                out,
                "{}{} = {} {} {}",
                indent, dest_name, left_go, op_str, right_go
            )?;
            initialized_vars.insert(*dest);
        }

        InstructionKind::UnaryOp { dest, op, operand } => {
            let dest_name = go_var_name(program, *dest);
            let operand_go = operand_to_go(program, operand);
            let op_str = unary_op_to_go(*op);

            writeln!(out, "{}{} = {}{}", indent, dest_name, op_str, operand_go)?;
            initialized_vars.insert(*dest);
        }

        InstructionKind::TableGet {
            dest,
            table,
            keys,
            field,
        } => {
            let table_info = &program.tables[*table];
            let table_name = pascal_case(&table_info.name);
            let dest_name = go_var_name(program, *dest);

            // Build key struct
            writeln!(out, "{}// TableGet: {}", indent, table_name)?;
            let key_args: Vec<String> = table_info
                .primary_key_fields
                .iter()
                .zip(keys.iter())
                .map(|(field_id, key_operand)| {
                    let field = &program.table_fields[*field_id];
                    format!("{}: {}", field.name, operand_to_go(program, key_operand))
                })
                .collect();

            // Generate unique variable names for each table access
            *table_access_count += 1;
            let call = format!(
                "get{}(tx, {}Key{{ {} }})",
                table_name,
                table_name,
                key_args.join(", ")
            );

            if let Some(field_id) = field {
                // Getting specific field
                let row_var = format!("row{}", table_access_count);
                let key_var = format!("keyBytes{}", table_access_count);
                writeln!(out, "{}{}, {} = {}", indent, key_var, row_var, call)?;
                // Add to rwSet
                writeln!(
                    out,
                    "{}rwSet = AddRWSet(rwSet, \"{}\", {})",
                    indent, table_name, key_var
                )?;

                let field_access = table_field_accessor(program, table_info, *field_id, &row_var);
                writeln!(out, "{}{} = {}", indent, dest_name, field_access)?;
                initialized_vars.insert(*dest);
            } else {
                // Getting entire row
                let key_var = format!("keyBytes{}", table_access_count);
                writeln!(out, "{}{}, {} = {}", indent, key_var, dest_name, call)?;
                // Add to rwSet
                writeln!(
                    out,
                    "{}rwSet = AddRWSet(rwSet, \"{}\", {})",
                    indent, table_name, key_var
                )?;
                initialized_vars.insert(*dest);
            }
        }

        InstructionKind::TableSet {
            table,
            keys,
            field,
            value,
        } => {
            let table_info = &program.tables[*table];
            let table_name = pascal_case(&table_info.name);

            writeln!(out, "{}// TableSet: {}", indent, table_name)?;
            let key_args: Vec<String> = table_info
                .primary_key_fields
                .iter()
                .zip(keys.iter())
                .map(|(field_id, key_operand)| {
                    let field = &program.table_fields[*field_id];
                    format!("{}: {}", field.name, operand_to_go(program, key_operand))
                })
                .collect();

            // Generate unique variable names for each table access
            *table_access_count += 1;
            let key_var = format!("keyBytes{}", table_access_count);
            let row_var = format!("row{}", table_access_count);

            // First get the row
            writeln!(
                out,
                "{}{}, {} = get{}(tx, {}Key{{ {} }})",
                indent,
                key_var,
                row_var,
                table_name,
                table_name,
                key_args.join(", ")
            )?;

            // Add to rwSet
            writeln!(
                out,
                "{}rwSet = AddRWSet(rwSet, \"{}\", {})",
                indent, table_name, key_var
            )?;

            if let Some(field_id) = field {
                // Setting specific field
                let field_target = table_field_accessor(program, table_info, *field_id, &row_var);
                writeln!(
                    out,
                    "{}{} = {}",
                    indent,
                    field_target,
                    operand_to_go(program, value)
                )?;
            }

            // Write back
            writeln!(
                out,
                "{}putData(tx.Bucket([]byte(\"t{}\")), {}, mustJSON({}))",
                indent, table_name, key_var, row_var
            )?;
        }

        InstructionKind::Assert { condition, message } => {
            let cond_go = operand_to_go(program, condition);
            writeln!(out, "{}if !({}) {{", indent, cond_go)?;
            writeln!(
                out,
                "{}\tpanic(\"{}\")",
                indent,
                message.replace('"', "\\\"")
            )?;
            writeln!(out, "{}}}", indent)?;
        }

        InstructionKind::Call { dest, func, args } => {
            let func_info = &program.functions[*func];
            let func_name = func_info.name.as_str();
            let has_go_decorator = func_info
                .decorators
                .iter()
                .any(|decorator| decorator.name == "go");
            let go_name = go_function_name(program, *func);

            let call_expr = if has_go_decorator && func_name == "+" {
                let parts: Vec<String> = args
                    .iter()
                    .map(|operand| operand_to_string(program, operand))
                    .collect();
                if parts.is_empty() {
                    format!("{}()", go_name)
                } else {
                    format!("{}({})", go_name, parts.join(", "))
                }
            } else if has_go_decorator && func_name == "get" {
                assert!(args.len() == 2, "list get expects list and index arguments");
                let list_expr = operand_to_go(program, &args[0]);
                let index_expr = operand_to_go(program, &args[1]);
                format!("{}({}, int({}))", go_name, list_expr, index_expr)
            } else if func_name == "str" {
                args.first()
                    .map(|operand| operand_to_string(program, operand))
                    .unwrap_or_else(|| "\"\"".to_string())
            } else {
                let arg_exprs: Vec<String> =
                    args.iter().map(|arg| operand_to_go(program, arg)).collect();
                if arg_exprs.is_empty() {
                    format!("{}()", go_name)
                } else {
                    format!("{}({})", go_name, arg_exprs.join(", "))
                }
            };

            if let Some(dest_var) = dest {
                let dest_name = go_var_name(program, *dest_var);
                writeln!(out, "{}{} = {}", indent, dest_name, call_expr)?;
                initialized_vars.insert(*dest_var);
            } else if func_name == "str" {
                writeln!(out, "{}_ = {}", indent, call_expr)?;
            } else {
                writeln!(out, "{}{}", indent, call_expr)?;
            }
        }
    }

    Ok(())
}

/// Lower a sequence of instructions with table access optimization
fn lower_instructions_with_optimization(
    out: &mut String,
    program: &cfg::Program,
    instructions: &[Instruction],
    groups: &[TableAccessGroup],
    inst_to_group: &HashMap<usize, (usize, usize)>,
    initialized_vars: &mut HashSet<cfg::VariableId>,
    table_access_count: &mut usize,
    indent: &str,
) -> Result<(), std::fmt::Error> {
    let mut processed_groups: HashSet<usize> = HashSet::new();
    
    for (inst_idx, inst) in instructions.iter().enumerate() {
        // Check if this instruction is part of a group
        if let Some(&(group_idx, access_idx_in_group)) = inst_to_group.get(&inst_idx) {
            // Only process the group once (at the first access)
            if access_idx_in_group == 0 && !processed_groups.contains(&group_idx) {
                processed_groups.insert(group_idx);
                let group = &groups[group_idx];
                
                // Only optimize if worth combining and all are field accesses
                if group.is_worth_combining() && group.all_field_accesses() {
                    lower_table_access_group(
                        out,
                        program,
                        group,
                        initialized_vars,
                        table_access_count,
                        indent,
                    )?;
                } else {
                    // Not worth optimizing, but still part of a group - process the single instruction
                    lower_single_instruction(
                        out,
                        program,
                        inst,
                        initialized_vars,
                        table_access_count,
                        indent,
                    )?;
                }
            } else if processed_groups.contains(&group_idx) {
                // This instruction was already handled as part of an optimized group, skip it
                continue;
            } else {
                // Part of a group but not the first access - shouldn't happen with proper grouping
                // Process individually as fallback
                lower_single_instruction(
                    out,
                    program,
                    inst,
                    initialized_vars,
                    table_access_count,
                    indent,
                )?;
            }
        } else {
            // Not part of any group, process normally
            lower_single_instruction(
                out,
                program,
                inst,
                initialized_vars,
                table_access_count,
                indent,
            )?;
        }
    }
    
    Ok(())
}

/// Lower a single instruction (wrapper around internal implementation)
/// Public for use by other codegen modules like gen_global
pub(super) fn lower_single_instruction(
    out: &mut String,
    program: &cfg::Program,
    inst: &Instruction,
    initialized_vars: &mut HashSet<cfg::VariableId>,
    table_access_count: &mut usize,
    indent: &str,
) -> Result<(), std::fmt::Error> {
    // Calls the internal implementation
    lower_instruction_impl(out, program, inst, initialized_vars, table_access_count, indent)
}

/// Lower a grouped set of table accesses into a single get/put operation
fn lower_table_access_group(
    out: &mut String,
    program: &cfg::Program,
    group: &TableAccessGroup,
    initialized_vars: &mut HashSet<cfg::VariableId>,
    table_access_count: &mut usize,
    indent: &str,
) -> Result<(), std::fmt::Error> {
    let table_info = &program.tables[group.table_id];
    let table_name = pascal_case(&table_info.name);
    
    writeln!(out, "{}// Optimized combined table access: {}", indent, table_name)?;
    
    // Build key struct arguments
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
    
    // Generate unique variable names
    *table_access_count += 1;
    let key_var = format!("keyBytes{}", table_access_count);
    let row_var = format!("row{}", table_access_count);
    
    // Get the whole row
    writeln!(
        out,
        "{}{}, {} = get{}(tx, {}Key{{{}}})",
        indent, key_var, row_var, table_name, table_name,
        key_args.join(", ")
    )?;
    
    // Add to rwSet
    writeln!(
        out,
        "{}rwSet = AddRWSet(rwSet, \"{}\", {})",
        indent, table_name, key_var
    )?;
    
    // Process all accesses in the group
    for access in &group.accesses {
        match access {
            TableAccess::Get { dest, field, .. } => {
                if let Some(field_id) = field {
                    let dest_name = go_var_name(program, *dest);
                    let accessor = table_field_accessor(program, table_info, *field_id, &row_var);
                    writeln!(out, "{}{} = {}", indent, dest_name, accessor)?;
                    initialized_vars.insert(*dest);
                }
            }
            TableAccess::Set { field, value, .. } => {
                if let Some(field_id) = field {
                    let accessor = table_field_accessor(program, table_info, *field_id, &row_var);
                    let value_go = operand_to_go(program, value);
                    writeln!(out, "{}{} = {}", indent, accessor, value_go)?;
                }
            }
        }
    }
    
    // If there were any writes, write the row back
    if group.has_writes {
        writeln!(
            out,
            "{}put{}(tx, {}Key{{{}}}, {})",
            indent, table_name, table_name, key_args.join(", "), row_var
        )?;
    }
    
    Ok(())
}

pub(super) fn lower_terminator_goto(
    out: &mut String,
    program: &cfg::Program,
    term: &Terminator,
    indent: &str,
    initialized_vars: &HashSet<cfg::VariableId>,
    next_hop_live_in: &[cfg::VariableId],
) -> Result<(), std::fmt::Error> {
    match term {
        Terminator::Jump(target) => {
            writeln!(out, "{}goto BB{}", indent, target.index())?;
        }

        Terminator::Branch {
            condition,
            if_true,
            if_false,
        } => {
            let cond_go = operand_to_go(program, condition);
            writeln!(out, "{}if {} {{", indent, cond_go)?;
            writeln!(out, "{}\tgoto BB{}", indent, if_true.index())?;
            writeln!(out, "{}}} else {{", indent)?;
            writeln!(out, "{}\tgoto BB{}", indent, if_false.index())?;
            writeln!(out, "{}}}", indent)?;
        }

        Terminator::Return(value) => {
            if let Some(val) = value {
                let result_expr = operand_to_string(program, val);
                writeln!(out, "{}return &proto.TrxRes{{", indent)?;
                writeln!(out, "{}\tStatus: proto.Status_Success,", indent)?;
                writeln!(out, "{}\tInfo:   in.Info,", indent)?;
                writeln!(out, "{}\tResults: []string{{{}}},", indent, result_expr)?;
                writeln!(out, "{}\tRWSets: rwSet,", indent)?;
                writeln!(out, "{}}}, nil", indent)?;
            } else {
                writeln!(out, "{}return &proto.TrxRes{{", indent)?;
                writeln!(out, "{}\tStatus: proto.Status_Success,", indent)?;
                writeln!(out, "{}\tInfo:   in.Info,", indent)?;
                writeln!(out, "{}\tRWSets: rwSet,", indent)?;
                writeln!(out, "{}}}, nil", indent)?;
            }
        }

        Terminator::HopExit { .. } => {
            // Collect variables that are live-in at the next hop (these are live-out of current hop)
            if !next_hop_live_in.is_empty() {
                writeln!(out, "{}// Collect live-out variables to results", indent)?;
                let mut results = Vec::new();
                for var_id in next_hop_live_in {
                    let var_name = go_var_name(program, *var_id);
                    let expr = if initialized_vars.contains(var_id) {
                        go_var_to_string(program, *var_id)
                    } else {
                        format!("params[\"{}\"]", var_name)
                    };
                    results.push(expr);
                }
                writeln!(out, "{}return &proto.TrxRes{{", indent)?;
                writeln!(out, "{}\tStatus: proto.Status_Success,", indent)?;
                writeln!(out, "{}\tInfo:   in.Info,", indent)?;
                writeln!(
                    out,
                    "{}\tResults: []string{{{}}},",
                    indent,
                    results.join(", ")
                )?;
                writeln!(out, "{}\tRWSets: rwSet,", indent)?;
                writeln!(out, "{}}}, nil", indent)?;
                return Ok(());
            }
            // Fallback: no live-out variables (last hop or no inter-hop dependencies)
            writeln!(out, "{}return &proto.TrxRes{{", indent)?;
            writeln!(out, "{}\tStatus: proto.Status_Success,", indent)?;
            writeln!(out, "{}\tInfo:   in.Info,", indent)?;
            writeln!(out, "{}\tRWSets: rwSet,", indent)?;
            writeln!(out, "{}}}, nil", indent)?;
        }

        Terminator::Abort => {
            writeln!(out, "{}panic(\"transaction aborted\")", indent)?;
        }
    }

    Ok(())
}

// Lower instruction for partition calculation - returns Some(shard) if table access is found
fn lower_instruction_partition(
    out: &mut String,
    program: &cfg::Program,
    inst: &Instruction,
    indent: &str,
    initialized_vars: &mut HashSet<cfg::VariableId>,
    table_access_count: &mut usize,
) -> Result<Option<String>, std::fmt::Error> {
    // Check if this is a table access
    match &inst.kind {
        InstructionKind::TableGet { table, keys, .. }
        | InstructionKind::TableSet { table, keys, .. } => {
            // First table access found - call the partition function
            let table_info = &program.tables[*table];
            let table_name = pascal_case(&table_info.name);

            writeln!(
                out,
                "{}// First table access: {} - calculate partition",
                indent, table_name
            )?;

            let key_args: Vec<String> = table_info
                .primary_key_fields
                .iter()
                .zip(keys.iter())
                .map(|(field_id, key_operand)| {
                    let field = &program.table_fields[*field_id];
                    format!("{}: {}", field.name, operand_to_go(program, key_operand))
                })
                .collect();

            // Determine which function to call (get or put - both return partition)
            let call = if matches!(&inst.kind, InstructionKind::TableGet { .. }) {
                format!(
                    "get{}Par({}Key{{ {} }})",
                    table_name,
                    table_name,
                    key_args.join(", ")
                )
            } else {
                format!(
                    "put{}Par({}Key{{ {} }})",
                    table_name,
                    table_name,
                    key_args.join(", ")
                )
            };

            // Return the partition shard
            Ok(Some(call))
        }
        _ => {
            // For non-table-access instructions, use the normal lowering
            lower_single_instruction(
                out,
                program,
                inst,
                initialized_vars,
                table_access_count,
                indent,
            )?;
            Ok(None)
        }
    }
}

// Lower terminator for partition calculation - just reuse the normal one
fn lower_terminator_partition_goto(
    out: &mut String,
    program: &cfg::Program,
    term: &Terminator,
    indent: &str,
) -> Result<(), std::fmt::Error> {
    match term {
        Terminator::Jump(target) => {
            writeln!(out, "{}goto BB{}", indent, target.index())?;
        }

        Terminator::Branch {
            condition,
            if_true,
            if_false,
        } => {
            let cond_go = operand_to_go(program, condition);
            writeln!(out, "{}if {} {{", indent, cond_go)?;
            writeln!(out, "{}\tgoto BB{}", indent, if_true.index())?;
            writeln!(out, "{}}} else {{", indent)?;
            writeln!(out, "{}\tgoto BB{}", indent, if_false.index())?;
            writeln!(out, "{}}}", indent)?;
        }

        Terminator::Return(_) | Terminator::HopExit { .. } => {
            // For partition calculation, should not reach here
            writeln!(out, "{}return 0 // Unexpected return in partition", indent)?;
        }

        Terminator::Abort => {
            writeln!(out, "{}return 0 // Abort in partition", indent)?;
        }
    }

    Ok(())
}

pub(super) fn operand_to_go(program: &cfg::Program, operand: &Operand) -> String {
    match operand {
        Operand::Variable(var_id) => go_var_name(program, *var_id),
        Operand::Constant(const_val) => match const_val {
            cfg::ConstantValue::Int(i) => i.to_string(),
            cfg::ConstantValue::Float(f) => format!("{}", f),
            cfg::ConstantValue::Bool(b) => b.to_string(),
            cfg::ConstantValue::String(s) => format!("\"{}\"", s.replace('"', "\\\"")),
        },
        Operand::Global(global_id) => {
            let global = &program.global_consts[*global_id];
            global.name.clone()
        }
        Operand::Table(_) => "/* table */".to_string(),
    }
}

pub(super) fn binary_op_to_go(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::AddInt | BinaryOp::AddFloat => "+",
        BinaryOp::SubInt | BinaryOp::SubFloat => "-",
        BinaryOp::MulInt | BinaryOp::MulFloat => "*",
        BinaryOp::DivInt | BinaryOp::DivFloat => "/",
        BinaryOp::ModInt => "%",
        BinaryOp::EqInt | BinaryOp::EqFloat | BinaryOp::Eq => "==",
        BinaryOp::NeqInt | BinaryOp::NeqFloat | BinaryOp::Neq => "!=",
        BinaryOp::LtInt | BinaryOp::LtFloat => "<",
        BinaryOp::LeqInt | BinaryOp::LeqFloat => "<=",
        BinaryOp::GtInt | BinaryOp::GtFloat => ">",
        BinaryOp::GeqInt | BinaryOp::GeqFloat => ">=",
        BinaryOp::And => "&&",
        BinaryOp::Or => "||",
    }
}

pub(super) fn unary_op_to_go(op: UnaryOp) -> &'static str {
    match op {
        UnaryOp::NegInt | UnaryOp::NegFloat => "-",
        UnaryOp::NotBool => "!",
    }
}

fn table_field_accessor(
    program: &cfg::Program,
    table: &cfg::Table,
    field_id: cfg::FieldId,
    row_var: &str,
) -> String {
    let field_info = &program.table_fields[field_id];
    if table.primary_key_fields.contains(&field_id) {
        format!("{}.Key.{}", row_var, field_info.name)
    } else {
        format!("{}.{}", row_var, field_info.name)
    }
}

pub(super) fn go_function_name(program: &cfg::Program, func_id: cfg::FunctionId) -> String {
    let function = &program.functions[func_id];
    if function
        .decorators
        .iter()
        .any(|decorator| decorator.name == "go")
    {
        go_function_rename(&function.name)
            .unwrap_or_else(|| function.name.as_str())
            .to_string()
    } else {
        function.name.clone()
    }
}

const GO_FUNCTION_RENAMES: &[(&str, &str)] = &[
    ("length", "len"),
    ("float", "float32"),
    ("int", "uint64"),
    ("+", "concat"),
    ("get", "listGet"),
];

fn go_function_rename(name: &str) -> Option<&'static str> {
    GO_FUNCTION_RENAMES
        .iter()
        .find(|(from, _)| *from == name)
        .map(|(_, to)| *to)
}

fn go_var_to_string(program: &cfg::Program, var_id: cfg::VariableId) -> String {
    let expr = go_var_name(program, var_id);
    let ty_id = program.variables[var_id].ty;
    go_value_to_string(program, ty_id, &expr)
}

fn go_value_to_string(program: &cfg::Program, ty_id: cfg::TypeId, expr: &str) -> String {
    match &program.types[ty_id] {
        cfg::Type::Primitive(cfg::PrimitiveType::Int) => format!("fromUint64({})", expr),
        cfg::Type::Primitive(cfg::PrimitiveType::Float) => format!("fromFloat32({})", expr),
        cfg::Type::Primitive(cfg::PrimitiveType::Bool) => format!("fromBool({})", expr),
        cfg::Type::Primitive(cfg::PrimitiveType::String) => expr.to_string(),
        _ => format!("fmt.Sprint({})", expr),
    }
}

fn operand_to_string(program: &cfg::Program, operand: &Operand) -> String {
    match operand {
        Operand::Variable(var_id) => go_var_to_string(program, *var_id),
        Operand::Constant(const_val) => match const_val {
            cfg::ConstantValue::Int(i) => format!("fromUint64(uint64({}))", i),
            cfg::ConstantValue::Float(f) => format!("fromFloat32(float32({}))", f),
            cfg::ConstantValue::Bool(b) => format!("fromBool({})", b),
            cfg::ConstantValue::String(s) => go_string_literal(s),
        },
        Operand::Global(global_id) => {
            let global = &program.global_consts[*global_id];
            go_value_to_string(program, global.ty, &global.name)
        }
        Operand::Table(table_id) => {
            let table = &program.tables[*table_id];
            go_string_literal(&table.name)
        }
    }
}

fn go_string_literal(value: &str) -> String {
    format!("\"{}\"", value.escape_default())
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
        "func {}NextReq(in *proto.TrxRes, params map[string]string, history []uint32, shards int) (*proto.TrxReq, map[string]string, []uint32, int) {{",
        func_name
    )?;

    writeln!(out, "\thopId := in.Info.Hopid")?;
    writeln!(out, "\treq := &proto.TrxReq{{Info: in.Info}}")?;
    writeln!(out, "\tnextShard := 0")?;
    writeln!(out)?;

    // Run liveness analysis to determine what variables need to be passed between hops
    let liveness = analyze_live_variables(function, program);

    writeln!(out, "\tswitch hopId {{")?;

    // Generate cases for each hop
    for (hop_idx, _hop_id) in function.hops.iter().enumerate() {
        writeln!(out, "\tcase {}:", hop_idx)?;

        // If there's a next hop, extract its live-in variables from results and put into params
        if let Some(&next_hop_id) = function.hops.get(hop_idx + 1) {
            let next_hop = &program.hops[next_hop_id];
            if let Some(next_entry) = next_hop.entry_block {
                if let Some(live_in_lattice) = liveness.block_entry.get(&next_entry) {
                    if let Some(live_in_set) = live_in_lattice.as_set() {
                        let mut live_vars: Vec<_> =
                            live_in_set.iter().map(|LiveVar(v)| *v).collect();
                        live_vars.sort_by_key(|var_id| var_id.index());
                        if !live_vars.is_empty() {
                            writeln!(out, "\t\t// Extract live-out variables from results and put into params")?;
                            for (idx, var_id) in live_vars.iter().enumerate() {
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

        // Call the partition hop function to calculate nextShard
        writeln!(out, "\t\t// Deep copy params to avoid modification")?;
        writeln!(out, "\t\tparamsCopy := make(map[string]string)")?;
        writeln!(out, "\t\tfor k, v := range params {{")?;
        writeln!(out, "\t\t\tparamsCopy[k] = v")?;
        writeln!(out, "\t\t}}")?;
        writeln!(
            out,
            "\t\t// Calculate partition using partition hop function"
        )?;
        writeln!(
            out,
            "\t\tnextShard = int({}Hop{}Par(paramsCopy))",
            func_name, hop_idx
        )?;
    }

    writeln!(out, "\tdefault:")?;
    writeln!(out, "\t\treturn nil, params, history, 0")?;
    writeln!(out, "\t}}")?;
    writeln!(out)?;

    writeln!(out, "\thistory = append(history, uint32(nextShard))")?;
    writeln!(out, "\treq.Params = params")?;
    writeln!(out, "\treq.History = history")?;
    writeln!(out, "\treq.Info.Hopid += 1")?;
    writeln!(out, "\treq.Dependency = in.Dependency")?;
    writeln!(out, "\treturn req, params, history, nextShard")?;
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
        let hop_type_enum = hop_types.get(&(function_id, hop_id)).copied().unwrap_or(HopType::NormalHop);
        let hop_type = match hop_type_enum {
            HopType::NormalHop => "util.NormalHop",
            HopType::MergedHopBegin => "util.MergedHopBegin",
            HopType::MergedHop => "util.MergedHop",
            HopType::MergedHopEnd => "util.MergedHopEnd",
        };

        // Process function name
        let process_func = format!("{}Hop{}", func_name, hop_index);

        // Get conflicts map for this hop
        let conflicts_map = if let Some(hop_conflicts) = conflicts.get(&(function_id, hop_id)) {
            if hop_conflicts.is_empty() {
                "map[int32]int32{}".to_string()
            } else {
                // Build the conflicts map string
                let mut parts: Vec<String> = hop_conflicts.iter()
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
