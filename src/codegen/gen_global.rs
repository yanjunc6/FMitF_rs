use super::{
    gen_transaction::{go_function_name, lower_instruction_goto, operand_to_go},
    util::{go_type_string, go_var_name, pascal_case, write_go_file_header},
    GoProgram,
};
use crate::cfg::{self, FunctionKind, InstructionKind};
use crate::sc_graph::SCGraph;
use std::collections::HashSet;
use std::error::Error;
use std::fmt::Write;

pub(super) fn generate_global(program: &cfg::Program, sc_graph: &SCGraph) -> Result<GoProgram, Box<dyn Error>> {
    let content = build_global_source(program, sc_graph)?;
    Ok(GoProgram::new("global.go", content))
}

fn build_global_source(program: &cfg::Program, sc_graph: &SCGraph) -> Result<String, std::fmt::Error> {
    let mut out = String::new();

    write_go_file_header(&mut out, &["encoding/json", "github.com/boltdb/bolt"])?;

    writeln!(out, "func mustJSON(value any) []byte {{")?;
    writeln!(out, "\tbytes, err := json.Marshal(value)")?;
    writeln!(out, "\tif err != nil {{")?;
    writeln!(out, "\t\tpanic(err)")?;
    writeln!(out, "\t}}")?;
    writeln!(out, "\treturn bytes")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    writeln!(
        out,
        "func putData(bucket *bolt.Bucket, key []byte, value []byte) {{"
    )?;
    writeln!(out, "\tif bucket == nil {{")?;
    writeln!(out, "\t\tpanic(\"nil bucket\")")?;
    writeln!(out, "\t}}")?;
    writeln!(out, "\tif err := bucket.Put(key, value); err != nil {{")?;
    writeln!(out, "\t\tpanic(err)")?;
    writeln!(out, "\t}}")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    if program.global_consts.len() > 0 {
        writeln!(out, "const (")?;
        for (_, global_const) in &program.global_consts {
            let go_type = go_type_string(program, global_const.ty);
            let literal = go_constant_literal(&global_const.init);
            writeln!(out, "\t{} {} = {}", global_const.name, go_type, literal)?;
        }
        writeln!(out, ")")?;
        writeln!(out)?;
    }

    for table_id in &program.all_tables {
        let table = &program.tables[*table_id];
        let struct_name = go_type_name(&table.name);
        let key_struct_name = format!("{}Key", struct_name);
        let bucket_name = format!("t{}", struct_name);

        writeln!(out, "type {} struct {{", key_struct_name)?;
        for field_id in &table.primary_key_fields {
            let field = &program.table_fields[*field_id];
            let go_field_name = go_field_name(&field.name);
            let go_type = go_type_string(program, field.field_type);
            writeln!(out, "\t{} {}", go_field_name, go_type)?;
        }
        writeln!(out, "}}")?;
        writeln!(out)?;

        writeln!(out, "type {} struct {{", struct_name)?;
        writeln!(out, "\tKey {}", key_struct_name)?;
        for field_id in &table.other_fields {
            let field = &program.table_fields[*field_id];
            let go_field_name = go_field_name(&field.name);
            let go_type = go_type_string(program, field.field_type);
            writeln!(out, "\t{} {}", go_field_name, go_type)?;
        }
        writeln!(out, "}}")?;
        writeln!(out)?;

        writeln!(
            out,
            "func get{}(tx *bolt.Tx, key {}) ([]byte, {}) {{",
            struct_name, key_struct_name, struct_name
        )?;
        writeln!(out, "\tbucket := tx.Bucket([]byte(\"{}\"))", bucket_name)?;
        writeln!(out, "\tif bucket == nil {{")?;
        writeln!(out, "\t\tpanic(\"missing bucket {}\")", bucket_name)?;
        writeln!(out, "\t}}")?;
        writeln!(out, "\tkeyBytes := mustJSON(key)")?;
        writeln!(out, "\tvar row {}", struct_name)?;
        writeln!(out, "\tif data := bucket.Get(keyBytes); data != nil {{")?;
        writeln!(
            out,
            "\t\tif err := json.Unmarshal(data, &row); err != nil {{"
        )?;
        writeln!(out, "\t\t\tpanic(err)")?;
        writeln!(out, "\t\t}}")?;
        writeln!(out, "\t}}")?;
        writeln!(out, "\trow.Key = key")?;
        writeln!(out, "\treturn keyBytes, row")?;
        writeln!(out, "}}")?;
        writeln!(out)?;

        writeln!(
            out,
            "func put{}(tx *bolt.Tx, key {}, row {}) {{",
            struct_name, key_struct_name, struct_name
        )?;
        writeln!(out, "\tbucket := tx.Bucket([]byte(\"{}\"))", bucket_name)?;
        writeln!(out, "\tif bucket == nil {{")?;
        writeln!(out, "\t\tpanic(\"missing bucket {}\")", bucket_name)?;
        writeln!(out, "\t}}")?;
        writeln!(out, "\trow.Key = key")?;
        writeln!(out, "\tputData(bucket, mustJSON(key), mustJSON(row))")?;
        writeln!(out, "}}")?;
        writeln!(out)?;
    }

    generate_partition_functions(&mut out, program)?;

    generate_scheds_function(&mut out, program, sc_graph)?;
    generate_chains_function(&mut out, program)?;

    Ok(out)
}

fn generate_partition_functions(
    out: &mut String,
    program: &cfg::Program,
) -> Result<(), std::fmt::Error> {
    for &function_id in &program.all_functions {
        if matches!(program.functions[function_id].kind, FunctionKind::Partition) {
            write_partition_function(out, program, function_id)?;
        }
    }
    Ok(())
}

fn write_partition_function(
    out: &mut String,
    program: &cfg::Program,
    function_id: cfg::FunctionId,
) -> Result<(), std::fmt::Error> {
    let function = &program.functions[function_id];
    let go_name = go_function_name(program, function_id);

    let function_type = &program.types[function.signature.ty];
    let (param_types, return_type) = match function_type {
        cfg::Type::Function {
            param_types,
            return_type,
        } => (param_types, return_type.as_ref()),
        other => panic!("expected function type for partition, found {:?}", other),
    };

    let params: Vec<String> = function
        .params
        .iter()
        .zip(param_types.iter())
        .map(|(var_id, ty_id)| {
            let name = go_var_name(program, *var_id);
            let ty = go_type_string(program, *ty_id);
            format!("{} {}", name, ty)
        })
        .collect();

    let return_ty = go_type_string(program, *return_type);

    writeln!(
        out,
        "func {}({}) {} {{",
        go_name,
        params.join(", "),
        return_ty
    )?;

    let entry_block = function
        .entry_block
        .expect("partition must have entry block");

    // Collect all variable declarations (excluding parameters)
    let mut var_decls: HashSet<cfg::VariableId> = HashSet::new();
    for &block_id in &function.all_blocks {
        let block = &program.basic_blocks[block_id];
        for inst in &block.instructions {
            match &inst.kind {
                InstructionKind::Assign { dest, .. }
                | InstructionKind::BinaryOp { dest, .. }
                | InstructionKind::UnaryOp { dest, .. }
                | InstructionKind::TableGet { dest, .. } => {
                    if !function.params.contains(dest) {
                        var_decls.insert(*dest);
                    }
                }
                InstructionKind::Call { dest: Some(dest), .. } => {
                    if !function.params.contains(dest) {
                        var_decls.insert(*dest);
                    }
                }
                _ => {}
            }
        }
    }

    // Emit variable declarations
    for &var_id in &var_decls {
        let var_name = go_var_name(program, var_id);
        let var_type = &program.variables[var_id].ty;
        let type_str = go_type_string(program, *var_type);
        writeln!(out, "\tvar {} {}", var_name, type_str)?;
    }

    // Collect used labels
    let used_labels = collect_used_labels_partition(program, function);

    // Generate basic blocks with goto labels
    for &block_id in &function.all_blocks {
        let block = &program.basic_blocks[block_id];
        
        // Only emit label if it's used or it's the entry block
        if used_labels.contains(&block_id.index()) || block_id == entry_block {
            writeln!(out, "BB{}:", block_id.index())?;
        }

        // Emit instructions
        for inst in &block.instructions {
            lower_instruction_partition(out, program, inst, "\t")?;
        }

        // Emit terminator
        lower_partition_terminator_goto(out, program, &block.terminator, "\t")?;
    }

    writeln!(out, "}}")?;
    writeln!(out)?;

    Ok(())
}

// Helper to collect used labels for partition functions
fn collect_used_labels_partition(
    program: &cfg::Program,
    function: &cfg::Function,
) -> HashSet<usize> {
    let mut used: HashSet<usize> = HashSet::new();
    for &block_id in &function.all_blocks {
        let block = &program.basic_blocks[block_id];
        match &block.terminator {
            cfg::Terminator::Jump(target) => {
                used.insert(target.index());
            }
            cfg::Terminator::Branch { if_true, if_false, .. } => {
                used.insert(if_true.index());
                used.insert(if_false.index());
            }
            _ => {}
        }
    }
    used
}

// Lower instruction for partition functions (no table access tracking)
fn lower_instruction_partition(
    out: &mut String,
    program: &cfg::Program,
    inst: &cfg::Instruction,
    indent: &str,
) -> Result<(), std::fmt::Error> {
    // For partition functions, we stop at the first table access and return
    match &inst.kind {
        InstructionKind::TableGet { .. } | InstructionKind::TableSet { .. } => {
            // At first table access, calculate partition and return
            // This will be filled in properly when we know the partition logic
            writeln!(out, "{}// First table access - calculate partition", indent)?;
            writeln!(out, "{}if true {{", indent)?;
            writeln!(out, "{}\treturn 0 // TODO: call partition function", indent)?;
            writeln!(out, "{}}}", indent)?;
        }
        _ => {
            // For other instructions, use the goto-based lowering without table access tracking
            let mut temp_out = String::new();
            let mut initialized_vars = HashSet::new();
            let mut table_access_count = 0usize;
            lower_instruction_goto(
                &mut temp_out,
                program,
                inst,
                &mut initialized_vars,
                &mut table_access_count,
                indent,
            )?;
            out.push_str(&temp_out);
        }
    }
    Ok(())
}

fn lower_partition_terminator_goto(
    out: &mut String,
    program: &cfg::Program,
    term: &cfg::Terminator,
    indent: &str,
) -> Result<(), std::fmt::Error> {
    match term {
        cfg::Terminator::Jump(target) => {
            writeln!(out, "{}goto BB{}", indent, target.index())?;
        }

        cfg::Terminator::Branch {
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

        cfg::Terminator::Return(value) => {
            if let Some(val) = value {
                let expr = operand_to_go(program, val);
                writeln!(out, "{}return {}", indent, expr)?;
            } else {
                writeln!(out, "{}return", indent)?;
            }
        }

        cfg::Terminator::Abort => {
            writeln!(out, "{}panic(\"partition aborted\")", indent)?;
        }

        cfg::Terminator::HopExit { .. } => {
            writeln!(out, "{}panic(\"unexpected hop exit in partition\")", indent)?;
        }
    }

    Ok(())
}

fn go_type_name(name: &str) -> String {
    pascal_case(name)
}

fn go_field_name(name: &str) -> String {
    // Field identifiers can remain as-is as they are already exported (upper-case)
    name.to_string()
}

fn go_constant_literal(value: &cfg::ConstantValue) -> String {
    match value {
        cfg::ConstantValue::Int(i) => i.to_string(),
        cfg::ConstantValue::Float(f) => f.into_inner().to_string(),
        cfg::ConstantValue::Bool(b) => b.to_string(),
        cfg::ConstantValue::String(s) => format!("\"{}\"", s.escape_default()),
    }
}

fn generate_scheds_function(
    out: &mut String,
    program: &cfg::Program,
    sc_graph: &SCGraph,
) -> Result<(), std::fmt::Error> {
    // Determine which tables have C-edges
    let mut tables_with_c_edges: HashSet<cfg::TableId> = HashSet::new();
    
    for &function_id in &program.all_transactions {
        let function = &program.functions[function_id];
        for &hop_id in &function.hops {
            let hop = &program.hops[hop_id];
            
            // Check if this hop has C-edges
            if sc_graph.hop_has_c_edges(function_id, hop_id) {
                // Find all tables accessed in this hop
                for &block_id in &hop.blocks {
                    let block = &program.basic_blocks[block_id];
                    for inst in &block.instructions {
                        match &inst.kind {
                            cfg::InstructionKind::TableGet { table, .. }
                            | cfg::InstructionKind::TableSet { table, .. } => {
                                tables_with_c_edges.insert(*table);
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
    }

    writeln!(out, "func Scheds() {{")?;
    writeln!(out, "\t// Initialize scheduler maps")?;
    writeln!(out, "\tLockSchedulers = make(map[string]*Scheduler)")?;
    writeln!(out, "\tAccessSchedulers = make(map[string]*Scheduler)")?;
    writeln!(out, "\tAccessDSchedulers = make(map[string]*Scheduler)")?;
    writeln!(out)?;
    
    writeln!(out, "\t// Create one lock scheduler for each table")?;
    for &table_id in &program.all_tables {
        let table = &program.tables[table_id];
        let table_name = go_type_name(&table.name);
        writeln!(out, "\tLockSchedulers[\"{}\"] = NewScheduler()", table_name)?;
    }
    writeln!(out)?;

    if !tables_with_c_edges.is_empty() {
        writeln!(out, "\t// Create one access scheduler for each table in hops that has C-edges")?;
        for &table_id in &program.all_tables {
            if tables_with_c_edges.contains(&table_id) {
                let table = &program.tables[table_id];
                let table_name = go_type_name(&table.name);
                writeln!(out, "\tAccessSchedulers[\"{}\"] = NewScheduler()", table_name)?;
            }
        }
    }
    
    writeln!(out, "}}")?;
    writeln!(out)?;

    Ok(())
}

fn generate_chains_function(
    out: &mut String,
    program: &cfg::Program,
) -> Result<(), std::fmt::Error> {
    writeln!(out, "func Chains(db *bolt.DB) []*Chain {{")?;
    
    let chain_calls: Vec<String> = program
        .all_transactions
        .iter()
        .map(|&function_id| {
            let function = &program.functions[function_id];
            let func_name = pascal_case(&function.name);
            format!("{}ChainImpl(db)", func_name)
        })
        .collect();
    
    if chain_calls.is_empty() {
        writeln!(out, "\treturn []*Chain{{}}")?;
    } else {
        writeln!(out, "\treturn []*Chain{{")?;
        for call in chain_calls {
            writeln!(out, "\t\t{},", call)?;
        }
        writeln!(out, "\t}}")?;
    }
    
    writeln!(out, "}}")?;
    writeln!(out)?;

    Ok(())
}
