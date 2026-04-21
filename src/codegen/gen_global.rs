use super::{
    gen_helper::{
        collect_used_labels, collect_var_decls_from_instruction, go_function_name,
        lower_instruction, lower_terminator_goto, CodeGenContext,
    },
    util::{go_type_string, go_var_name, pascal_case, snake_case, write_go_file_header},
    GoProgram,
};
use crate::cfg::{self, FunctionKind, InstructionKind};
use crate::sc_graph::SCGraph;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::Write;

pub fn generate_global(
    program: &cfg::Program,
    sc_graph: &SCGraph,
) -> Result<GoProgram, Box<dyn Error>> {
    let content = build_global_source(program, sc_graph)?;
    Ok(GoProgram::new("global.go", content))
}

fn build_global_source(
    program: &cfg::Program,
    sc_graph: &SCGraph,
) -> Result<String, std::fmt::Error> {
    let mut out = String::new();

    write_go_file_header(
        &mut out,
        &["encoding/json", "sync", "github.com/boltdb/bolt"],
    )?;

    writeln!(out, "func mustJSON(value any) []byte {{")?;
    writeln!(out, "\tbytes, err := json.Marshal(value)")?;
    writeln!(out, "\tif err != nil {{")?;
    writeln!(out, "\t\tpanic(err)")?;
    writeln!(out, "\t}}")?;
    writeln!(out, "\treturn bytes")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    // Generate cache structs for each table
    generate_cache_structs(&mut out, program)?;

    // Generate flush caches function
    generate_flush_caches(&mut out, program)?;

    // Generate iterator get_* functions for each table's primary keys
    generate_iterator_getters(&mut out, program)?;

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

        // Generate unlocked flush function for this table (internal helper)
        writeln!(
            out,
            "// flush{}CacheUnlocked contains the core logic for flushing.",
            struct_name
        )?;
        writeln!(
            out,
            "// It is unexported and \"unsafe\" because it assumes the caller has already acquired the necessary lock."
        )?;
        writeln!(
            out,
            "func flush{}CacheUnlocked(tx *bolt.Tx) {{",
            struct_name
        )?;
        writeln!(
            out,
            "\tif {}Cache.row != nil && {}Cache.isDirty {{",
            snake_case(&table.name),
            snake_case(&table.name)
        )?;
        writeln!(out, "\t\tbucket := tx.Bucket([]byte(\"{}\"))", bucket_name)?;
        writeln!(out, "\t\tif bucket == nil {{")?;
        writeln!(out, "\t\t\tpanic(\"missing bucket {}\")", bucket_name)?;
        writeln!(out, "\t\t}}")?;
        writeln!(
            out,
            "\t\tvar key {} = {}Cache.row.Key",
            key_struct_name,
            snake_case(&table.name)
        )?;
        writeln!(
            out,
            "\t\tvar row {} = *{}Cache.row",
            struct_name,
            snake_case(&table.name)
        )?;
        writeln!(out, "\t\tputData(bucket, mustJSON(key), mustJSON(row))")?;
        writeln!(out, "\t}}")?;
        writeln!(out, "\t// Clear the cache after flushing.")?;
        writeln!(out, "\t{}Cache.row = nil", snake_case(&table.name))?;
        writeln!(out, "\t{}Cache.isDirty = false", snake_case(&table.name))?;
        writeln!(out, "}}")?;
        writeln!(out)?;

        // Generate public thread-safe flush function for this table
        writeln!(
            out,
            "// flush{}Cache is the public, thread-safe API for flushing the cache.",
            struct_name
        )?;
        writeln!(
            out,
            "// It acquires the lock before calling the core logic."
        )?;
        writeln!(out, "func flush{}Cache(tx *bolt.Tx) {{", struct_name)?;
        writeln!(out, "\t{}Cache.lock.Lock()", snake_case(&table.name))?;
        writeln!(
            out,
            "\tdefer {}Cache.lock.Unlock()",
            snake_case(&table.name)
        )?;
        writeln!(out, "\tflush{}CacheUnlocked(tx)", struct_name)?;
        writeln!(out, "}}")?;
        writeln!(out)?;

        // Generate get function with cache (thread-safe with double-checked locking)
        writeln!(
            out,
            "// get{} fetches data, utilizing the cache in a highly concurrent and safe manner.",
            struct_name
        )?;
        writeln!(
            out,
            "func get{}(tx *bolt.Tx, key {}) ([]byte, {}) {{",
            struct_name, key_struct_name, struct_name
        )?;
        writeln!(
            out,
            "\t// 1. Fast path (Read-only): Use a read lock for concurrent cache checks."
        )?;
        writeln!(out, "\t{}Cache.lock.RLock()", snake_case(&table.name))?;
        writeln!(
            out,
            "\tif {}Cache.row != nil && {}Cache.row.Key == key {{",
            snake_case(&table.name),
            snake_case(&table.name)
        )?;
        writeln!(out, "\t\trow := *{}Cache.row", snake_case(&table.name))?;
        writeln!(out, "\t\t{}Cache.lock.RUnlock()", snake_case(&table.name))?;
        writeln!(out, "\t\treturn mustJSON(key), row // Cache Hit")?;
        writeln!(out, "\t}}")?;
        writeln!(out, "\t{}Cache.lock.RUnlock()", snake_case(&table.name))?;
        writeln!(out)?;
        writeln!(
            out,
            "\t// 2. Slow path (Write path): Acquire an exclusive lock."
        )?;
        writeln!(out, "\t{}Cache.lock.Lock()", snake_case(&table.name))?;
        writeln!(
            out,
            "\tdefer {}Cache.lock.Unlock()",
            snake_case(&table.name)
        )?;
        writeln!(out)?;
        writeln!(out, "\t// 3. Double-check in case another goroutine populated the cache while we waited for the lock.")?;
        writeln!(
            out,
            "\tif {}Cache.row != nil && {}Cache.row.Key == key {{",
            snake_case(&table.name),
            snake_case(&table.name)
        )?;
        writeln!(
            out,
            "\t\treturn mustJSON(key), *{}Cache.row // Cache Hit",
            snake_case(&table.name)
        )?;
        writeln!(out, "\t}}")?;
        writeln!(out)?;
        writeln!(
            out,
            "\t// 4. Cache Miss: Call the \"unlocked\" helper since we already hold the lock."
        )?;
        writeln!(out, "\tflush{}CacheUnlocked(tx)", struct_name)?;
        writeln!(out)?;
        writeln!(out, "\t// 5. Proceed with DB lookup.")?;
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
        writeln!(out, "\t")?;
        writeln!(
            out,
            "\t// 6. Update cache with the newly fetched, clean row."
        )?;
        writeln!(out, "\t{}Cache.row = &row", snake_case(&table.name))?;
        writeln!(out, "\t{}Cache.isDirty = false", snake_case(&table.name))?;
        writeln!(out)?;
        writeln!(out, "\treturn keyBytes, row")?;
        writeln!(out, "}}")?;
        writeln!(out)?;

        // Generate put function with cache (thread-safe)
        writeln!(
            out,
            "// put{} updates the cache and marks it as dirty (write-back).",
            struct_name
        )?;
        writeln!(
            out,
            "func put{}(tx *bolt.Tx, key {}, row {}) {{",
            struct_name, key_struct_name, struct_name
        )?;
        writeln!(out, "\t{}Cache.lock.Lock()", snake_case(&table.name))?;
        writeln!(
            out,
            "\tdefer {}Cache.lock.Unlock()",
            snake_case(&table.name)
        )?;
        writeln!(out)?;
        writeln!(
            out,
            "\t// If the cache holds a different row, call the \"unlocked\" helper to flush it."
        )?;
        writeln!(
            out,
            "\tif {}Cache.row != nil && {}Cache.row.Key != key {{",
            snake_case(&table.name),
            snake_case(&table.name)
        )?;
        writeln!(out, "\t\tflush{}CacheUnlocked(tx)", struct_name)?;
        writeln!(out, "\t}}")?;
        writeln!(out)?;
        writeln!(
            out,
            "\t// Place the new row in the cache and mark it as dirty."
        )?;
        writeln!(out, "\trow.Key = key")?;
        writeln!(out, "\t{}Cache.row = &row", snake_case(&table.name))?;
        writeln!(out, "\t{}Cache.isDirty = true", snake_case(&table.name))?;
        writeln!(out, "}}")?;
        writeln!(out)?;

        // Generate partition versions of table access functions
        if let Some(part_func_id) = table.node_partition {
            // getPar function - returns partition instead of reading data
            writeln!(
                out,
                "func get{}Par(key {}) uint64 {{",
                struct_name, key_struct_name
            )?;
            writeln!(out, "\t// Call partition function to determine shard")?;

            // Build call arguments using node_partition_args
            let call_args: Vec<String> = table
                .node_partition_args
                .iter()
                .map(|arg| match arg {
                    cfg::PartitionArg::Field(field_id) => {
                        let field = &program.table_fields[*field_id];
                        format!("key.{}", field.name)
                    }
                    cfg::PartitionArg::Constant(val) => match val {
                        cfg::ConstantValue::Int(i) => i.to_string(),
                        cfg::ConstantValue::Float(f) => f.to_string(),
                        cfg::ConstantValue::Bool(b) => b.to_string(),
                        cfg::ConstantValue::String(s) => format!("\"{}\"", s),
                        cfg::ConstantValue::Null => "nil".to_string(),
                    },
                })
                .collect();

            let go_part_func_name = go_function_name(program, part_func_id);
            writeln!(
                out,
                "\treturn {}({})",
                go_part_func_name,
                call_args.join(", ")
            )?;
            writeln!(out, "}}")?;
            writeln!(out)?;

            // putPar function - also just returns partition (no difference for partition calculation)
            writeln!(
                out,
                "func put{}Par(key {}) uint64 {{",
                struct_name, key_struct_name
            )?;
            writeln!(out, "\t// Call partition function to determine shard")?;
            writeln!(
                out,
                "\treturn {}({})",
                go_part_func_name,
                call_args.join(", ")
            )?;
            writeln!(out, "}}")?;
            writeln!(out)?;
        }
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
    let mut var_decls: HashMap<cfg::VariableId, String> = HashMap::new();
    for &block_id in &function.all_blocks {
        let block = &program.basic_blocks[block_id];
        for inst in &block.instructions {
            if let InstructionKind::Assign { dest, .. }
            | InstructionKind::BinaryOp { dest, .. }
            | InstructionKind::UnaryOp { dest, .. }
            | InstructionKind::TableGet { dest, .. }
            | InstructionKind::Call {
                dest: Some(dest), ..
            } = &inst.kind
            {
                if !function.params.contains(dest) {
                    collect_var_decls_from_instruction(program, inst, &mut var_decls);
                }
            }
        }
    }

    // Emit variable declarations
    for (&var_id, type_str) in &var_decls {
        let var_name = go_var_name(program, var_id);
        writeln!(out, "\tvar {} {}", var_name, type_str)?;
    }

    // Collect used labels
    let mut used_labels: HashSet<cfg::BasicBlockId> = HashSet::new();
    for &block_id in &function.all_blocks {
        let block = &program.basic_blocks[block_id];
        collect_used_labels(&block.terminator, &mut used_labels);
    }

    // Add initial goto to entry block to ensure all labels are reachable
    writeln!(out, "\tgoto BB{}", entry_block.index())?;
    writeln!(out)?;

    // Generate basic blocks with goto labels
    for &block_id in &function.all_blocks {
        let block = &program.basic_blocks[block_id];

        // Only emit label if it's used or it's the entry block
        if used_labels.contains(&block_id) || block_id == entry_block {
            writeln!(out, "BB{}:", block_id.index())?;
        }

        // Emit instructions
        for inst in &block.instructions {
            lower_instruction_partition(out, program, inst, "\t")?;
        }

        // Emit terminator
        lower_terminator_goto(
            out,
            program,
            &block.terminator,
            "\t",
            None,
            CodeGenContext::PartitionFunction,
        )?;
    }

    writeln!(out, "}}")?;
    writeln!(out)?;

    Ok(())
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
            lower_instruction(
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
        cfg::ConstantValue::Null => "nil".to_string(),
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
        writeln!(
            out,
            "\t// Create one access scheduler for each table in hops that has C-edges"
        )?;
        for &table_id in &program.all_tables {
            if tables_with_c_edges.contains(&table_id) {
                let table = &program.tables[table_id];
                let table_name = go_type_name(&table.name);
                writeln!(
                    out,
                    "\tAccessSchedulers[\"{}\"] = NewScheduler()",
                    table_name
                )?;
            }
        }
    }

    writeln!(out, "}}")?;
    writeln!(out)?;

    Ok(())
}

fn generate_iterator_getters(
    out: &mut String,
    program: &cfg::Program,
) -> Result<(), std::fmt::Error> {
    writeln!(out, "// Iterator getter functions for table primary keys")?;
    writeln!(out, "// Generated for each primary key field in each table")?;
    writeln!(out)?;

    // Build a map from field_name to a list of all matching function IDs (in order)
    // We'll consume them in order as we iterate through tables
    use std::collections::HashMap;
    let mut field_func_lists: HashMap<String, Vec<cfg::FunctionId>> = HashMap::new();

    for (func_id, func) in program.functions.iter() {
        // Only consider functions with @generated decorator
        if !func.decorators.iter().any(|d| d.name == "generated") {
            continue;
        }

        // Extract field name from function name (e.g., "get_id" -> "id")
        if let Some(field_name) = func.name.strip_prefix("get_") {
            field_func_lists
                .entry(field_name.to_string())
                .or_insert_with(Vec::new)
                .push(func_id);
        }
    }

    for &table_id in &program.all_tables {
        let table = &program.tables[table_id];
        let table_name = pascal_case(&table.name);

        // Generate a get_* function for each primary key field
        for &field_id in &table.primary_key_fields {
            let field = &program.table_fields[field_id];
            let field_name = &field.name;
            let field_type = go_type_string(program, field.field_type);
            let func_name = format!("get_{}", field_name);

            // Get the next function ID for this field name (consuming in order)
            let maybe_func_id = field_func_lists.get_mut(field_name).and_then(|func_list| {
                if !func_list.is_empty() {
                    Some(func_list.remove(0))
                } else {
                    None
                }
            });

            let final_func_name = if let Some(func_id) = maybe_func_id {
                // Use function ID suffix to avoid naming conflicts
                format!("{}_{}", func_name, func_id.index())
            } else {
                func_name
            };

            writeln!(
                out,
                "// {} extracts {} from the iterator for table {}",
                final_func_name, field_name, table_name
            )?;
            writeln!(
                out,
                "func {}(iter *Iterator) {} {{",
                final_func_name, field_type
            )?;
            writeln!(out, "\tif !iter.isValid {{")?;
            writeln!(
                out,
                "\t\tpanic(\"{}: iterator is not valid\")",
                final_func_name
            )?;
            writeln!(out, "\t}}")?;
            writeln!(out)?;
            writeln!(
                out,
                "\t// Deserialize the composite key to extract {}",
                field_name
            )?;
            writeln!(out, "\ttype CompositeKey struct {{")?;

            // Include all primary key fields in the composite key structure
            for &pk_field_id in &table.primary_key_fields {
                let pk_field = &program.table_fields[pk_field_id];
                let pk_field_type = go_type_string(program, pk_field.field_type);
                writeln!(out, "\t\t{} {}", pk_field.name, pk_field_type)?;
            }

            writeln!(out, "\t}}")?;
            writeln!(out)?;
            writeln!(out, "\tvar key CompositeKey")?;
            writeln!(
                out,
                "\tif err := json.Unmarshal(iter.currentKey, &key); err != nil {{"
            )?;
            writeln!(out, "\t\tpanic(err)")?;
            writeln!(out, "\t}}")?;
            writeln!(out)?;
            writeln!(out, "\treturn key.{}", field_name)?;
            writeln!(out, "}}")?;
            writeln!(out)?;
        }
    }

    Ok(())
}

fn generate_cache_structs(out: &mut String, program: &cfg::Program) -> Result<(), std::fmt::Error> {
    for &table_id in &program.all_tables {
        let table = &program.tables[table_id];
        let struct_name = pascal_case(&table.name);
        let cache_var = format!("{}Cache", snake_case(&table.name));

        writeln!(
            out,
            "// Global variable for the single-row write-back cache with a RWMutex."
        )?;
        writeln!(out, "var {} struct {{", cache_var)?;
        writeln!(out, "\tlock    sync.RWMutex")?;
        writeln!(out, "\trow     *{}", struct_name)?;
        writeln!(out, "\tisDirty bool")?;
        writeln!(out, "}}")?;
        writeln!(out)?;
    }
    Ok(())
}

fn generate_flush_caches(out: &mut String, program: &cfg::Program) -> Result<(), std::fmt::Error> {
    writeln!(out, "// FlushAllCaches flushes all table caches")?;
    writeln!(out, "func FlushAllCaches(tx *bolt.Tx) {{")?;
    for &table_id in &program.all_tables {
        let table = &program.tables[table_id];
        let struct_name = pascal_case(&table.name);
        writeln!(out, "\tflush{}Cache(tx)", struct_name)?;
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
