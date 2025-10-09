use super::{
    util::{go_var_name, pascal_case, snake_case, write_go_file_header},
    GoProgram,
};
use crate::cfg::{
    self, BinaryOp, FunctionKind, Instruction, InstructionKind, Operand, Terminator, UnaryOp,
};
use crate::dataflow::{analyze_live_variables, DataflowResults, LiveVar, SetLattice};
use std::collections::HashMap;
use std::error::Error;
use std::fmt::Write;

pub fn generate_transactions(program: &cfg::Program) -> Result<Vec<GoProgram>, Box<dyn Error>> {
    let mut files = Vec::new();

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
            &["github.com/boltdb/bolt", "ShardDB/proto", "strconv", "fmt"],
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

        // Generate NextReq function
        generate_next_req(&mut content, program, function_id, &struct_name)?;

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

    writeln!(out, "\tparams := in.Params")?;
    writeln!(out)?;

    // Run liveness analysis on the entire function
    let liveness = analyze_live_variables(function, program);

    // Get live-in variables for this hop's entry block
    let entry_block = hop.entry_block.expect("Hop must have entry block");
    let live_in_set = liveness
        .block_entry
        .get(&entry_block)
        .and_then(|lattice| lattice.as_set())
        .map(|set| set.iter().map(|LiveVar(v)| *v).collect::<Vec<_>>())
        .unwrap_or_default();

    // Get live-in variables for the next hop (these are the live-out of current hop)
    let next_hop_live_in = if let Some(next_id) = next_hop_id {
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

    // Extract live-in variables from params
    for var_id in &live_in_set {
        let var = &program.variables[*var_id];
        let var_name = go_var_name(program, *var_id);
        let var_type = &program.types[var.ty];

        match var_type {
            cfg::Type::Primitive(cfg::PrimitiveType::Int) => {
                writeln!(
                    out,
                    "\t{}, _ := strconv.ParseUint(params[\"{}\"], 10, 64)",
                    var_name, var_name
                )?;
            }
            cfg::Type::Primitive(cfg::PrimitiveType::Float) => {
                writeln!(
                    out,
                    "\t{}, _ := strconv.ParseFloat(params[\"{}\"], 32)",
                    var_name, var_name
                )?;
            }
            cfg::Type::Primitive(cfg::PrimitiveType::Bool) => {
                writeln!(
                    out,
                    "\t{}, _ := strconv.ParseBool(params[\"{}\"])",
                    var_name, var_name
                )?;
            }
            cfg::Type::Primitive(cfg::PrimitiveType::String) => {
                writeln!(out, "\t{} := params[\"{}\"]", var_name, var_name)?;
            }
            _ => {
                // For complex types, deserialize from JSON
                writeln!(out, "\tvar {} interface{{}}", var_name)?;
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

    // Track if we need to generate basic block labels
    let needs_labels = hop.blocks.len() > 1;

    // Generate variables map to track locals
    let mut var_decls = HashMap::new();
    let mut table_access_count = 0;

    // Mark live-in variables as already declared
    for var_id in &live_in_set {
        var_decls.insert(*var_id, true);
    }

    // Process each basic block
    for (_bb_idx, &block_id) in hop.blocks.iter().enumerate() {
        let block = &program.basic_blocks[block_id];

        // Generate label for blocks (except first if it's the only one)
        if needs_labels {
            writeln!(out, "bb{}:", block_id.index())?;
        }

        // Lower each instruction
        for inst in &block.instructions {
            lower_instruction(out, program, inst, &mut var_decls, &mut table_access_count)?;
        }

        // Lower terminator
        lower_terminator(
            out,
            program,
            &block.terminator,
            needs_labels,
            &liveness,
            block_id,
            &next_hop_live_in,
        )?;
        writeln!(out)?;
    }

    writeln!(out, "}}")?;
    writeln!(out)?;

    Ok(())
}

fn lower_instruction(
    out: &mut String,
    program: &cfg::Program,
    inst: &Instruction,
    var_decls: &mut HashMap<cfg::VariableId, bool>,
    table_access_count: &mut usize,
) -> Result<(), std::fmt::Error> {
    match &inst.kind {
        InstructionKind::Assign { dest, src } => {
            let dest_name = go_var_name(program, *dest);
            let src_go = operand_to_go(program, src);

            if !var_decls.contains_key(dest) {
                writeln!(out, "\t{} := {}", dest_name, src_go)?;
                var_decls.insert(*dest, true);
            } else {
                writeln!(out, "\t{} = {}", dest_name, src_go)?;
            }
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

            if !var_decls.contains_key(dest) {
                writeln!(
                    out,
                    "\t{} := {} {} {}",
                    dest_name, left_go, op_str, right_go
                )?;
                var_decls.insert(*dest, true);
            } else {
                writeln!(out, "\t{} = {} {} {}", dest_name, left_go, op_str, right_go)?;
            }
        }

        InstructionKind::UnaryOp { dest, op, operand } => {
            let dest_name = go_var_name(program, *dest);
            let operand_go = operand_to_go(program, operand);
            let op_str = unary_op_to_go(*op);

            if !var_decls.contains_key(dest) {
                writeln!(out, "\t{} := {}{}", dest_name, op_str, operand_go)?;
                var_decls.insert(*dest, true);
            } else {
                writeln!(out, "\t{} = {}{}", dest_name, op_str, operand_go)?;
            }
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
            writeln!(out, "\t\t// TableGet: {}", table_name)?;
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

            if let Some(field_id) = field {
                // Getting specific field
                let field_info = &program.table_fields[*field_id];
                let row_var = format!("row{}", table_access_count);
                writeln!(
                    out,
                    "\t{}, {} := get{}(tx, {}Key{{ {} }})",
                    key_var,
                    row_var,
                    table_name,
                    table_name,
                    key_args.join(", ")
                )?;

                if !var_decls.contains_key(dest) {
                    writeln!(out, "\t{} := {}.{}", dest_name, row_var, field_info.name)?;
                    var_decls.insert(*dest, true);
                } else {
                    writeln!(out, "\t{} = {}.{}", dest_name, row_var, field_info.name)?;
                }
            } else {
                // Getting entire row
                if !var_decls.contains_key(dest) {
                    writeln!(
                        out,
                        "\t{}, {} := get{}(tx, {}Key{{ {} }})",
                        key_var,
                        dest_name,
                        table_name,
                        table_name,
                        key_args.join(", ")
                    )?;
                    var_decls.insert(*dest, true);
                } else {
                    writeln!(
                        out,
                        "\t{}, {} = get{}(tx, {}Key{{ {} }})",
                        key_var,
                        dest_name,
                        table_name,
                        table_name,
                        key_args.join(", ")
                    )?;
                }
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

            writeln!(out, "\t\t// TableSet: {}", table_name)?;
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
                "\t{}, {} := get{}(tx, {}Key{{ {} }})",
                key_var,
                row_var,
                table_name,
                table_name,
                key_args.join(", ")
            )?;

            if let Some(field_id) = field {
                // Setting specific field
                let field_info = &program.table_fields[*field_id];
                writeln!(
                    out,
                    "\t{}.{} = {}",
                    row_var,
                    field_info.name,
                    operand_to_go(program, value)
                )?;
            }

            // Write back
            writeln!(
                out,
                "\tputData(tx.Bucket([]byte(\"t{}\")), {}, mustJSON({}))",
                table_name, key_var, row_var
            )?;
        }

        InstructionKind::Assert { condition, message } => {
            let cond_go = operand_to_go(program, condition);
            writeln!(out, "\tif !({}) {{", cond_go)?;
            writeln!(out, "\t\tpanic(\"{}\")", message.replace('"', "\\\""))?;
            writeln!(out, "\t}}")?;
        }

        InstructionKind::Call {
            dest,
            func,
            args: _,
        } => {
            let func_info = &program.functions[*func];
            writeln!(out, "\t\t// Call: {}", func_info.name)?;
            writeln!(out, "\t\t// TODO: implement function call lowering")?;
            if let Some(dest_var) = dest {
                let dest_name = go_var_name(program, *dest_var);
                if !var_decls.contains_key(dest_var) {
                    writeln!(out, "\tvar {} interface{{}}", dest_name)?;
                    var_decls.insert(*dest_var, true);
                }
            }
        }
    }

    Ok(())
}

fn lower_terminator(
    out: &mut String,
    program: &cfg::Program,
    term: &Terminator,
    use_goto: bool,
    liveness: &DataflowResults<SetLattice<LiveVar>>,
    block_id: cfg::BasicBlockId,
    next_hop_live_in: &[cfg::VariableId],
) -> Result<(), std::fmt::Error> {
    match term {
        Terminator::Jump(target) => {
            if use_goto {
                writeln!(out, "\tgoto bb{}", target.index())?;
            }
        }

        Terminator::Branch {
            condition,
            if_true,
            if_false,
        } => {
            let cond_go = operand_to_go(program, condition);
            if use_goto {
                writeln!(out, "\tif {} {{", cond_go)?;
                writeln!(out, "\t\tgoto bb{}", if_true.index())?;
                writeln!(out, "\t}} else {{")?;
                writeln!(out, "\t\tgoto bb{}", if_false.index())?;
                writeln!(out, "\t}}")?;
            } else {
                writeln!(out, "\t// Branch without goto")?;
            }
        }

        Terminator::Return(value) => {
            if let Some(val) = value {
                let val_go = operand_to_go(program, val);
                writeln!(out, "\treturn &proto.TrxRes{{")?;
                writeln!(out, "\t\tStatus: proto.Status_Success,")?;
                writeln!(out, "\t\tInfo:   in.Info,")?;
                writeln!(out, "\t\tResults: []string{{fmt.Sprint({})}},", val_go)?;
                writeln!(out, "\t}}, nil")?;
            } else {
                writeln!(out, "\treturn &proto.TrxRes{{")?;
                writeln!(out, "\t\tStatus: proto.Status_Success,")?;
                writeln!(out, "\t\tInfo:   in.Info,")?;
                writeln!(out, "\t}}, nil")?;
            }
        }

        Terminator::HopExit { .. } => {
            // Collect variables that are live-in at the next hop (these are live-out of current hop)
            if !next_hop_live_in.is_empty() {
                writeln!(out, "\t// Collect live-out variables to results")?;
                let results: Vec<String> = next_hop_live_in
                    .iter()
                    .map(|var_id| {
                        let var_name = go_var_name(program, *var_id);
                        format!("fmt.Sprint({})", var_name)
                    })
                    .collect();
                writeln!(out, "\treturn &proto.TrxRes{{")?;
                writeln!(out, "\t\tStatus: proto.Status_Success,")?;
                writeln!(out, "\t\tInfo:   in.Info,")?;
                writeln!(out, "\t\tResults: []string{{{}}},", results.join(", "))?;
                writeln!(out, "\t}}, nil")?;
                return Ok(());
            }
            // Fallback: no live-out variables (last hop or no inter-hop dependencies)
            writeln!(out, "\treturn &proto.TrxRes{{")?;
            writeln!(out, "\t\tStatus: proto.Status_Success,")?;
            writeln!(out, "\t\tInfo:   in.Info,")?;
            writeln!(out, "\t}}, nil")?;
        }

        Terminator::Abort => {
            writeln!(out, "\tpanic(\"transaction aborted\")")?;
        }
    }

    Ok(())
}

fn operand_to_go(program: &cfg::Program, operand: &Operand) -> String {
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

fn binary_op_to_go(op: BinaryOp) -> &'static str {
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

fn unary_op_to_go(op: UnaryOp) -> &'static str {
    match op {
        UnaryOp::NegInt | UnaryOp::NegFloat => "-",
        UnaryOp::NotBool => "!",
    }
}

fn generate_next_req(
    out: &mut String,
    program: &cfg::Program,
    function_id: cfg::FunctionId,
    struct_name: &str,
) -> Result<(), std::fmt::Error> {
    let function = &program.functions[function_id];

    writeln!(
        out,
        "func (chain *{}) NextReq(in *proto.TrxRes, params map[string]string, history []uint32, shards int) (*proto.TrxReq, map[string]string, []uint32, int) {{",
        struct_name
    )?;

    writeln!(out, "\thopId := in.Info.Hopid")?;
    writeln!(out, "\treq := &proto.TrxReq{{Info: in.Info}}")?;
    writeln!(out, "\tnextShard := 0")?;
    writeln!(out)?;

    // Run liveness analysis to determine what variables need to be passed between hops
    let liveness = analyze_live_variables(function, program);

    writeln!(out, "\tswitch hopId {{")?;

    // Generate cases for each hop
    for (hop_idx, &hop_id) in function.hops.iter().enumerate() {
        writeln!(out, "\tcase {}:", hop_idx)?;

        // If there's a next hop, extract its live-in variables from results and put into params
        if let Some(&next_hop_id) = function.hops.get(hop_idx + 1) {
            let next_hop = &program.hops[next_hop_id];
            if let Some(next_entry) = next_hop.entry_block {
                if let Some(live_in_lattice) = liveness.block_entry.get(&next_entry) {
                    if let Some(live_in_set) = live_in_lattice.as_set() {
                        let live_vars: Vec<_> = live_in_set.iter().map(|LiveVar(v)| *v).collect();
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

        // Find the first table access in this hop to determine partition
        let hop = &program.hops[hop_id];
        let mut found_partition = false;

        'block_loop: for &block_id in &hop.blocks {
            let block = &program.basic_blocks[block_id];
            for inst in &block.instructions {
                if let InstructionKind::TableGet { table, .. }
                | InstructionKind::TableSet { table, .. } = &inst.kind
                {
                    let table_info = &program.tables[*table];

                    // If table has a partition function, evaluate it
                    if let Some(part_func_id) = table_info.node_partition {
                        let part_func = &program.functions[part_func_id];

                        // Generate partition evaluation code
                        writeln!(out, "\t\t// Partition using {}", part_func.name)?;

                        // Build partition function call based on partition arguments
                        match part_func.name.as_str() {
                            "f" => {
                                // f(x) = 10 * x
                                let key_field = table_info.node_partition_args[0];
                                let field_info = &program.table_fields[key_field];
                                writeln!(
                                    out,
                                    "\t\t{} := toUint64(params[\"{}\"])",
                                    field_info.name, field_info.name
                                )?;
                                writeln!(out, "\t\tnextShard = int({} * 10)", field_info.name)?;
                            }
                            "g" => {
                                // g(x, y) = 10 * x + y
                                let key_field_0 = table_info.node_partition_args[0];
                                let key_field_1 = table_info.node_partition_args[1];
                                let field_info_0 = &program.table_fields[key_field_0];
                                let field_info_1 = &program.table_fields[key_field_1];
                                writeln!(
                                    out,
                                    "\t\t{} := toUint64(params[\"{}\"])",
                                    field_info_0.name, field_info_0.name
                                )?;
                                writeln!(
                                    out,
                                    "\t\t{} := toUint64(params[\"{}\"])",
                                    field_info_1.name, field_info_1.name
                                )?;
                                writeln!(
                                    out,
                                    "\t\tnextShard = int({} * 10 + {})",
                                    field_info_0.name, field_info_1.name
                                )?;
                            }
                            "h" => {
                                // h(x, y) = 10 * x + (y % 10)
                                let key_field_0 = table_info.node_partition_args[0];
                                let key_field_1 = table_info.node_partition_args[1];
                                let field_info_0 = &program.table_fields[key_field_0];
                                let field_info_1 = &program.table_fields[key_field_1];
                                writeln!(
                                    out,
                                    "\t\t{} := toUint64(params[\"{}\"])",
                                    field_info_0.name, field_info_0.name
                                )?;
                                writeln!(
                                    out,
                                    "\t\t{} := toUint64(params[\"{}\"])",
                                    field_info_1.name, field_info_1.name
                                )?;
                                writeln!(
                                    out,
                                    "\t\tnextShard = int({} * 10 + ({} % 10))",
                                    field_info_0.name, field_info_1.name
                                )?;
                            }
                            _ => {
                                writeln!(
                                    out,
                                    "\t\t// Unknown partition function: {}",
                                    part_func.name
                                )?;
                                writeln!(out, "\t\tnextShard = 0")?;
                            }
                        }

                        found_partition = true;
                        break 'block_loop;
                    }
                }
            }
        }

        if !found_partition {
            writeln!(
                out,
                "\t\t// No partition function found, default to shard 0"
            )?;
            writeln!(out, "\t\tnextShard = 0")?;
        }
    }

    writeln!(out, "\tdefault:")?;
    writeln!(out, "\t\tpanic(\"invalid hop id\")")?;
    writeln!(out, "\t}}")?;
    writeln!(out)?;

    writeln!(out, "\thistory = append(history, uint32(nextShard))")?;
    writeln!(out, "\treq.Params = params")?;
    writeln!(out, "\treq.History = history")?;
    writeln!(out, "\treq.Info.Hopid += 1")?;
    writeln!(out, "\treq.Dependency = in.Dependency")?;
    writeln!(out, "\treturn req, params, history, nextShard")?;
    writeln!(out, "}}")?;

    Ok(())
}
