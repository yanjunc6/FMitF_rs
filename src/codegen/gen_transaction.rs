use super::{
    state_machine::GoStateMachine,
    util::{go_type_string, go_var_name, pascal_case, snake_case, write_go_file_header},
    GoProgram,
};
use crate::cfg::{
    self, BinaryOp, ConstantValue, FunctionKind, Instruction, InstructionKind, Operand, Terminator,
    UnaryOp,
};
use crate::dataflow::{analyze_live_variables, DataflowResults, LiveVar, SetLattice};
use std::collections::{HashMap, HashSet};
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

    let mut state_machine = GoStateMachine::new(program);
    let mut initialized_vars: HashSet<cfg::VariableId> = HashSet::new();

    // Extract live-in variables from params
    for var_id in &live_in_set {
        let var = &program.variables[*var_id];
        let var_name = go_var_name(program, *var_id);
        let var_type = &program.types[var.ty];

        let decl_type = match var_type {
            cfg::Type::Primitive(cfg::PrimitiveType::Int)
            | cfg::Type::Primitive(cfg::PrimitiveType::Float)
            | cfg::Type::Primitive(cfg::PrimitiveType::Bool)
            | cfg::Type::Primitive(cfg::PrimitiveType::String)
            | cfg::Type::List(_) => go_type_string(program, var.ty),
            _ => String::from("interface{}"),
        };
        state_machine.ensure_var_decl(*var_id, Some(decl_type.clone()));

        match var_type {
            cfg::Type::Primitive(cfg::PrimitiveType::Int) => {
                state_machine.add_pre_stmt(format!(
                    "\t{} = toUint64(params[\"{}\"])",
                    var_name, var_name
                ));
            }
            cfg::Type::Primitive(cfg::PrimitiveType::Float) => {
                state_machine.add_pre_stmt(format!(
                    "\t{} = toFloat32(params[\"{}\"])",
                    var_name, var_name
                ));
            }
            cfg::Type::Primitive(cfg::PrimitiveType::Bool) => {
                state_machine
                    .add_pre_stmt(format!("\t{} = toBool(params[\"{}\"])", var_name, var_name));
            }
            cfg::Type::Primitive(cfg::PrimitiveType::String) => {
                state_machine.add_pre_stmt(format!("\t{} = params[\"{}\"]", var_name, var_name));
            }
            cfg::Type::List(_) => {
                state_machine.add_pre_stmt(format!(
                    "\tjson.Unmarshal([]byte(params[\"{}\"]), &{})",
                    var_name, var_name
                ));
            }
            _ => {
                state_machine.add_pre_stmt(format!(
                    "\tjson.Unmarshal([]byte(params[\"{}\"]), &{})",
                    var_name, var_name
                ));
            }
        }

        initialized_vars.insert(*var_id);
    }

    let mut table_access_count = 0;

    // Process each basic block
    for (_bb_idx, &block_id) in hop.blocks.iter().enumerate() {
        let block = &program.basic_blocks[block_id];
        let mut case_body = String::new();

        for inst in &block.instructions {
            lower_instruction(
                &mut case_body,
                program,
                inst,
                &mut state_machine,
                &mut initialized_vars,
                &mut table_access_count,
                "\t\t\t",
            )?;
        }

        lower_terminator(
            &mut case_body,
            program,
            &block.terminator,
            "\t\t\t",
            &liveness,
            block_id,
            &initialized_vars,
            &next_hop_live_in,
        )?;
        case_body.push('\n');
        state_machine.add_case(block_id.index(), case_body);
    }

    state_machine.render(out, entry_block.index())?;

    writeln!(out, "}}")?;
    writeln!(out)?;

    Ok(())
}

pub(super) fn lower_instruction(
    out: &mut String,
    program: &cfg::Program,
    inst: &Instruction,
    state_machine: &mut GoStateMachine,
    initialized_vars: &mut HashSet<cfg::VariableId>,
    table_access_count: &mut usize,
    indent: &str,
) -> Result<(), std::fmt::Error> {
    match &inst.kind {
        InstructionKind::Assign { dest, src } => {
            let dest_name = go_var_name(program, *dest);
            let src_go = operand_to_go(program, src);

            state_machine.ensure_var_decl(*dest, None);
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

            state_machine.ensure_var_decl(*dest, None);
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

            state_machine.ensure_var_decl(*dest, None);
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
                writeln!(out, "{}_, {} := {}", indent, row_var, call)?;

                let field_access = table_field_accessor(program, table_info, *field_id, &row_var);
                state_machine.ensure_var_decl(*dest, None);
                writeln!(out, "{}{} = {}", indent, dest_name, field_access)?;
                initialized_vars.insert(*dest);
            } else {
                // Getting entire row
                state_machine.ensure_var_decl(*dest, None);
                writeln!(out, "{}_, {} = {}", indent, dest_name, call)?;
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
                "{}{}, {} := get{}(tx, {}Key{{ {} }})",
                indent,
                key_var,
                row_var,
                table_name,
                table_name,
                key_args.join(", ")
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
                state_machine.ensure_var_decl(*dest_var, None);
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

pub(super) fn lower_terminator(
    out: &mut String,
    program: &cfg::Program,
    term: &Terminator,
    indent: &str,
    _liveness: &DataflowResults<SetLattice<LiveVar>>,
    _block_id: cfg::BasicBlockId,
    initialized_vars: &HashSet<cfg::VariableId>,
    next_hop_live_in: &[cfg::VariableId],
) -> Result<(), std::fmt::Error> {
    match term {
        Terminator::Jump(target) => {
            writeln!(out, "{}currentState = {}", indent, target.index())?;
            writeln!(out, "{}continue", indent)?;
        }

        Terminator::Branch {
            condition,
            if_true,
            if_false,
        } => {
            let cond_go = operand_to_go(program, condition);
            writeln!(out, "{}if {} {{", indent, cond_go)?;
            writeln!(out, "{}\tcurrentState = {}", indent, if_true.index())?;
            writeln!(out, "{}\tcontinue", indent)?;
            writeln!(out, "{}}} else {{", indent)?;
            writeln!(out, "{}\tcurrentState = {}", indent, if_false.index())?;
            writeln!(out, "{}\tcontinue", indent)?;
            writeln!(out, "{}}}", indent)?;
        }

        Terminator::Return(value) => {
            if let Some(val) = value {
                let result_expr = operand_to_string(program, val);
                writeln!(out, "{}return &proto.TrxRes{{", indent)?;
                writeln!(out, "{}\tStatus: proto.Status_Success,", indent)?;
                writeln!(out, "{}\tInfo:   in.Info,", indent)?;
                writeln!(out, "{}\tResults: []string{{{}}},", indent, result_expr)?;
                writeln!(out, "{}}}, nil", indent)?;
            } else {
                writeln!(out, "{}return &proto.TrxRes{{", indent)?;
                writeln!(out, "{}\tStatus: proto.Status_Success,", indent)?;
                writeln!(out, "{}\tInfo:   in.Info,", indent)?;
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
                writeln!(out, "{}}}, nil", indent)?;
                return Ok(());
            }
            // Fallback: no live-out variables (last hop or no inter-hop dependencies)
            writeln!(out, "{}return &proto.TrxRes{{", indent)?;
            writeln!(out, "{}\tStatus: proto.Status_Success,", indent)?;
            writeln!(out, "{}\tInfo:   in.Info,", indent)?;
            writeln!(out, "{}}}, nil", indent)?;
        }

        Terminator::Abort => {
            writeln!(out, "{}panic(\"transaction aborted\")", indent)?;
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

fn partition_operand_to_u64(program: &cfg::Program, operand: &Operand) -> String {
    match operand {
        Operand::Variable(var_id) => {
            let var_name = go_var_name(program, *var_id);
            format!("toUint64(params[\"{}\"])", var_name)
        }
        Operand::Constant(ConstantValue::Int(value)) => format!("uint64({})", value),
        Operand::Constant(ConstantValue::Float(value)) => format!("uint64({})", value),
        other => panic!("unsupported partition operand: {:?}", other),
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

        // Find the first table access in this hop to determine partition
        let hop = &program.hops[hop_id];
        let mut found_partition = false;

        'block_loop: for &block_id in &hop.blocks {
            let block = &program.basic_blocks[block_id];
            for inst in &block.instructions {
                let (table_id, keys) =
                    if let InstructionKind::TableGet { table, keys, .. } = &inst.kind {
                        (*table, keys)
                    } else if let InstructionKind::TableSet { table, keys, .. } = &inst.kind {
                        (*table, keys)
                    } else {
                        continue;
                    };

                let table_info = &program.tables[table_id];

                if let Some(part_func_id) = table_info.node_partition {
                    let part_func = &program.functions[part_func_id];
                    writeln!(out, "\t\t// Partition using {}", part_func.name)?;

                    let mut key_exprs = HashMap::new();
                    for (field_id, key_operand) in
                        table_info.primary_key_fields.iter().zip(keys.iter())
                    {
                        key_exprs.insert(*field_id, partition_operand_to_u64(program, key_operand));
                    }

                    let mut arg_vars = Vec::new();
                    for (idx, field_id) in table_info.node_partition_args.iter().enumerate() {
                        let expr = key_exprs.get(field_id).cloned().unwrap_or_else(|| {
                            panic!(
                                "missing partition key for field {}",
                                program.table_fields[*field_id].name
                            )
                        });
                        let local_name = format!("partArg{}", idx);
                        writeln!(out, "\t\t{} := {}", local_name, expr)?;
                        arg_vars.push(local_name);
                    }

                    let go_name = go_function_name(program, part_func_id);
                    let call_expr = if arg_vars.is_empty() {
                        format!("{}()", go_name)
                    } else {
                        format!("{}({})", go_name, arg_vars.join(", "))
                    };
                    writeln!(out, "\t\tpartitionValue := {}", call_expr)?;
                    writeln!(out, "\t\tnextShard = int(partitionValue)")?;

                    found_partition = true;
                    break 'block_loop;
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
