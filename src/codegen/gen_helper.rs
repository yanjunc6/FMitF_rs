//! Shared code generation utilities for Go codegen
//!
//! This module contains common functions used across multiple code generation modules,
//! including operand conversion, terminator lowering, and instruction lowering.

use crate::cfg::{self, BinaryOp, Instruction, InstructionKind, Operand, Terminator, UnaryOp};
use std::collections::{HashMap, HashSet};
use std::fmt::Write;

use super::gen_table_optimizer::{OptimizedOp, TableAccess, TableAccessGroup};
use super::util::{go_type_string, go_var_name, pascal_case};

/// Context for code generation - controls the code generation mode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CodeGenContext {
    /// Normal transaction hop execution
    Normal,
    /// Partition calculation for transaction hops (returns 0 on Return)
    TransactionPartition,
    /// Standalone partition function (returns actual value on Return)
    PartitionFunction,
}

impl CodeGenContext {
    pub fn is_partition(&self) -> bool {
        matches!(
            self,
            CodeGenContext::TransactionPartition | CodeGenContext::PartitionFunction
        )
    }
}

/// Context for hop execution including variables and tables
pub struct HopContext<'a> {
    pub initialized_vars: &'a HashSet<cfg::VariableId>,
    pub vars_to_write_back: &'a [cfg::VariableId],
    pub tables_written: &'a HashSet<cfg::TableId>,
}

/// Convert a CFG operand to Go code
pub fn operand_to_go(program: &cfg::Program, operand: &Operand) -> String {
    match operand {
        Operand::Variable(var_id) => go_var_name(program, *var_id),
        Operand::Constant(value) => match value {
            cfg::ConstantValue::Int(i) => i.to_string(),
            cfg::ConstantValue::Float(f) => f.to_string(),
            cfg::ConstantValue::Bool(b) => b.to_string(),
            cfg::ConstantValue::String(s) => go_string_literal(s),
            cfg::ConstantValue::Null => "nil".to_string(),
        },
        Operand::Global(global_id) => {
            let global = &program.global_consts[*global_id];
            global.name.clone()
        }
        Operand::Table(_) => "/* table operand */".to_string(),
    }
}

/// Convert a binary operator to Go syntax
pub fn binary_op_to_go(op: BinaryOp) -> &'static str {
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

/// Convert a unary operator to Go syntax
pub fn unary_op_to_go(op: UnaryOp) -> &'static str {
    match op {
        UnaryOp::NegInt | UnaryOp::NegFloat => "-",
        UnaryOp::NotBool => "!",
    }
}

/// Generate the Go function name for a given function ID.
/// Handles three types of renaming:
/// 1. @go decorator: applies built-in renames (length->len, float->float32, int->uint64, get->listGet)
/// 2. @rename decorator: appends function ID suffix to avoid conflicts (e.g., get_id_28)
/// 3. Default: uses the function name as-is
pub fn go_function_name(program: &cfg::Program, func_id: cfg::FunctionId) -> String {
    let function = &program.functions[func_id];
    let func_name = &function.name;

    // Check for @go decorator (applies built-in renames)
    let has_go_decorator = function
        .decorators
        .iter()
        .any(|decorator| decorator.name == "go");

    // Check for @rename decorator (adds function ID suffix)
    let has_rename_decorator = function
        .decorators
        .iter()
        .any(|decorator| decorator.name == "rename");

    // Apply built-in renames if @go decorator is present
    let base_name = if has_go_decorator {
        go_function_rename(func_name)
            .unwrap_or(func_name)
            .to_string()
    } else {
        func_name.to_string()
    };

    // Add function ID suffix if @rename decorator is present
    if has_rename_decorator {
        format!("{}_{}", base_name, func_id.index())
    } else {
        base_name
    }
}

const GO_FUNCTION_RENAMES: &[(&str, &str)] = &[
    ("length", "len"),
    ("float", "float32"),
    ("int", "uint64"),
    ("get", "listGet"),
];

fn go_function_rename(name: &str) -> Option<&'static str> {
    GO_FUNCTION_RENAMES
        .iter()
        .find(|(from, _)| *from == name)
        .map(|(_, to)| *to)
}

pub fn go_var_to_string(program: &cfg::Program, var_id: cfg::VariableId) -> String {
    let expr = go_var_name(program, var_id);
    let ty_id = program.variables[var_id].ty;
    go_value_to_string(program, ty_id, &expr)
}

fn go_value_to_string(program: &cfg::Program, ty_id: cfg::TypeId, expr: &str) -> String {
    match &program.types[ty_id] {
        cfg::Type::Primitive(cfg::PrimitiveType::String) => expr.to_string(),
        _ => format!("string(mustJSON({}))", expr),
    }
}

pub fn operand_to_string(program: &cfg::Program, operand: &Operand) -> String {
    match operand {
        Operand::Variable(var_id) => go_var_to_string(program, *var_id),
        Operand::Constant(value) => match value {
            cfg::ConstantValue::Int(i) => format!("strconv.FormatUint({}, 10)", i),
            cfg::ConstantValue::Float(f) => format!("strconv.FormatFloat({}, 'f', -1, 32)", f),
            cfg::ConstantValue::Bool(b) => format!("strconv.FormatBool({})", b),
            cfg::ConstantValue::String(s) => go_string_literal(s),
            cfg::ConstantValue::Null => "\"null\"".to_string(),
        },
        Operand::Global(global_id) => {
            let global = &program.global_consts[*global_id];
            let expr = &global.name;
            go_value_to_string(program, global.ty, expr)
        }
        Operand::Table(_) => "/* table */".to_string(),
    }
}

pub fn go_string_literal(value: &str) -> String {
    format!("\"{}\"", value.escape_default())
}

pub fn table_field_accessor(
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

/// Collect variable declarations from an instruction
pub fn collect_var_decls_from_instruction(
    program: &cfg::Program,
    inst: &Instruction,
    var_decls: &mut HashMap<cfg::VariableId, String>,
) {
    match &inst.kind {
        InstructionKind::Assign { dest, .. }
        | InstructionKind::BinaryOp { dest, .. }
        | InstructionKind::UnaryOp { dest, .. }
        | InstructionKind::TableGet { dest, .. }
        | InstructionKind::Call {
            dest: Some(dest), ..
        } => {
            let var = &program.variables[*dest];
            let type_str = go_type_string(program, var.ty);
            var_decls.insert(*dest, type_str);
        }
        _ => {}
    }
}

/// Collect labels that are actually used (targets of goto statements)
pub fn collect_used_labels(term: &Terminator, used_labels: &mut HashSet<cfg::BasicBlockId>) {
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

/// Lower terminator - works for all three contexts: Normal, TransactionPartition, PartitionFunction
/// For hop functions, pass Some(HopContext)
/// For standalone partition functions, pass None
pub fn lower_terminator_goto(
    out: &mut String,
    program: &cfg::Program,
    term: &Terminator,
    indent: &str,
    hop_context: Option<HopContext>,
    ctx: CodeGenContext,
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
            let cond_expr = operand_to_go(program, condition);
            writeln!(out, "{}if {} {{", indent, cond_expr)?;
            writeln!(out, "{}\tgoto BB{}", indent, if_true.index())?;
            writeln!(out, "{}}} else {{", indent)?;
            writeln!(out, "{}\tgoto BB{}", indent, if_false.index())?;
            writeln!(out, "{}}}", indent)?;
        }

        Terminator::Return(operand) => {
            match ctx {
                CodeGenContext::TransactionPartition => {
                    // Transaction partition mode: suppress unused and return 0
                    if let Some(operand) = operand {
                        let ret_expr = operand_to_go(program, operand);
                        writeln!(out, "{}_ = {}", indent, ret_expr)?;
                    }
                    writeln!(out, "{}return 0", indent)?;
                }
                CodeGenContext::Normal => {
                    // Normal mode: doing nothing
                    writeln!(out, "{}// return - no action", indent)?;
                    generate_trx_res_return(out, program, hop_context, indent)?;
                }
                CodeGenContext::PartitionFunction => {
                    // Partition function: return the actual value
                    if let Some(operand) = operand {
                        let ret_expr = operand_to_go(program, operand);
                        writeln!(out, "{}return {}", indent, ret_expr)?;
                    } else {
                        writeln!(out, "{}return", indent)?;
                    }
                }
            }
        }

        Terminator::HopExit { .. } => {
            if ctx.is_partition() {
                // For partition mode, generate unreachable results assignment at the end
                if let Some(ref ctx) = hop_context {
                    if !ctx.vars_to_write_back.is_empty() {
                        let results = generate_results_exprs(
                            program,
                            ctx.vars_to_write_back,
                            ctx.initialized_vars,
                        );
                        writeln!(out, "\t// Unreachable: use variables in results")?;
                        writeln!(out, "\t_ = []string{{{}}}", results.join(", "))?;
                    }
                }
                writeln!(out, "{}panic(\"unexpected hop exit in partition\")", indent)?;
            } else {
                generate_trx_res_return(out, program, hop_context, indent)?;
            }
        }

        Terminator::Abort => {
            writeln!(out, "{}panic(\"abort\")", indent)?;
        }
    }

    Ok(())
}

/// Helper function to generate result expressions for variables to write back
fn generate_results_exprs(
    program: &cfg::Program,
    vars_to_write_back: &[cfg::VariableId],
    initialized_vars: &HashSet<cfg::VariableId>,
) -> Vec<String> {
    let mut results = Vec::new();
    for var_id in vars_to_write_back {
        let var_name = go_var_name(program, *var_id);
        let expr = if initialized_vars.contains(var_id) {
            go_var_to_string(program, *var_id)
        } else {
            format!("params[\"{}\"])", var_name)
        };
        results.push(expr);
    }
    results
}

/// Helper function to generate a TrxRes return statement
/// Used in both normal hop exit and transaction returns
pub fn generate_trx_res_return(
    out: &mut String,
    program: &cfg::Program,
    hop_context: Option<HopContext>,
    indent: &str,
) -> Result<(), std::fmt::Error> {
    // Flush caches for tables written in this hop
    if let Some(ref ctx) = hop_context {
        if !ctx.tables_written.is_empty() {
            writeln!(
                out,
                "{}// Flush caches for tables written in this hop",
                indent
            )?;
            for table_id in ctx.tables_written {
                let table = &program.tables[*table_id];
                let table_name = pascal_case(&table.name);
                writeln!(out, "{}flush{}Cache(tx)", indent, table_name)?;
            }
        }
    }

    // Collect variables that need to be written back (live-out AND defined in this hop)
    if let Some(ctx) = hop_context {
        if !ctx.vars_to_write_back.is_empty() {
            writeln!(out, "{}// Write back variables to results", indent)?;
            let results =
                generate_results_exprs(program, ctx.vars_to_write_back, ctx.initialized_vars);
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
        } else {
            writeln!(out, "{}return &proto.TrxRes{{", indent)?;
            writeln!(out, "{}\tStatus: proto.Status_Success,", indent)?;
            writeln!(out, "{}\tInfo:   in.Info,", indent)?;
            writeln!(out, "{}\tRWSets: rwSet,", indent)?;
            writeln!(out, "{}}}, nil", indent)?;
        }
    } else {
        // No hop context - generate basic return
        writeln!(out, "{}return &proto.TrxRes{{", indent)?;
        writeln!(out, "{}\tStatus: proto.Status_Success,", indent)?;
        writeln!(out, "{}\tInfo:   in.Info,", indent)?;
        writeln!(out, "{}\tRWSets: rwSet,", indent)?;
        writeln!(out, "{}}}, nil", indent)?;
    }

    Ok(())
}

/// Lower a single instruction to Go code
pub fn lower_instruction(
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
            if matches!(src, Operand::Constant(cfg::ConstantValue::Null)) {
                writeln!(
                    out,
                    "{}// TODO: handle null deletion for {}",
                    indent, dest_name
                )?;
            } else {
                let source_expr = operand_to_go(program, src);
                writeln!(out, "{}{} = {}", indent, dest_name, source_expr)?;
            }
            initialized_vars.insert(*dest);
        }

        InstructionKind::BinaryOp {
            dest,
            op,
            left,
            right,
        } => {
            let dest_name = go_var_name(program, *dest);
            let left_expr = operand_to_go(program, left);
            let right_expr = operand_to_go(program, right);
            let op_str = binary_op_to_go(*op);
            writeln!(
                out,
                "{}{} = {} {} {}",
                indent, dest_name, left_expr, op_str, right_expr
            )?;
            initialized_vars.insert(*dest);
        }

        InstructionKind::UnaryOp { dest, op, operand } => {
            let dest_name = go_var_name(program, *dest);
            let operand_expr = operand_to_go(program, operand);
            let op_str = unary_op_to_go(*op);
            writeln!(out, "{}{} = {}{}", indent, dest_name, op_str, operand_expr)?;
            initialized_vars.insert(*dest);
        }

        InstructionKind::Assert { condition, message } => {
            let cond_expr = operand_to_go(program, condition);
            let msg = if message.is_empty() {
                "assertion failed".to_string()
            } else {
                message.clone()
            };
            writeln!(out, "{}if !({}) {{", indent, cond_expr)?;
            writeln!(out, "{}\tpanic(\"{}\")", indent, msg.escape_default())?;
            writeln!(out, "{}}}", indent)?;
        }

        InstructionKind::Call { dest, func, args } => {
            let function = &program.functions[*func];
            let func_name = go_function_name(program, *func);
            let arg_exprs: Vec<String> =
                args.iter().map(|arg| operand_to_go(program, arg)).collect();

            // Special handling for built-in functions
            match function.name.as_str() {
                "to_unit" | "to_unit_return" => {
                    // to_unit and to_unit_return consume their argument and return unit.
                    if !arg_exprs.is_empty() {
                        writeln!(out, "{}_ = {}", indent, arg_exprs[0])?;
                    }
                }
                "scan" => {
                    // scan(table, n, m) returns an iterator
                    if let Some(dest) = dest {
                        let dest_name = go_var_name(program, *dest);
                        // args[0] is the table (Table operand)
                        // args[1] is n (int)
                        // args[2] is m (int)
                        if let Operand::Table(table_id) = &args[0] {
                            let table_name = &program.tables[*table_id].name;
                            let n_expr = operand_to_go(program, &args[1]);
                            let m_expr = operand_to_go(program, &args[2]);
                            writeln!(
                                out,
                                "{}{} = scan(tx, \"t{}\", int({}), int({}))",
                                indent, dest_name, table_name, n_expr, m_expr
                            )?;
                        }
                        initialized_vars.insert(*dest);
                    }
                }
                "hasNext" => {
                    // hasNext(iterator) returns bool
                    if let Some(dest) = dest {
                        let dest_name = go_var_name(program, *dest);
                        writeln!(
                            out,
                            "{}{} = {}({})",
                            indent,
                            dest_name,
                            func_name,
                            arg_exprs.join(", ")
                        )?;
                        initialized_vars.insert(*dest);
                    }
                }
                "next" => {
                    // next(iterator) returns the next element
                    if let Some(dest) = dest {
                        let dest_name = go_var_name(program, *dest);
                        writeln!(
                            out,
                            "{}{} = {}({})",
                            indent,
                            dest_name,
                            func_name,
                            arg_exprs.join(", ")
                        )?;
                        initialized_vars.insert(*dest);
                    }
                }
                "str" => {
                    // Convert to string - use operand_to_string for proper conversion
                    if let Some(dest) = dest {
                        let dest_name = go_var_name(program, *dest);
                        let str_expr = if !args.is_empty() {
                            operand_to_string(program, &args[0])
                        } else {
                            "\"\"".to_string()
                        };
                        writeln!(out, "{}{} = {}", indent, dest_name, str_expr)?;
                        initialized_vars.insert(*dest);
                    }
                }
                "append" => {
                    // Append to list
                    if let Some(dest) = dest {
                        let dest_name = go_var_name(program, *dest);
                        writeln!(
                            out,
                            "{}{} = append({})",
                            indent,
                            dest_name,
                            arg_exprs.join(", ")
                        )?;
                        initialized_vars.insert(*dest);
                    }
                }
                "emptyList" => {
                    // Create an empty list
                    if let Some(dest) = dest {
                        let dest_name = go_var_name(program, *dest);
                        writeln!(out, "{}{} = nil", indent, dest_name)?;
                    }
                }
                "+" => {
                    // String concatenation or list concatenation
                    if let Some(dest) = dest {
                        let dest_name = go_var_name(program, *dest);
                        // Check if this is string concatenation
                        if args.len() == 2 {
                            let dest_ty = program.variables[*dest].ty;
                            let is_string = matches!(
                                &program.types[dest_ty],
                                crate::cfg::Type::Primitive(crate::cfg::PrimitiveType::String)
                            );
                            if is_string && !arg_exprs.is_empty() {
                                writeln!(
                                    out,
                                    "{}{} = concat({}, {})",
                                    indent, dest_name, arg_exprs[0], arg_exprs[1]
                                )?;
                            } else {
                                // List concatenation or other types
                                writeln!(
                                    out,
                                    "{}{} = {}({})",
                                    indent,
                                    dest_name,
                                    func_name,
                                    arg_exprs.join(", ")
                                )?;
                            }
                        } else {
                            writeln!(
                                out,
                                "{}{} = {}({})",
                                indent,
                                dest_name,
                                func_name,
                                arg_exprs.join(", ")
                            )?;
                        }
                        initialized_vars.insert(*dest);
                    }
                }
                _ => {
                    // Default function call handling
                    if let Some(dest) = dest {
                        let dest_name = go_var_name(program, *dest);
                        writeln!(
                            out,
                            "{}{} = {}({})",
                            indent,
                            dest_name,
                            func_name,
                            arg_exprs.join(", ")
                        )?;
                        initialized_vars.insert(*dest);
                    } else {
                        writeln!(out, "{}{}({})", indent, func_name, arg_exprs.join(", "))?;
                    }
                }
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

            writeln!(out, "{}// TableGet: {}", indent, table_name)?;

            let key_args: Vec<String> = table_info
                .primary_key_fields
                .iter()
                .zip(keys.iter())
                .map(|(field_id, key_operand)| {
                    let field_info = &program.table_fields[*field_id];
                    let key_expr = operand_to_go(program, key_operand);
                    format!("{}: {}", field_info.name, key_expr)
                })
                .collect();

            *table_access_count += 1;
            let key_var = format!("keyBytes{}", table_access_count);
            let row_var = format!("row{}", table_access_count);

            writeln!(
                out,
                "{}{}, {} = get{}(tx, {}Key{{{}}})",
                indent,
                key_var,
                row_var,
                table_name,
                table_name,
                key_args.join(", ")
            )?;
            writeln!(
                out,
                "{}rwSet = AddRWSet(rwSet, \"{}\", {})",
                indent, table_name, key_var
            )?;

            let dest_name = go_var_name(program, *dest);
            if let Some(field_id) = field {
                let field_accessor = table_field_accessor(program, table_info, *field_id, &row_var);
                writeln!(out, "{}{} = {}", indent, dest_name, field_accessor)?;
            } else {
                // Read whole row
                writeln!(out, "{}{} = {}", indent, dest_name, row_var)?;
            }
            initialized_vars.insert(*dest);
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
                    let field_info = &program.table_fields[*field_id];
                    let key_expr = operand_to_go(program, key_operand);
                    format!("{}: {}", field_info.name, key_expr)
                })
                .collect();

            *table_access_count += 1;
            let key_var = format!("keyBytes{}", table_access_count);
            let row_var = format!("row{}", table_access_count);

            writeln!(
                out,
                "{}{}, {} = get{}(tx, {}Key{{{}}})",
                indent,
                key_var,
                row_var,
                table_name,
                table_name,
                key_args.join(", ")
            )?;
            writeln!(
                out,
                "{}rwSet = AddRWSet(rwSet, \"{}\", {})",
                indent, table_name, key_var
            )?;

            if let Some(field_id) = field {
                let field_accessor = table_field_accessor(program, table_info, *field_id, &row_var);
                if matches!(value, Operand::Constant(cfg::ConstantValue::Null)) {
                    writeln!(
                        out,
                        "{}// TODO: handle null deletion for {}",
                        indent, field_accessor
                    )?;
                } else {
                    let value_expr = operand_to_go(program, value);
                    writeln!(out, "{}{} = {}", indent, field_accessor, value_expr)?;
                }
            } else {
                // Write whole row
                if matches!(value, Operand::Constant(cfg::ConstantValue::Null)) {
                    writeln!(
                        out,
                        "{}// TODO: handle null deletion for {}",
                        indent, row_var
                    )?;
                } else {
                    let value_expr = operand_to_go(program, value);
                    writeln!(out, "{}{} = {}", indent, row_var, value_expr)?;
                }
            }

            writeln!(
                out,
                "{}put{}(tx, {}Key{{{}}}, {})",
                indent,
                table_name,
                table_name,
                key_args.join(", "),
                row_var
            )?;
        }
    }

    Ok(())
}

/// Lower a single optimized operation (instruction or combined table access)
pub fn lower_optimized_op(
    out: &mut String,
    program: &cfg::Program,
    op: &OptimizedOp,
    initialized_vars: &mut HashSet<cfg::VariableId>,
    table_access_count: &mut usize,
    indent: &str,
) -> Result<(), std::fmt::Error> {
    match op {
        OptimizedOp::Single(inst) => {
            lower_instruction(
                out,
                program,
                inst,
                initialized_vars,
                table_access_count,
                indent,
            )?;
        }
        OptimizedOp::CombinedTableAccess(group) => {
            lower_combined_table_access(
                out,
                program,
                group,
                initialized_vars,
                table_access_count,
                indent,
            )?;
        }
    }
    Ok(())
}

/// Lower a sequence of optimized operations
pub fn lower_optimized_ops(
    out: &mut String,
    program: &cfg::Program,
    ops: &[OptimizedOp],
    initialized_vars: &mut HashSet<cfg::VariableId>,
    table_access_count: &mut usize,
    indent: &str,
) -> Result<(), std::fmt::Error> {
    for op in ops {
        lower_optimized_op(
            out,
            program,
            op,
            initialized_vars,
            table_access_count,
            indent,
        )?;
    }
    Ok(())
}

/// Lower a combined table access group (multiple gets/sets to the same key)
pub fn lower_combined_table_access(
    out: &mut String,
    program: &cfg::Program,
    group: &TableAccessGroup,
    initialized_vars: &mut HashSet<cfg::VariableId>,
    table_access_count: &mut usize,
    indent: &str,
) -> Result<(), std::fmt::Error> {
    let table_info = &program.tables[group.table_id];
    let table_name = pascal_case(&table_info.name);

    writeln!(
        out,
        "{}// Combined table access: {} ({} operations)",
        indent,
        table_name,
        group.accesses.len()
    )?;

    let key_args: Vec<String> = table_info
        .primary_key_fields
        .iter()
        .zip(group.keys.iter())
        .map(|(field_id, key_operand)| {
            let field_info = &program.table_fields[*field_id];
            let key_expr = operand_to_go(program, key_operand);
            format!("{}: {}", field_info.name, key_expr)
        })
        .collect();

    *table_access_count += 1;
    let key_var = format!("keyBytes{}", table_access_count);
    let row_var = format!("row{}", table_access_count);

    // Fetch the row once
    writeln!(
        out,
        "{}{}, {} = get{}(tx, {}Key{{{}}})",
        indent,
        key_var,
        row_var,
        table_name,
        table_name,
        key_args.join(", ")
    )?;
    writeln!(
        out,
        "{}rwSet = AddRWSet(rwSet, \"{}\", {})",
        indent, table_name, key_var
    )?;

    // Process all accesses
    for access in &group.accesses {
        match access {
            TableAccess::Get { dest, field, .. } => {
                let dest_name = go_var_name(program, *dest);
                if let Some(field_id) = field {
                    let field_accessor =
                        table_field_accessor(program, table_info, *field_id, &row_var);
                    writeln!(out, "{}{} = {}", indent, dest_name, field_accessor)?;
                } else {
                    // Read whole row
                    writeln!(out, "{}{} = {}", indent, dest_name, row_var)?;
                }
                initialized_vars.insert(*dest);
            }
            TableAccess::Set { field, value, .. } => {
                if let Some(field_id) = field {
                    let field_accessor =
                        table_field_accessor(program, table_info, *field_id, &row_var);
                    let value_expr = operand_to_go(program, value);
                    writeln!(out, "{}{} = {}", indent, field_accessor, value_expr)?;
                } else {
                    // Write whole row
                    let value_expr = operand_to_go(program, value);
                    writeln!(out, "{}{} = {}", indent, row_var, value_expr)?;
                }
            }
        }
    }

    // Write back if there were any writes
    if group.has_writes {
        writeln!(
            out,
            "{}put{}(tx, {}Key{{{}}}, {})",
            indent,
            table_name,
            table_name,
            key_args.join(", "),
            row_var
        )?;
    }

    Ok(())
}
