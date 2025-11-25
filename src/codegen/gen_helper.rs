//! Shared code generation utilities for Go codegen
//!
//! This module contains common functions used across multiple code generation modules,
//! including operand conversion, terminator lowering, and instruction lowering.

use crate::cfg::{self, BinaryOp, Instruction, InstructionKind, Operand, Terminator, UnaryOp};
use std::collections::{HashMap, HashSet};
use std::fmt::Write;

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

/// Convert a CFG operand to Go code
pub fn operand_to_go(program: &cfg::Program, operand: &Operand) -> String {
    match operand {
        Operand::Variable(var_id) => go_var_name(program, *var_id),
        Operand::Constant(value) => match value {
            cfg::ConstantValue::Int(i) => i.to_string(),
            cfg::ConstantValue::Float(f) => f.to_string(),
            cfg::ConstantValue::Bool(b) => b.to_string(),
            cfg::ConstantValue::String(s) => go_string_literal(s),
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
/// For hop functions, pass Some((initialized_vars, vars_to_write_back))
/// For standalone partition functions, pass None
pub fn lower_terminator_goto(
    out: &mut String,
    program: &cfg::Program,
    term: &Terminator,
    indent: &str,
    hop_context: Option<(&HashSet<cfg::VariableId>, &[cfg::VariableId])>,
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
                    // Transaction partition mode: return 0 as partition
                    if let Some(operand) = operand {
                        let ret_expr = operand_to_go(program, operand);
                        writeln!(out, "{}_ = {}", indent, ret_expr)?;
                    }
                    writeln!(out, "{}return 0", indent)?;
                }
                CodeGenContext::Normal | CodeGenContext::PartitionFunction => {
                    // Standalone partition function: return actual value
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
                if let Some((initialized_vars, vars_to_write_back)) = hop_context {
                    if !vars_to_write_back.is_empty() {
                        let results =
                            generate_results_exprs(program, vars_to_write_back, initialized_vars);
                        writeln!(out, "\t// Unreachable: use variables in results")?;
                        writeln!(out, "\t_ = []string{{{}}}", results.join(", "))?;
                    }
                }
                writeln!(out, "{}panic(\"unexpected hop exit in partition\")", indent)?;
            } else {
                // Collect variables that need to be written back (live-out AND defined in this hop)
                if let Some((initialized_vars, vars_to_write_back)) = hop_context {
                    if !vars_to_write_back.is_empty() {
                        writeln!(out, "{}// Write back variables to results", indent)?;
                        let results =
                            generate_results_exprs(program, vars_to_write_back, initialized_vars);
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
                    writeln!(out, "{}return &proto.TrxRes{{", indent)?;
                    writeln!(out, "{}\tStatus: proto.Status_Success,", indent)?;
                    writeln!(out, "{}\tInfo:   in.Info,", indent)?;
                    writeln!(out, "{}\tRWSets: rwSet,", indent)?;
                    writeln!(out, "{}}}, nil", indent)?;
                }
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
            format!("params[\"{}\"]", var_name)
        };
        results.push(expr);
    }
    results
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
            let source_expr = operand_to_go(program, src);
            writeln!(out, "{}{} = {}", indent, dest_name, source_expr)?;
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

        InstructionKind::Call { dest, func, args } => {
            let func_name = go_function_name(program, *func);
            let arg_exprs: Vec<String> =
                args.iter().map(|arg| operand_to_go(program, arg)).collect();

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
                let value_expr = operand_to_go(program, value);
                writeln!(out, "{}{} = {}", indent, field_accessor, value_expr)?;
            } else {
                // Write whole row
                let value_expr = operand_to_go(program, value);
                writeln!(out, "{}{} = {}", indent, row_var, value_expr)?;
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

        _ => {
            // Skip other instruction types
        }
    }

    Ok(())
}
