use crate::ast::{ReturnType, Span, TypeName};
use crate::cfg::*;
use std::io::{Result, Write};

fn escape_dot_label(s: &str) -> String {
    s.replace("\n", "\\n")
        .replace("\"", "\\\"")
        .replace("{", "\\{")
        .replace("}", "\\}")
}

fn format_span_str(span: &Span, show_spans: bool) -> String {
    if show_spans {
        format!(" @{}:{}", span.line, span.column)
    } else {
        String::new()
    }
}

fn format_operand(operand: &Operand, function: &FunctionCfg) -> String {
    match operand {
        Operand::Var(var_id) => {
            let var = &function.variables[*var_id];
            format!("%{}", var.name)
        }
        Operand::Const(c) => match c {
            Constant::Int(i) => i.to_string(),
            Constant::Float(f) => f.to_string(),
            Constant::Bool(b) => b.to_string(),
            Constant::String(s) => format!("\"{}\"", escape_dot_label(s)),
        },
    }
}

fn format_rvalue(rvalue: &Rvalue, function: &FunctionCfg, program: &CfgProgram) -> String {
    match rvalue {
        Rvalue::Use(op) => format_operand(op, function),
        Rvalue::TableAccess {
            table,
            pk_fields,
            pk_values,
            field,
        } => {
            let table_info = &program.tables[*table];
            let field_info = &program.fields[*field];
            
            // Format composite primary keys
            let pk_parts: Vec<String> = pk_fields.iter().zip(pk_values.iter())
                .map(|(pk_field_id, pk_value)| {
                    let pk_field_info = &program.fields[*pk_field_id];
                    format!("{}:{}", pk_field_info.name, format_operand(pk_value, function))
                })
                .collect();
            
            format!(
                "{}[{}].{}",
                table_info.name,
                pk_parts.join(", "),
                field_info.name
            )
        }
        Rvalue::UnaryOp { op, operand } => {
            format!("{:?} {}", op, format_operand(operand, function))
        }
        Rvalue::BinaryOp { op, left, right } => {
            format!(
                "{} {:?} {}",
                format_operand(left, function),
                op,
                format_operand(right, function)
            )
        }
    }
}

fn format_statement(
    stmt: &Statement,
    function: &FunctionCfg,
    program: &CfgProgram,
    show_spans: bool,
) -> String {
    let stmt_str = match stmt {
        Statement::Assign { var, rvalue, .. } => {
            let variable = &function.variables[*var];
            format!(
                "%{} = {}",
                variable.name,
                format_rvalue(rvalue, function, program)
            )
        }
        Statement::TableAssign {
            table,
            pk_fields,
            pk_values,
            field,
            value,
            ..
        } => {
            let table_info = &program.tables[*table];
            let field_info = &program.fields[*field];
            
            // Format composite primary keys
            let pk_parts: Vec<String> = pk_fields.iter().zip(pk_values.iter())
                .map(|(pk_field_id, pk_value)| {
                    let pk_field_info = &program.fields[*pk_field_id];
                    format!("{}:{}", pk_field_info.name, format_operand(pk_value, function))
                })
                .collect();
            
            format!(
                "{}[{}].{} = {}",
                table_info.name,
                pk_parts.join(", "),
                field_info.name,
                format_operand(value, function)
            )
        }
    };
    let span_val = match stmt {
        Statement::Assign { span, .. } => span,
        Statement::TableAssign { span, .. } => span,
    };
    format!("{}{}", stmt_str, format_span_str(span_val, show_spans))
}

fn format_terminator(terminator: &Terminator, function: &FunctionCfg) -> String {
    match terminator {
        Terminator::Goto(block_id) => format!("goto bb{}", block_id.index()),
        Terminator::Branch {
            condition,
            then_block,
            else_block,
        } => {
            format!(
                "if {} then bb{} else bb{}",
                format_operand(condition, function),
                then_block.index(),
                else_block.index()
            )
        }
        Terminator::Return(Some(op)) => format!("return {}", format_operand(op, function)),
        Terminator::Return(None) => "return".to_string(),
        Terminator::Abort => "abort".to_string(),
        Terminator::HopExit { next_hop } => match next_hop {
            Some(hop_id) => format!("hop_exit -> hop{}", hop_id.index()),
            None => "hop_exit -> function_end".to_string(),
        },
    }
}

pub fn print_cfg_dot_format(
    program: &CfgProgram,
    writer: &mut impl Write,
    show_spans: bool,
) -> Result<()> {
    writeln!(writer, "digraph CFG {{")?;
    writeln!(writer, "  compound=true;")?;
    writeln!(writer, "  node [shape=box, style=rounded];")?;
    writeln!(writer, "")?;

    for (func_id, function) in program.functions.iter() {
        writeln!(writer, "  subgraph cluster_func_{} {{", func_id.index())?;
        let func_span_str = format_span_str(&function.span, show_spans);
        writeln!(
            writer,
            "    label=\"Function: {}{}\";",
            escape_dot_label(&function.name),
            func_span_str
        )?;
        writeln!(writer, "    style=filled;")?;
        writeln!(writer, "    color=lightgrey;")?;
        writeln!(writer, "")?;

        let mut hop_to_blocks: std::collections::HashMap<HopId, Vec<BasicBlockId>> =
            std::collections::HashMap::new();

        for (block_id, block) in function.blocks.iter() {
            hop_to_blocks
                .entry(block.hop_id)
                .or_default()
                .push(block_id);
        }

        for (hop_id, blocks_in_hop) in &hop_to_blocks {
            writeln!(
                writer,
                "    subgraph cluster_func_{}_hop_{} {{",
                func_id.index(),
                hop_id.index()
            )?;
            let hop = &function.hops[*hop_id];
            let node_name = &program.nodes[hop.node_id].name;
            let hop_span_str = format_span_str(&hop.span, show_spans);
            writeln!(
                writer,
                "      label=\"Hop {} on Node {}{}\";",
                hop_id.index(),
                escape_dot_label(node_name),
                hop_span_str
            )?;
            writeln!(writer, "      style=dotted;")?;

            for &block_id in blocks_in_hop {
                let block = &function.blocks[block_id];
                let block_span_str = format_span_str(&block.span, show_spans);
                let mut label = format!(
                    "BB{} (Hop {}){}\n",
                    block_id.index(),
                    hop_id.index(),
                    block_span_str
                );

                for stmt in &block.statements {
                    label.push_str(&format!(
                        "{}\n",
                        format_statement(stmt, function, program, show_spans)
                    ));
                }

                label.push_str(&format!(
                    "Term: {}",
                    format_terminator(&block.terminator, function)
                ));

                writeln!(
                    writer,
                    "      f{}_bb{} [label=\"{}\"];",
                    func_id.index(),
                    block_id.index(),
                    escape_dot_label(&label)
                )?;
            }
            writeln!(writer, "    }}")?;
        }
        writeln!(writer, "")?;

        for (block_id, block) in function.blocks.iter() {
            let source_node = format!("f{}_bb{}", func_id.index(), block_id.index());

            match &block.terminator {
                Terminator::Goto(target_bb) => {
                    writeln!(
                        writer,
                        "    {} -> f{}_bb{};",
                        source_node,
                        func_id.index(),
                        target_bb.index()
                    )?;
                }
                Terminator::Branch {
                    condition,
                    then_block,
                    else_block,
                } => {
                    let cond_str = format_operand(condition, function);
                    writeln!(
                        writer,
                        "    {} -> f{}_bb{} [label=\"{}\"];",
                        source_node,
                        func_id.index(),
                        then_block.index(),
                        escape_dot_label(&format!("{} is true", cond_str))
                    )?;
                    writeln!(
                        writer,
                        "    {} -> f{}_bb{} [label=\"{}\"];",
                        source_node,
                        func_id.index(),
                        else_block.index(),
                        escape_dot_label(&format!("{} is false", cond_str))
                    )?;
                }
                Terminator::HopExit {
                    next_hop: Some(next_hop_id),
                } => {
                    let next_hop_entry = function.hops[*next_hop_id].entry_block.unwrap(); // Use unwrap()
                    writeln!(
                        writer,
                        "    {} -> f{}_bb{} [label=\"To Hop {}\", style=dashed];",
                        source_node,
                        func_id.index(),
                        next_hop_entry.index(),
                        next_hop_id.index()
                    )?;
                }
                Terminator::Return(_)
                | Terminator::Abort
                | Terminator::HopExit { next_hop: None } => {
                    // No outgoing edges for these terminators in the dot graph
                }
            }
        }
        writeln!(writer, "  }}")?;
        writeln!(writer, "")?;
    }

    writeln!(writer, "}}")?;
    Ok(())
}

pub fn format_cfg_text(program: &CfgProgram, options: &CfgPrintOptions) -> String {
    if options.quiet && options.format == CfgFormat::Text {
        // Only return quiet message for text format
        return String::from("CFG generation complete");
    }

    let mut s = String::new();
    s.push_str("Control Flow Graph (CFG):\n");
    s.push_str("===========================\n\n");

    for (func_id, function) in program.functions.iter() {
        let func_span_str = format_span_str(&function.span, options.show_spans);
        s.push_str(&format!(
            "Function {}: {} (",
            func_id.index(),
            function.name
        ));

        // Print parameters
        for (i, &param_id) in function.parameters.iter().enumerate() {
            let param = &function.variables[param_id];
            s.push_str(&format!("{} {}", type_name(&param.ty), param.name));
            if i < function.parameters.len() - 1 {
                s.push_str(", ");
            }
        }
        s.push_str(&format!(
            ") -> {}{}\n",
            return_type_name(&function.return_type),
            func_span_str
        ));

        s.push_str(&format!(
            "  Entry Hop: hop{}\n",
            function.entry_hop.unwrap().index()
        )); // Use unwrap()

        if options.verbose {
            s.push_str("  Variables:\n");
            for (var_id, var) in function.variables.iter() {
                let param_str = if var.is_parameter { " (param)" } else { "" };
                s.push_str(&format!(
                    "    Var {} ({}): {} {}{}\n",
                    var_id.index(),
                    var.name,
                    type_name(&var.ty),
                    var.name,
                    param_str
                ));
            }
            s.push_str("\n");

            s.push_str("  Hops:\n");
            for (hop_id, hop) in function.hops.iter() {
                let node_name = &program.nodes[hop.node_id].name;
                let hop_span_str = format_span_str(&hop.span, options.show_spans);
                s.push_str(&format!(
                    "    Hop {} on Node {}{}\n",
                    hop_id.index(),
                    node_name,
                    hop_span_str
                ));
                s.push_str(&format!(
                    "      Entry Block: BB{}\n",
                    hop.entry_block.unwrap().index()
                )); // Use unwrap()
                s.push_str(&format!(
                    "      Blocks: {:?}\n",
                    hop.blocks.iter().map(|b| b.index()).collect::<Vec<_>>()
                ));
            }
            s.push_str("\n");

            s.push_str("  Basic Blocks:\n");
            for (block_id, block) in function.blocks.iter() {
                let block_span_str = format_span_str(&block.span, options.show_spans);
                s.push_str(&format!(
                    "    BB{} (Hop {}):{}\n",
                    block_id.index(),
                    block.hop_id.index(),
                    block_span_str
                ));

                for stmt in &block.statements {
                    s.push_str(&format!(
                        "      {}\n",
                        format_statement(stmt, function, program, options.show_spans)
                    ));
                }

                s.push_str(&format!(
                    "      Terminator: {}\n",
                    format_terminator(&block.terminator, function)
                ));
            }
        } else {
            // Not verbose
            s.push_str(&format!("  Number of Hops: {}\n", function.hops.len()));
            s.push_str(&format!(
                "  Number of Basic Blocks: {}\n",
                function.blocks.len()
            ));
            s.push_str(&format!(
                "  Number of Variables: {}\n",
                function.variables.len()
            ));
        }
        s.push_str("\n---------------------------\n\n");
    }
    s
}

pub fn print_cfg_summary(program: &CfgProgram) -> String {
    let mut s = String::new();
    s.push_str("CFG Summary:\n");
    s.push_str("============\n\n");

    s.push_str(&format!("Total Functions: {}\n", program.functions.len()));
    s.push_str(&format!("Total Nodes: {}\n", program.nodes.len()));
    s.push_str(&format!("Total Tables: {}\n", program.tables.len()));
    s.push_str("\n");

    s.push_str("Functions:\n");
    for (func_id, function) in program.functions.iter() {
        s.push_str(&format!(
            "  {}: {} ({} hops, {} blocks, {} variables)\n",
            func_id.index(),
            function.name,
            function.hops.len(),
            function.blocks.len(),
            function.variables.len()
        ));
    }

    s
}

// Helper functions for type names
fn type_name(t: &TypeName) -> &'static str {
    match t {
        TypeName::Int => "int",
        TypeName::Float => "float",
        TypeName::String => "string",
        TypeName::Bool => "bool",
    }
}

fn return_type_name(ret: &ReturnType) -> String {
    match ret {
        ReturnType::Void => "void".to_string(),
        ReturnType::Type(t) => type_name(t).to_string(),
    }
}

/// Print options for CFG output
#[derive(Debug, Clone)]
pub struct CfgPrintOptions {
    pub format: CfgFormat,
    pub verbose: bool,
    pub quiet: bool,
    pub show_spans: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CfgFormat {
    Text,
    Dot,
    Summary,
}

impl Default for CfgPrintOptions {
    fn default() -> Self {
        Self {
            format: CfgFormat::Text,
            verbose: false,
            quiet: false,
            show_spans: false,
        }
    }
}

/// Main entry point for printing CFG
pub fn print_cfg(
    program: &CfgProgram,
    options: &CfgPrintOptions,
    writer: &mut impl Write,
) -> Result<()> {
    if options.quiet && options.format != CfgFormat::Text { // For Dot and Summary, quiet means no extra console output from main, but file still written
         // For non-text formats, quiet doesn't change the content written to file/stdout for that format.
         // The main `output_cfg` function handles not printing "Building CFG..." etc.
         // If we are writing to a file, the file should still be generated.
         // If we are writing to stdout, then for Dot/Summary, it should still print.
         // The `format_cfg_text` handles its own "quiet mode" message.
    }

    match options.format {
        CfgFormat::Text => {
            let output = format_cfg_text(program, options);
            write!(writer, "{}", output)?;
        }
        CfgFormat::Dot => {
            print_cfg_dot_format(program, writer, options.show_spans)?;
        }
        CfgFormat::Summary => {
            let output = print_cfg_summary(program);
            write!(writer, "{}", output)?;
        }
    }
    Ok(())
}
