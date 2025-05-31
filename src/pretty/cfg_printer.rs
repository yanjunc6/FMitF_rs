use crate::cfg::*;
use std::io::{Write, Result};

fn escape_dot_label(s: &str) -> String {
    s.replace("\n", "\\n").replace("\"", "\\\"").replace("{", "\\{").replace("}", "\\}")
}

fn format_operand(operand: &Operand, function: &FunctionCfg) -> String {
    match operand {
        Operand::Variable(var_id) => {
            let var = &function.variables[*var_id];
            format!("%{}", var.name)
        }
        Operand::Constant(c) => match c {
            Constant::Int(i) => i.to_string(),
            Constant::Float(f) => f.to_string(),
            Constant::Bool(b) => b.to_string(),
            Constant::String(s) => format!("\"{}\"", escape_dot_label(s)),
        },
    }
}

fn format_rvalue(rvalue: &Rvalue, function: &FunctionCfg, ctx: &CfgCtx) -> String {
    match rvalue {
        Rvalue::Use(op) => format_operand(op, function),
        Rvalue::TableAccess { table_id, primary_key, field_id } => {
            let table = &ctx.tables[*table_id];
            let field = &ctx.fields[*field_id];
            format!("{}[pk:{}].{}", table.name, format_operand(primary_key, function), field.field_name)
        }
        Rvalue::UnaryOp { op, operand } => {
            format!("{:?} {}", op, format_operand(operand, function))
        }
        Rvalue::BinaryOp { op, left, right } => {
            format!("{} {:?} {}", format_operand(left, function), op, format_operand(right, function))
        }
    }
}

fn format_statement(stmt: &Statement, function: &FunctionCfg, ctx: &CfgCtx) -> String {
    match stmt {
        Statement::Assign { variable, rvalue, .. } => {
            let var = &function.variables[*variable];
            format!("%{} = {}", var.name, format_rvalue(rvalue, function, ctx))
        }
        Statement::TableAssign { table_id, primary_key, field_id, value, .. } => {
            let table = &ctx.tables[*table_id];
            let field = &ctx.fields[*field_id];
            format!("{}[pk:{}].{} = {}", 
                table.name, 
                format_operand(primary_key, function), 
                field.field_name, 
                format_operand(value, function))
        }
        Statement::DeclareVariable { variable, init, .. } => {
            let var = &function.variables[*variable];
            format!("declare %{} = {}", var.name, format_rvalue(init, function, ctx))
        }
    }
}

fn format_terminator(terminator: &Terminator, function: &FunctionCfg) -> String {
    match terminator {
        Terminator::Goto(block_id) => format!("goto bb{}", block_id.index()),
        Terminator::Branch { condition, then_block, else_block } => {
            format!("if {} then bb{} else bb{}", 
                format_operand(condition, function),
                then_block.index(),
                else_block.index())
        }
        Terminator::Return(Some(op)) => format!("return {}", format_operand(op, function)),
        Terminator::Return(None) => "return".to_string(),
        Terminator::Abort => "abort".to_string(),
        Terminator::HopTransition { next_hop, target_block } => {
            format!("transition to hop{} bb{}", next_hop.index(), target_block.index())
        }
        Terminator::FunctionExit => "function_exit".to_string(),
    }
}

pub fn print_cfg_dot_format(ctx: &CfgCtx, writer: &mut impl Write) -> Result<()> {
    writeln!(writer, "digraph CFG {{")?;
    writeln!(writer, "  compound=true;")?;
    writeln!(writer, "  node [shape=box, style=rounded];")?;
    writeln!(writer, "")?;

    for (func_id, function) in ctx.functions.iter() {
        writeln!(writer, "  subgraph cluster_func_{} {{", func_id.index())?;
        writeln!(writer, "    label=\"Function: {}\";", escape_dot_label(&function.name))?;
        writeln!(writer, "    style=filled;")?;
        writeln!(writer, "    color=lightgrey;")?;
        writeln!(writer, "")?;

        // Group blocks by hop for visual clustering
        let mut hop_to_blocks: std::collections::HashMap<HopId, Vec<BasicBlockId>> = 
            std::collections::HashMap::new();
        
        for (block_id, block) in function.blocks.iter() {
            if let Some(hop_id) = block.hop_id {
                hop_to_blocks.entry(hop_id).or_default().push(block_id);
            }
        }

        for (hop_id, blocks_in_hop) in &hop_to_blocks {
            writeln!(writer, "    subgraph cluster_func_{}_hop_{} {{", func_id.index(), hop_id.index())?;
            let hop = &function.hops[*hop_id];
            let node_name = &ctx.nodes[hop.node_id].name;
            writeln!(writer, "      label=\"Hop {} on Node {}\";", hop_id.index(), escape_dot_label(node_name))?;
            writeln!(writer, "      style=dotted;")?;
            
            for &block_id in blocks_in_hop {
                let block = &function.blocks[block_id];
                let mut label = format!("BB{} (Hop {})\n", block_id.index(), hop_id.index());
                
                for stmt in &block.statements {
                    label.push_str(&format!("{}\n", format_statement(stmt, function, ctx)));
                }
                
                if let Some(ref term) = block.terminator {
                    label.push_str(&format!("Term: {}", format_terminator(term, function)));
                }
                
                writeln!(writer, "      f{}_bb{} [label=\"{}\"];", 
                    func_id.index(), block_id.index(), escape_dot_label(&label))?;
            }
            writeln!(writer, "    }}")?;
        }
        writeln!(writer, "")?;

        // Draw edges between blocks
        for (block_id, block) in function.blocks.iter() {
            let source_node = format!("f{}_bb{}", func_id.index(), block_id.index());
            
            if let Some(ref terminator) = block.terminator {
                match terminator {
                    Terminator::Goto(target_bb) => {
                        writeln!(writer, "    {} -> f{}_bb{};", 
                            source_node, func_id.index(), target_bb.index())?;
                    }
                    Terminator::Branch { condition, then_block, else_block } => {
                        let cond_str = format_operand(condition, function);
                        writeln!(writer, "    {} -> f{}_bb{} [label=\"{}\"];", 
                            source_node, func_id.index(), then_block.index(), 
                            escape_dot_label(&format!("{} is true", cond_str)))?;
                        writeln!(writer, "    {} -> f{}_bb{} [label=\"{}\"];", 
                            source_node, func_id.index(), else_block.index(), 
                            escape_dot_label(&format!("{} is false", cond_str)))?;
                    }
                    Terminator::HopTransition { next_hop, target_block } => {
                        writeln!(writer, "    {} -> f{}_bb{} [label=\"To Hop {}\", style=dashed];",
                            source_node, func_id.index(), target_block.index(), next_hop.index())?;
                    }
                    Terminator::Return(_) | Terminator::Abort | Terminator::FunctionExit => {
                        // No outgoing edges
                    }
                }
            }
        }
        writeln!(writer, "  }}")?;
        writeln!(writer, "")?;
    }

    writeln!(writer, "}}")?;
    Ok(())
}

pub fn format_cfg_text(ctx: &CfgCtx, verbose: bool, quiet: bool) -> String {
    if quiet {
        return String::from("CFG generation complete (quiet mode).");
    }

    let mut s = String::new();
    s.push_str("Control Flow Graph (CFG):\n");
    s.push_str("===========================\n\n");

    for (func_id, function) in ctx.functions.iter() {
        s.push_str(&format!("Function {}: {} (", func_id.index(), function.name));
        
        // Print parameters
        for (i, &param_id) in function.parameters.iter().enumerate() {
            let param = &function.variables[param_id];
            s.push_str(&format!("{} {}", type_name(&param.ty), param.name));
            if i < function.parameters.len() - 1 {
                s.push_str(", ");
            }
        }
        s.push_str(&format!(") -> {}\n", return_type_name(&function.return_type)));
        
        if let Some(entry_hop) = function.entry_hop {
            s.push_str(&format!("  Entry Hop: {}\n", entry_hop.index()));
        }

        if verbose {
            s.push_str("  Variables:\n");
            for (var_id, var) in function.variables.iter() {
                let scope_str = match &var.scope {
                    VariableScope::Function => "function".to_string(),
                    VariableScope::Hop(hop_id) => format!("hop {}", hop_id.index()),
                };
                let param_str = if var.is_param { " (param)" } else { "" };
                s.push_str(&format!("    Var {} ({}): {} {} [scope: {}]{}\n", 
                    var_id.index(), var.name, type_name(&var.ty), var.name, scope_str, param_str));
            }
            s.push_str("\n");

            s.push_str("  Hops:\n");
            for (hop_id, hop) in function.hops.iter() {
                let node_name = &ctx.nodes[hop.node_id].name;
                s.push_str(&format!("    Hop {} on Node {}\n", hop_id.index(), node_name));
                if let Some(entry_block) = hop.entry_block {
                    s.push_str(&format!("      Entry Block: BB{}\n", entry_block.index()));
                }
                s.push_str(&format!("      Blocks: {:?}\n", 
                    hop.blocks.iter().map(|b| b.index()).collect::<Vec<_>>()));
            }
            s.push_str("\n");

            s.push_str("  Basic Blocks:\n");
            for (block_id, block) in function.blocks.iter() {
                let hop_str = match block.hop_id {
                    Some(hop_id) => format!("Hop {}", hop_id.index()),
                    None => "No Hop".to_string(),
                };
                s.push_str(&format!("    BB{} ({}):\n", block_id.index(), hop_str));
                
                for stmt in &block.statements {
                    s.push_str(&format!("      {}\n", format_statement(stmt, function, ctx)));
                }
                
                if let Some(ref terminator) = block.terminator {
                    s.push_str(&format!("      Terminator: {}\n", format_terminator(terminator, function)));
                }
                
                s.push_str(&format!("      Successors: {:?}\n", 
                    block.successors.iter().map(|b| b.index()).collect::<Vec<_>>()));
                s.push_str(&format!("      Predecessors: {:?}\n", 
                    block.predecessors.iter().map(|b| b.index()).collect::<Vec<_>>()));
            }
        } else {
            s.push_str(&format!("  Number of Hops: {}\n", function.hops.len()));
            s.push_str(&format!("  Number of Basic Blocks: {}\n", function.blocks.len()));
            s.push_str(&format!("  Number of Variables: {}\n", function.variables.len()));
        }
        s.push_str("\n---------------------------\n\n");
    }
    s
}

pub fn print_cfg_summary(ctx: &CfgCtx) -> String {
    let mut s = String::new();
    s.push_str("CFG Summary:\n");
    s.push_str("============\n\n");
    
    s.push_str(&format!("Total Functions: {}\n", ctx.functions.len()));
    s.push_str(&format!("Total Nodes: {}\n", ctx.nodes.len()));
    s.push_str(&format!("Total Tables: {}\n", ctx.tables.len()));
    s.push_str("\n");
    
    s.push_str("Functions:\n");
    for (func_id, function) in ctx.functions.iter() {
        s.push_str(&format!("  {}: {} ({} hops, {} blocks, {} variables)\n",
            func_id.index(),
            function.name,
            function.hops.len(),
            function.blocks.len(),
            function.variables.len()));
    }
    
    s
}

// Helper functions
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

#[derive(Debug, Clone)]
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
pub fn print_cfg(ctx: &CfgCtx, options: &CfgPrintOptions, writer: &mut impl Write) -> Result<()> {
    match options.format {
        CfgFormat::Text => {
            let output = format_cfg_text(ctx, options.verbose, options.quiet);
            write!(writer, "{}", output)?;
        }
        CfgFormat::Dot => {
            print_cfg_dot_format(ctx, writer)?;
        }
        CfgFormat::Summary => {
            let output = print_cfg_summary(ctx);
            write!(writer, "{}", output)?;
        }
    }
    Ok(())
}