use crate::cfg::{CfgProgram, FunctionId as CfgFunctionId};
use crate::sc_graph::{EdgeType as SCGraphEdgeType, SCGraph, SCGraphNodeId};
use std::collections::HashMap;
use std::io::{Result, Write};

fn escape_dot_label(s: &str) -> String {
    s.replace("\n", "\\n")
        .replace("\"", "\\\"")
        .replace("{", "\\{")
        .replace("}", "\\}")
}

/// Print options for SC-Graph output
#[derive(Debug, Clone)]
pub struct SCGraphPrintOptions {
    pub format: SCGraphFormat,
    pub verbose: bool,
    pub show_spans: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SCGraphFormat {
    Text,
    Dot,
    Summary,
}

impl Default for SCGraphPrintOptions {
    fn default() -> Self {
        Self {
            format: SCGraphFormat::Summary,
            verbose: false,
            show_spans: false,
        }
    }
}

/// Main entry point for printing SC-Graph
pub fn print_sc_graph(
    sc_graph: &SCGraph,
    cfg_program: &CfgProgram,
    options: &SCGraphPrintOptions,
    writer: &mut impl Write,
) -> Result<()> {
    match options.format {
        SCGraphFormat::Text => {
            let output = format_sc_graph_text(sc_graph, cfg_program, options);
            write!(writer, "{}", output)?;
        }
        SCGraphFormat::Dot => {
            format_sc_graph_dot(sc_graph, cfg_program, writer)?;
        }
        SCGraphFormat::Summary => {
            let output = format_sc_graph_summary(sc_graph, cfg_program);
            write!(writer, "{}", output)?;
        }
    }
    Ok(())
}

fn format_sc_graph_summary(sc_graph: &SCGraph, _cfg_program: &CfgProgram) -> String {
    let (nodes_count, s_edges_count, c_edges_count) = sc_graph.stats();
    let mixed_cycles = sc_graph.find_mixed_cycles();

    format!(
        "SC-Graph Summary:\n\
         =================\n\
         Nodes (Hops): {}\n\
         S-Edges: {}\n\
         C-Edges: {}\n\
         Mixed S/C Cycles Found: {}\n",
        nodes_count,
        s_edges_count,
        c_edges_count,
        mixed_cycles.len()
    )
}

fn format_sc_graph_text(
    sc_graph: &SCGraph,
    cfg_program: &CfgProgram,
    options: &SCGraphPrintOptions,
) -> String {
    let mut s = String::new();
    let (nodes_count, s_edges_count, c_edges_count) = sc_graph.stats();

    s.push_str("Serializability Conflict Graph (SC-Graph):\n");
    s.push_str("==========================================\n\n");
    s.push_str(&format!(
        "Stats: {} Nodes (Hops), {} S-Edges, {} C-Edges\n\n",
        nodes_count, s_edges_count, c_edges_count
    ));

    if options.verbose {
        s.push_str("Nodes (Hops):\n");
        for (sc_node_id, sc_node) in sc_graph.nodes.iter() {
            let func_name = &cfg_program.functions[sc_node.cfg_function_id].name;
            let cfg_node_name = &cfg_program.nodes[sc_node.cfg_node_id].name;
            s.push_str(&format!(
                "  SCNode {} (CFG Hop {}): Func='{}', CFGNode='{}'\n",
                sc_node_id.index(),
                sc_node.cfg_hop_id.index(),
                func_name,
                cfg_node_name
            ));
        }
        s.push_str("\nEdges:\n");
        for edge in &sc_graph.edges {
            // Fix: edge.source and edge.target are already SCGraphNodeId, use them directly
            let source_sc_node = &sc_graph.nodes[edge.source];
            let target_sc_node = &sc_graph.nodes[edge.target];
            s.push_str(&format!(
                "  CFGHop {} -> CFGHop {} (Type: {:?})\n",
                source_sc_node.cfg_hop_id.index(),
                target_sc_node.cfg_hop_id.index(),
                edge.edge_type
            ));
        }
        s.push_str("\n");
    }

    let mixed_cycles = sc_graph.find_mixed_cycles();
    s.push_str(&format!("Mixed S/C Cycles Found: {}\n", mixed_cycles.len()));
    if options.verbose && !mixed_cycles.is_empty() {
        s.push_str("Cycles:\n");
        for (i, cycle) in mixed_cycles.iter().enumerate() {
            let cycle_str: Vec<String> = cycle
                .iter()
                .map(|h_id| format!("H{}", h_id.index()))
                .collect();
            s.push_str(&format!("  Cycle {}: {}\n", i + 1, cycle_str.join(" -> ")));
        }
    }

    s
}

fn format_sc_graph_dot(
    sc_graph: &SCGraph,
    cfg_program: &CfgProgram,
    writer: &mut impl Write,
) -> Result<()> {
    writeln!(writer, "digraph SCGraph {{")?;
    writeln!(writer, "  compound=true;")?;
    writeln!(writer, "  node [shape=box, style=rounded];")?;
    writeln!(writer, "")?;

    // Group nodes by CFG Function
    let mut func_to_sc_nodes: HashMap<CfgFunctionId, Vec<SCGraphNodeId>> = HashMap::new();
    for (sc_node_id, sc_node_data) in sc_graph.nodes.iter() {
        func_to_sc_nodes
            .entry(sc_node_data.cfg_function_id)
            .or_default()
            .push(sc_node_id);
    }

    for (cfg_func_id, sc_node_ids) in &func_to_sc_nodes {
        let func_name = &cfg_program.functions[*cfg_func_id].name;
        writeln!(writer, "  subgraph cluster_func_{} {{", cfg_func_id.index())?;
        writeln!(
            writer,
            "    label=\"Function: {}\";",
            escape_dot_label(func_name)
        )?;
        writeln!(writer, "    style=filled;")?;
        writeln!(writer, "    color=lightgrey;")?;

        for &sc_node_id in sc_node_ids {
            let sc_node_data = &sc_graph.nodes[sc_node_id];
            let cfg_node_name = &cfg_program.nodes[sc_node_data.cfg_node_id].name;
            let label = format!(
                "Hop {} (CFG H{})\nNode: {}",
                sc_node_id.index(),
                sc_node_data.cfg_hop_id.index(),
                escape_dot_label(cfg_node_name)
            );
            writeln!(
                writer,
                "    sc_node_{} [label=\"{}\"];",
                sc_node_id.index(),
                label
            )?;
        }
        writeln!(writer, "  }}")?;
    }
    writeln!(writer, "")?;

    // Output edges
    for edge in &sc_graph.edges {
        // Fix: edge.source and edge.target are already SCGraphNodeId, use them directly
        let (color, style) = match edge.edge_type {
            SCGraphEdgeType::S => ("blue", "solid"),
            SCGraphEdgeType::C => ("red", "dashed"),
        };
        writeln!(
            writer,
            "  sc_node_{} -> sc_node_{} [color={}, style={}, label=\"{:?}\"];",
            edge.source.index(),
            edge.target.index(),
            color,
            style,
            edge.edge_type
        )?;
    }

    writeln!(writer, "}}")?;
    Ok(())
}
