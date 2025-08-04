use crate::sc_graph::{EdgeType, SCGraph, SCGraphEdge, SCGraphNodeId};
use crate::cfg::CfgProgram;
use std::io::Write;

/// DOT file generator for SC-Graph visualization.
/// 
/// This generates GraphViz DOT format files that can be rendered using:
/// - `dot -Tpng graph.dot -o graph.png` (PNG image)
/// - `dot -Tsvg graph.dot -o graph.svg` (SVG image)
/// - Online GraphViz viewers
/// 
/// The visualization represents:
/// - S-edges as directed edges (arrows) in blue
/// - C-edges as undirected edges (no arrows) in red
/// - Nodes with hop and function information
pub struct DotPrinter {
    /// Whether to include detailed labels
    show_details: bool,
    /// Whether to use colors for different edge types
    use_colors: bool,
}

impl DotPrinter {
    /// Creates a new DOT printer with default settings.
    pub fn new() -> Self {
        Self {
            show_details: true,
            use_colors: true,
        }
    }

    /// Creates a new DOT printer with custom settings.
    pub fn with_options(show_details: bool, use_colors: bool) -> Self {
        Self {
            show_details,
            use_colors,
        }
    }

    /// Generate DOT format for the SC-Graph
    pub fn generate_dot(&self, sc_graph: &SCGraph, cfg_program: &CfgProgram, writer: &mut dyn Write) -> std::io::Result<()> {
        // Start as digraph to support both directed and undirected edges
        writeln!(writer, "digraph SCGraph {{")?;
        writeln!(writer, "    rankdir=TB;")?;
        writeln!(writer, "    node [shape=box, style=rounded];")?;
        
        if self.use_colors {
            writeln!(writer, "    edge [penwidth=2];")?;
        }

        writeln!(writer)?;
        writeln!(writer, "    // Graph metadata")?;
        writeln!(writer, "    label=\"SC-Graph (Serializability Conflict Graph)\\nNodes: {} | S-edges: {} | C-edges: {}\";",
            sc_graph.nodes.len(),
            sc_graph.edges.iter().filter(|e| e.edge_type == EdgeType::S).count(),
            sc_graph.edges.iter().filter(|e| e.edge_type == EdgeType::C).count()
        )?;
        writeln!(writer, "    labelloc=t;")?;
        writeln!(writer, "    fontsize=14;")?;
        writeln!(writer)?;

        // Generate nodes
        writeln!(writer, "    // Nodes")?;
        for &node_id in &sc_graph.nodes {
            self.generate_node(node_id, cfg_program, writer)?;
        }
        writeln!(writer)?;

        // Generate edges - separate S and C edges for clarity
        let s_edges: Vec<_> = sc_graph.edges.iter()
            .filter(|e| e.edge_type == EdgeType::S)
            .collect();
        let c_edges: Vec<_> = sc_graph.edges.iter()
            .filter(|e| e.edge_type == EdgeType::C)
            .collect();

        if !s_edges.is_empty() {
            writeln!(writer, "    // S-edges (Sequential - Directed)")?;
            for edge in s_edges {
                self.generate_s_edge(edge, writer)?;
            }
            writeln!(writer)?;
        }

        if !c_edges.is_empty() {
            writeln!(writer, "    // C-edges (Conflict - Undirected)")?;
            for edge in c_edges {
                self.generate_c_edge(edge, writer)?;
            }
            writeln!(writer)?;
        }

        // End the graph
        writeln!(writer, "}}")?;

        Ok(())
    }

    /// Generate a node declaration
    fn generate_node(&self, node_id: SCGraphNodeId, cfg_program: &CfgProgram, writer: &mut dyn Write) -> std::io::Result<()> {
        let node_name = self.node_name(node_id);
        
        // Get function name from CFG program
        let function_name = if let Some(function) = cfg_program.functions.get(node_id.function_id) {
            function.name.clone()
        } else {
            format!("unknown_func_{}", self.format_function_id(node_id.function_id))
        };
        
        if self.show_details {
            writeln!(writer, "    {} [label=\"Hop {}\\n{}\"];",
                node_name,
                self.format_hop_id(node_id.hop_id),
                function_name
            )?;
        } else {
            writeln!(writer, "    {} [label=\"Hop {}\"];",
                node_name,
                self.format_hop_id(node_id.hop_id)
            )?;
        }

        Ok(())
    }

    /// Generate an S-edge (directed)
    fn generate_s_edge(&self, edge: &SCGraphEdge, writer: &mut dyn Write) -> std::io::Result<()> {
        let source_name = self.node_name(edge.source);
        let target_name = self.node_name(edge.target);

        if self.use_colors {
            writeln!(writer, "    {} -> {} [color=blue, label=\"S\"];",
                source_name, target_name
            )?;
        } else {
            writeln!(writer, "    {} -> {} [label=\"S\"];",
                source_name, target_name
            )?;
        }

        Ok(())
    }

    /// Generate a C-edge (undirected)
    fn generate_c_edge(&self, edge: &SCGraphEdge, writer: &mut dyn Write) -> std::io::Result<()> {
        let source_name = self.node_name(edge.source);
        let target_name = self.node_name(edge.target);

        if self.use_colors {
            writeln!(writer, "    {} -> {} [dir=none, color=red, label=\"C\"];",
                source_name, target_name
            )?;
        } else {
            writeln!(writer, "    {} -> {} [dir=none, label=\"C\"];",
                source_name, target_name
            )?;
        }

        Ok(())
    }

    /// Generate a unique node name for DOT format
    fn node_name(&self, node_id: SCGraphNodeId) -> String {
        format!("hop_{}_{}", 
            self.format_function_id(node_id.function_id),
            self.format_hop_id(node_id.hop_id)
        )
    }

    /// Format hop ID for display
    fn format_hop_id(&self, hop_id: crate::cfg::HopId) -> String {
        let debug_str = format!("{:?}", hop_id);
        if let Some(start) = debug_str.find("idx: ") {
            let start_pos = start + 5;
            if let Some(end) = debug_str[start_pos..].find(' ') {
                debug_str[start_pos..start_pos + end].to_string()
            } else {
                debug_str[start_pos..debug_str.len() - 2].to_string()
            }
        } else {
            "0".to_string()
        }
    }

    /// Format function ID for display
    fn format_function_id(&self, function_id: crate::cfg::FunctionId) -> String {
        let debug_str = format!("{:?}", function_id);
        if let Some(start) = debug_str.find("idx: ") {
            let start_pos = start + 5;
            if let Some(end) = debug_str[start_pos..].find(' ') {
                debug_str[start_pos..start_pos + end].to_string()
            } else {
                debug_str[start_pos..debug_str.len() - 2].to_string()
            }
        } else {
            "0".to_string()
        }
    }
}

impl Default for DotPrinter {
    fn default() -> Self {
        Self::new()
    }
}
