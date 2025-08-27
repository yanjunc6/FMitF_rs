use crate::cfg::CfgProgram;
use crate::pretty::PrettyPrinter;
use crate::sc_graph::{CombinedEdge, CombinedSCGraph, CombinedVertex, CombinedVertexId};
use std::io::Write;

/// DOT file generator for Combined SC-Graph visualization.
///
/// This generates GraphViz DOT format files for the combined SC-Graph after
/// deadlock elimination. The combined graph represents:
/// - Combined vertices as rectangular nodes showing merged pieces
/// - Directed edges between combined vertices (only S-edges remain)
/// - Transaction pieces grouped by function and instance within each vertex
///
/// The visualization can be rendered using:
/// - `dot -Tpng combined_graph.dot -o combined_graph.png` (PNG image)
/// - `dot -Tsvg combined_graph.dot -o combined_graph.svg` (SVG image)
/// - Online GraphViz viewers
pub struct CombinedDotPrinter {
    /// Whether to include detailed labels showing hop IDs
    show_details: bool,
    /// Whether to use colors for different pieces
    use_colors: bool,
    /// Whether to show individual hop IDs within pieces
    show_hop_details: bool,
}

impl CombinedDotPrinter {
    /// Creates a new combined DOT printer with default settings.
    pub fn new() -> Self {
        Self {
            show_details: true,
            use_colors: true,
            show_hop_details: true,
        }
    }

    /// Creates a new combined DOT printer with custom settings.
    pub fn with_options(show_details: bool, use_colors: bool, show_hop_details: bool) -> Self {
        Self {
            show_details,
            use_colors,
            show_hop_details,
        }
    }

    /// Generate DOT format for the Combined SC-Graph
    pub fn generate_dot(
        &self,
        combined_graph: &CombinedSCGraph,
        cfg_program: &CfgProgram,
        writer: &mut dyn Write,
    ) -> std::io::Result<()> {
        // Start as digraph since all remaining edges are directed
        writeln!(writer, "digraph CombinedSCGraph {{")?;
        writeln!(writer, "    rankdir=TB;")?;
        writeln!(writer, "    node [shape=box, style=\"rounded,filled\"];")?;

        if self.use_colors {
            writeln!(writer, "    edge [penwidth=2, color=blue];")?;
        }

        writeln!(writer)?;
        writeln!(writer, "    // Graph metadata")?;
        writeln!(writer, "    label=\"Combined SC-Graph (Deadlock-Free)\\nVertices: {} | Edges: {} | Acyclic: {}\";",
            combined_graph.vertices.len(),
            combined_graph.edges.len(),
            combined_graph.is_acyclic()
        )?;
        writeln!(writer, "    labelloc=t;")?;
        writeln!(writer, "    fontsize=14;")?;
        writeln!(writer)?;

        // Generate combined vertices
        writeln!(writer, "    // Combined Vertices")?;
        for vertex in &combined_graph.vertices {
            self.generate_combined_vertex(vertex, cfg_program, writer)?;
        }
        writeln!(writer)?;

        // Generate edges
        if !combined_graph.edges.is_empty() {
            writeln!(writer, "    // Edges (All Sequential - Directed)")?;
            for edge in &combined_graph.edges {
                self.generate_combined_edge(edge, writer)?;
            }
            writeln!(writer)?;
        }

        // End the graph
        writeln!(writer, "}}")?;

        Ok(())
    }

    /// Generate a combined vertex declaration
    fn generate_combined_vertex(
        &self,
        vertex: &CombinedVertex,
        cfg_program: &CfgProgram,
        writer: &mut dyn Write,
    ) -> std::io::Result<()> {
        let vertex_name = self.vertex_name(vertex.id);

        // Build the label showing all pieces in this vertex
        let mut label = format!("Vertex {}", vertex.id);

        if self.show_details {
            label.push_str("\\n");

            for (piece_idx, piece) in vertex.pieces.iter().enumerate() {
                if piece_idx > 0 {
                    label.push_str("\\n");
                }

                // Get function name from CFG program
                let function_name =
                    if let Some(function) = cfg_program.functions.get(piece.function_id) {
                        function.name.clone()
                    } else {
                        format!(
                            "unknown_func_{}",
                            self.format_function_id(piece.function_id)
                        )
                    };

                label.push_str(&format!("{} #{}", function_name, piece.instance + 1));

                if self.show_hop_details && !piece.hop_ids.is_empty() {
                    let hop_list: Vec<String> = piece
                        .hop_ids
                        .iter()
                        .map(|hop_id| self.format_hop_id(*hop_id))
                        .collect();
                    label.push_str(&format!(" [{}]", hop_list.join(",")));
                }
            }
        }

        // Choose color based on number of pieces
        let color = if self.use_colors {
            match vertex.pieces.len() {
                1 => "lightblue",
                2 => "lightgreen",
                3 => "lightyellow",
                4 => "lightpink",
                _ => "lightgray",
            }
        } else {
            "white"
        };

        writeln!(
            writer,
            "    {} [label=\"{}\", fillcolor={}];",
            vertex_name, label, color
        )?;

        Ok(())
    }

    /// Generate a combined edge (all edges are directed S-edges)
    fn generate_combined_edge(
        &self,
        edge: &CombinedEdge,
        writer: &mut dyn Write,
    ) -> std::io::Result<()> {
        let source_name = self.vertex_name(edge.source);
        let target_name = self.vertex_name(edge.target);

        if self.use_colors {
            writeln!(
                writer,
                "    {} -> {} [color=blue, label=\"S\"];",
                source_name, target_name
            )?;
        } else {
            writeln!(
                writer,
                "    {} -> {} [label=\"S\"];",
                source_name, target_name
            )?;
        }

        Ok(())
    }

    /// Generate a unique vertex name for DOT format
    fn vertex_name(&self, vertex_id: CombinedVertexId) -> String {
        format!("vertex_{}", vertex_id)
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

impl Default for CombinedDotPrinter {
    fn default() -> Self {
        Self::new()
    }
}

/// Implement PrettyPrinter trait for consistent interface
/// Note: This requires both CombinedSCGraph and CfgProgram as a tuple input
impl PrettyPrinter<(CombinedSCGraph, CfgProgram)> for CombinedDotPrinter {
    fn print(
        &self,
        data: &(CombinedSCGraph, CfgProgram),
        writer: &mut dyn Write,
    ) -> std::io::Result<()> {
        self.generate_dot(&data.0, &data.1, writer)
    }
}
