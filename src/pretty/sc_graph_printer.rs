use super::PrettyPrinter;
use crate::sc_graph::{SCGraph, SCGraphEdge, SCGraphNodeId, EdgeType};
use std::io::Write;
use std::collections::HashMap;

/// This printer correctly handles:
/// - S-edges as directed edges (→)
/// - C-edges as undirected edges (↔)
/// - Node information with hop and function details
pub struct SCGraphPrinter {
    /// Indentation level for nested structures
    indent_size: usize,
    /// Whether to show detailed node information
    show_details: bool,
    /// Whether to use graphical symbols for edges
    use_symbols: bool,
}

impl SCGraphPrinter {
    /// Creates a new SC-Graph printer with default settings.
    pub fn new() -> Self {
        Self { 
            indent_size: 2,
            show_details: true,
            use_symbols: true,
        }
    }

    /// Creates a new SC-Graph printer with custom settings.
    pub fn with_options(indent_size: usize, show_details: bool, use_symbols: bool) -> Self {
        Self { indent_size, show_details, use_symbols }
    }

    /// Helper to write indentation
    fn write_indent(&self, writer: &mut dyn Write, level: usize) -> std::io::Result<()> {
        for _ in 0..(level * self.indent_size) {
            write!(writer, " ")?;
        }
        Ok(())
    }

    /// Print the entire SC-Graph
    fn print_sc_graph(&self, sc_graph: &SCGraph, writer: &mut dyn Write) -> std::io::Result<()> {
        let nodes_count = sc_graph.nodes.len();
        let s_edges_count = sc_graph.edges.iter().filter(|e| e.edge_type == EdgeType::S).count();
        let c_edges_count = sc_graph.edges.iter().filter(|e| e.edge_type == EdgeType::C).count();
        
        writeln!(writer, "SC-Graph (Serializability Conflict Graph):")?;
        writeln!(writer, "  Nodes: {} (hops), S-edges: {} (directed), C-edges: {} (undirected)", 
                 nodes_count, s_edges_count, c_edges_count)?;
        writeln!(writer)?;

        // Print nodes
        if nodes_count > 0 {
            self.write_indent(writer, 1)?;
            writeln!(writer, "Nodes:")?;
            for &node_id in &sc_graph.nodes {
                self.print_node(node_id, writer, 2)?;
            }
            writeln!(writer)?;
        }

        // Group edges by type for better readability
        let mut s_edges = Vec::new();
        let mut c_edges = Vec::new();
        
        for edge in &sc_graph.edges {
            match edge.edge_type {
                EdgeType::S => s_edges.push(edge),
                EdgeType::C => c_edges.push(edge),
            }
        }

        // Print S-edges (directed)
        if !s_edges.is_empty() {
            self.write_indent(writer, 1)?;
            writeln!(writer, "S-edges (Sequential - Directed):")?;
            for edge in s_edges {
                self.print_s_edge(edge, sc_graph, writer, 2)?;
            }
            writeln!(writer)?;
        }

        // Print C-edges (undirected)
        if !c_edges.is_empty() {
            self.write_indent(writer, 1)?;
            writeln!(writer, "C-edges (Conflict - Undirected):")?;
            for edge in c_edges {
                self.print_c_edge(edge, sc_graph, writer, 2)?;
            }
            writeln!(writer)?;
        }

        // Print adjacency information if detailed view is enabled
        if self.show_details && nodes_count > 0 {
            self.print_adjacency_info(sc_graph, writer)?;
        }

        Ok(())
    }

    /// Print a single node with its information
    fn print_node(&self, node_id: SCGraphNodeId, writer: &mut dyn Write, level: usize) -> std::io::Result<()> {
        self.write_indent(writer, level)?;
        if self.show_details {
            writeln!(writer, "hop_{:?} (function_{:?})", node_id.hop_id, node_id.function_id)?;
        } else {
            writeln!(writer, "hop_{:?}", node_id.hop_id)?;
        }
        Ok(())
    }

    /// Print an S-edge (directed sequential edge)
    fn print_s_edge(&self, edge: &SCGraphEdge, _sc_graph: &SCGraph, writer: &mut dyn Write, level: usize) -> std::io::Result<()> {
        self.write_indent(writer, level)?;
        
        if self.use_symbols {
            writeln!(writer, "hop_{:?} → hop_{:?}", edge.source.hop_id, edge.target.hop_id)?;
        } else {
            writeln!(writer, "hop_{:?} -> hop_{:?}", edge.source.hop_id, edge.target.hop_id)?;
        }
        
        if self.show_details {
            self.write_indent(writer, level + 1)?;
            writeln!(writer, "(hop_{:?} → hop_{:?} within functions {:?}→{:?})", 
                     edge.source.hop_id, edge.target.hop_id, edge.source.function_id, edge.target.function_id)?;
        }
        
        Ok(())
    }

    /// Print a C-edge (undirected conflict edge)
    fn print_c_edge(&self, edge: &SCGraphEdge, _sc_graph: &SCGraph, writer: &mut dyn Write, level: usize) -> std::io::Result<()> {
        self.write_indent(writer, level)?;
        
        if self.use_symbols {
            writeln!(writer, "hop_{:?} ↔ hop_{:?}", edge.source.hop_id, edge.target.hop_id)?;
        } else {
            writeln!(writer, "hop_{:?} <-> hop_{:?}", edge.source.hop_id, edge.target.hop_id)?;
        }
        
        if self.show_details {
            self.write_indent(writer, level + 1)?;
            writeln!(writer, "(hop_{:?} ↔ hop_{:?} between function_{:?} and function_{:?})", 
                     edge.source.hop_id, edge.target.hop_id, edge.source.function_id, edge.target.function_id)?;
        }
        
        Ok(())
    }

    /// Print adjacency information for each node
    fn print_adjacency_info(&self, sc_graph: &SCGraph, writer: &mut dyn Write) -> std::io::Result<()> {
        self.write_indent(writer, 1)?;
        writeln!(writer, "Adjacency Information:")?;
        
        // Build adjacency maps
        let mut s_outgoing: HashMap<SCGraphNodeId, Vec<SCGraphNodeId>> = HashMap::new();
        let mut s_incoming: HashMap<SCGraphNodeId, Vec<SCGraphNodeId>> = HashMap::new();
        let mut c_adjacent: HashMap<SCGraphNodeId, Vec<SCGraphNodeId>> = HashMap::new();
        
        for edge in &sc_graph.edges {
            match edge.edge_type {
                EdgeType::S => {
                    s_outgoing.entry(edge.source).or_default().push(edge.target);
                    s_incoming.entry(edge.target).or_default().push(edge.source);
                }
                EdgeType::C => {
                    c_adjacent.entry(edge.source).or_default().push(edge.target);
                    c_adjacent.entry(edge.target).or_default().push(edge.source);
                }
            }
        }
        
        for &node_id in &sc_graph.nodes {
            self.write_indent(writer, 2)?;
            writeln!(writer, "hop_{:?} (function_{:?}):", node_id.hop_id, node_id.function_id)?;
            
            // S-edge outgoing
            if let Some(targets) = s_outgoing.get(&node_id) {
                self.write_indent(writer, 3)?;
                write!(writer, "S-outgoing: ")?;
                for (i, &target) in targets.iter().enumerate() {
                    if i > 0 { write!(writer, ", ")?; }
                    write!(writer, "hop_{:?}", target.hop_id)?;
                }
                writeln!(writer)?;
            }
            
            // S-edge incoming
            if let Some(sources) = s_incoming.get(&node_id) {
                self.write_indent(writer, 3)?;
                write!(writer, "S-incoming: ")?;
                for (i, &source) in sources.iter().enumerate() {
                    if i > 0 { write!(writer, ", ")?; }
                    write!(writer, "hop_{:?}", source.hop_id)?;
                }
                writeln!(writer)?;
            }
            
            // C-edge adjacent
            if let Some(adjacent) = c_adjacent.get(&node_id) {
                self.write_indent(writer, 3)?;
                write!(writer, "C-adjacent: ")?;
                for (i, &adj) in adjacent.iter().enumerate() {
                    if i > 0 { write!(writer, ", ")?; }
                    write!(writer, "hop_{:?}", adj.hop_id)?;
                }
                writeln!(writer)?;
            }
            
            // If no edges, indicate it
            let has_s_out = s_outgoing.contains_key(&node_id);
            let has_s_in = s_incoming.contains_key(&node_id);
            let has_c = c_adjacent.contains_key(&node_id);
            
            if !has_s_out && !has_s_in && !has_c {
                self.write_indent(writer, 3)?;
                writeln!(writer, "(isolated node)")?;
            }
        }
        
        Ok(())
    }

    /// Print a compact summary of the SC-Graph
    pub fn print_summary(&self, sc_graph: &SCGraph, writer: &mut dyn Write) -> std::io::Result<()> {
        let nodes_count = sc_graph.nodes.len();
        let s_edges_count = sc_graph.edges.iter().filter(|e| e.edge_type == EdgeType::S).count();
        let c_edges_count = sc_graph.edges.iter().filter(|e| e.edge_type == EdgeType::C).count();
        
        writeln!(writer, "SC-Graph Summary:")?;
        writeln!(writer, "  {} nodes (hops)", nodes_count)?;
        writeln!(writer, "  {} S-edges (sequential, directed)", s_edges_count)?;
        writeln!(writer, "  {} C-edges (conflict, undirected)", c_edges_count)?;
        writeln!(writer, "  Total edges: {}", s_edges_count + c_edges_count)?;
        
        if nodes_count > 0 {
            let density = (s_edges_count + c_edges_count) as f64 / (nodes_count * (nodes_count - 1)) as f64;
            writeln!(writer, "  Edge density: {:.3}", density)?;
        }
        
        Ok(())
    }
}

impl Default for SCGraphPrinter {
    fn default() -> Self {
        Self::new()
    }
}

impl PrettyPrinter<SCGraph> for SCGraphPrinter {
    fn print(&self, sc_graph: &SCGraph, writer: &mut dyn Write) -> std::io::Result<()> {
        self.print_sc_graph(sc_graph, writer)
    }
}
