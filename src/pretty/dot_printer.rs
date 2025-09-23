//! DOT format printer for SCGraph and CombinedSCGraph.
//!
//! Both graph types are emitted with a similar style so they can be visually
//! compared. Instance numbers are encoded in node names to distinguish
//! parallel instances of the same transaction.

use std::io::{Result as IoResult, Write};

use super::{EdgeType, SCGraph, SCGraphNodeId, CombinedSCGraph, CombinedVertex};

/// Escape a string for DOT label usage if needed (currently numbers only so trivial).
fn escape_label(s: &str) -> String { s.replace('"', "\\\"") }

/// Produce a stable string id for an SC graph node usable as DOT node id.
fn sc_node_id(n: &SCGraphNodeId) -> String {
    format!("f{}_i{}_h{}", n.function_id.index(), n.instance, n.hop_id.index())
}

/// Produce a human readable label for an SC graph node.

/// Write SCGraph in DOT format.
pub fn write_scgraph_dot<W: Write>(g: &SCGraph, mut w: W) -> IoResult<()> {
    // We treat S-edges as directed; C-edges as undirected. Because DOT cannot mix
    // directed/undirected inside the same graph perfectly, we emit a digraph and
    // represent C-edges as bidirectional edges with style=dashed (only one edge).
    writeln!(w, "digraph SCGraph {{")?;
    writeln!(w, "  graph [rankdir=LR];")?;
    writeln!(w, "  node  [shape=record, fontsize=10, fontname=Helvetica];")?;
    writeln!(w, "  edge  [fontname=Helvetica];")?;

    // Sort nodes for determinism.
    let mut nodes: Vec<_> = g.nodes.iter().collect();
    nodes.sort_by_key(|n| (n.function_id.index(), n.instance, n.hop_id.index()));
    for n in nodes.iter() {
        // Legacy raw SC graph node: keep old labeling minimal (function id + hop) since final spec focuses on combined graph.
        writeln!(w, "  {} [label=\"f{}#{}\\nHop {}\"];", sc_node_id(n), n.function_id.index(), n.instance, n.hop_id.index())?;
    }

    // Sort edges for determinism.
    let mut edges = g.edges.clone();
    edges.sort_by_key(|e| (match e.edge_type { EdgeType::S => 0usize, EdgeType::C => 1usize },
                           e.source.function_id.index(), e.source.instance, e.source.hop_id.index(),
                           e.target.function_id.index(), e.target.instance, e.target.hop_id.index()));

    for e in edges.iter() {
        match e.edge_type {
            EdgeType::S => {
                writeln!(w, "  {} -> {} [color=black, label=\"S\"];", sc_node_id(&e.source), sc_node_id(&e.target))?;
            }
            EdgeType::C => {
                // Emit an undirected style using dir=none dashed
                writeln!(w, "  {} -> {} [dir=none, style=dashed, color=darkorange, label=\"C\"];",
                    sc_node_id(&e.source), sc_node_id(&e.target))?;
            }
        }
    }

    writeln!(w, "}}")?;
    Ok(())
}

// ---------------- CombinedSCGraph DOT -----------------

/// Produce a stable DOT node id for a combined vertex.
fn combined_vertex_id(v: &CombinedVertex) -> String { format!("cv{}", v.id) }

/// Human label: aggregate pieces.
fn combined_vertex_label(v: &CombinedVertex, function_name: &str) -> String {
    // Spec: three lines: line1 functionName#instance, line2 Hop list ("Hop 1" or "Hop 1,2,3"), line3 reserved (empty for now).
    // Since each CombinedVertex currently has exactly one piece, we use that.
    let p = &v.pieces[0];
    let mut hop_numbers: Vec<String> = p.hop_ids.iter().map(|h| h.index().to_string()).collect();
    hop_numbers.sort();
    let hop_line = if hop_numbers.len() == 1 {
        format!("Hop {}", hop_numbers[0])
    } else {
        format!("Hop {}", hop_numbers.join(", "))
    };
    format!("{}#{}\\n{}\\n", function_name, p.instance, hop_line)
}

pub fn write_combined_scgraph_dot<W: Write>(g: &CombinedSCGraph, mut w: W, func_name_lookup: &dyn Fn(usize) -> String) -> IoResult<()> {
    writeln!(w, "digraph CombinedSCGraph {{")?;
    writeln!(w, "  graph [rankdir=LR];")?;
    writeln!(w, "  node  [shape=record, fontsize=10, fontname=Helvetica];")?;
    writeln!(w, "  edge  [fontname=Helvetica];")?;

    // Vertices
    for v in g.vertices.iter() {
        let p = &v.pieces[0];
        let fname = func_name_lookup(p.function_id.index());
        let label = combined_vertex_label(v, &fname);
        writeln!(w, "  {} [label=\"{}\"];", combined_vertex_id(v), escape_label(&label))?;
    }

    // Edges: S directed, C undirected dashed
    let mut edges = g.edges.clone();
    edges.sort_by_key(|e| (match e.edge_type { EdgeType::S => 0usize, EdgeType::C => 1usize }, e.source, e.target));
    for e in edges.iter() {
        let src = format!("cv{}", e.source);
        let dst = format!("cv{}", e.target);
        match e.edge_type {
            EdgeType::S => writeln!(w, "  {} -> {} [color=black, label=\"S\"];", src, dst)?,
            EdgeType::C => writeln!(w, "  {} -> {} [dir=none, style=dashed, color=darkorange, label=\"C\"];", src, dst)?,
        }
    }

    writeln!(w, "}}")?;
    Ok(())
}
