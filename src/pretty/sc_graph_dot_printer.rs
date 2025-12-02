//! DOT pretty printer implementations for SCGraph and CombinedSCGraph.
//!
//! Node label spec (CombinedSCGraph):
//!   Line 1: functionName#instance
//!   Line 2: Hop list -> "Hop 1" or "Hop 1, 2, 3"
//!   Line 3: (left blank for future use)
//! Raw SCGraph nodes use a simpler label (functionName#instance / single hop) to aid debugging.

use std::io::{Result as IoResult, Write};

use crate::cfg::{FunctionId, Program};
use crate::pretty::PrettyPrint;
use crate::sc_graph::{CombinedSCGraph, EdgeType, SCGraph};

/// Helper: escape quotes in labels.
fn esc(s: &str) -> String {
    s.replace('"', "\\\"")
}

/// Gather function name by id.
fn func_name(program: &Program, fid: FunctionId) -> &str {
    &program.functions[fid].name
}

impl SCGraph {
    /// Write SCGraph DOT with timing and elimination information
    pub fn to_dot_plotted<W: Write>(
        &self,
        program: &Program,
        c_edge_infos: &[crate::cli::summary::CEdgeVerificationInfo],
        mut w: W,
    ) -> IoResult<()> {
        writeln!(w, "digraph SCGraph {{")?;
        writeln!(w, "  graph [rankdir=LR];")?;
        writeln!(
            w,
            "  node  [shape=record, fontsize=10, fontname=Helvetica];"
        )?;
        writeln!(w, "  edge  [fontname=Helvetica];")?;

        // Add legend
        writeln!(w, "  // Legend for C-edge coloring")?;
        writeln!(w, "  subgraph cluster_legend {{")?;
        writeln!(w, "    label=\"Verification Time\";")?;
        writeln!(w, "    style=dashed;")?;
        writeln!(
            w,
            "    legend_fast [label=\"Fast (<1ms)\", color=\"#00FF00\", style=filled, shape=box];"
        )?;
        writeln!(w, "    legend_medium [label=\"Medium (1-10ms)\", color=\"#FFFF00\", style=filled, shape=box];")?;
        writeln!(w, "    legend_slow [label=\"Slow (10-100ms)\", color=\"#FFA500\", style=filled, shape=box];")?;
        writeln!(w, "    legend_very_slow [label=\"Very Slow (>100ms)\", color=\"#FF0000\", style=filled, shape=box];")?;
        writeln!(
            w,
            "    legend_eliminated [label=\"Eliminated (dotted)\", style=dotted, shape=box];"
        )?;
        writeln!(
            w,
            "    legend_remaining [label=\"Remaining (dashed)\", style=dashed, shape=box];"
        )?;
        writeln!(
            w,
            "    legend_fast -> legend_medium -> legend_slow -> legend_very_slow [style=invis];"
        )?;
        writeln!(
            w,
            "    legend_eliminated -> legend_remaining [style=invis];"
        )?;
        writeln!(w, "  }}")?;

        // Build a map from C-edge to its info
        let mut edge_info_map = std::collections::HashMap::new();
        for info in c_edge_infos {
            let key = (
                info.source_function_id,
                info.source_instance,
                info.source_hop_id,
                info.target_function_id,
                info.target_instance,
                info.target_hop_id,
            );
            edge_info_map.insert(key, info);
        }

        // Stable order
        let mut nodes: Vec<_> = self.nodes.iter().collect();
        nodes.sort_by_key(|n| (n.function_id.index(), n.instance, n.hop_id.index()));

        for n in nodes.iter() {
            let name = func_name(program, n.function_id);
            writeln!(
                w,
                "  f{}_i{}_h{} [label=\"{}#{}\\nHop {}\\n\"];",
                n.function_id.index(),
                n.instance,
                n.hop_id.index(),
                esc(name),
                n.instance,
                n.hop_id.index()
            )?;
        }

        let mut edges = self.edges.clone();
        edges.sort_by_key(|e| {
            (
                match e.edge_type {
                    EdgeType::S => 0usize,
                    EdgeType::C => 1usize,
                },
                e.source.function_id.index(),
                e.source.instance,
                e.source.hop_id.index(),
                e.target.function_id.index(),
                e.target.instance,
                e.target.hop_id.index(),
            )
        });

        for e in edges.iter() {
            let src = format!(
                "f{}_i{}_h{}",
                e.source.function_id.index(),
                e.source.instance,
                e.source.hop_id.index()
            );
            let dst = format!(
                "f{}_i{}_h{}",
                e.target.function_id.index(),
                e.target.instance,
                e.target.hop_id.index()
            );

            match e.edge_type {
                EdgeType::S => writeln!(w, "  {} -> {} [color=black, label=\"S\"];", src, dst)?,
                EdgeType::C => {
                    // Look up timing info
                    let key = (
                        e.source.function_id.index(),
                        e.source.instance,
                        e.source.hop_id.index(),
                        e.target.function_id.index(),
                        e.target.instance,
                        e.target.hop_id.index(),
                    );

                    if let Some(info) = edge_info_map.get(&key) {
                        let duration_us = info.duration.as_micros();
                        let duration_ms = info.duration.as_secs_f64() * 1000.0;

                        // Determine color based on time
                        let color = if duration_ms < 1.0 {
                            "#00FF00" // Green - fast
                        } else if duration_ms < 10.0 {
                            "#FFFF00" // Yellow - medium
                        } else if duration_ms < 100.0 {
                            "#FFA500" // Orange - slow
                        } else {
                            "#FF0000" // Red - very slow
                        };

                        // Determine style based on elimination
                        let style = if info.eliminated { "dotted" } else { "dashed" };

                        let label = if info.is_timeout {
                            "C (timeout)".to_string()
                        } else if duration_us < 1000 {
                            format!("C ({}µs)", duration_us)
                        } else {
                            format!("C ({:.1}ms)", duration_ms)
                        };

                        writeln!(
                            w,
                            "  {} -> {} [dir=none, style={}, color=\"{}\", label=\"{}\"];",
                            src, dst, style, color, label
                        )?;
                    } else {
                        // No timing info available - use default
                        writeln!(
                            w,
                            "  {} -> {} [dir=none, style=dashed, color=darkorange, label=\"C\"];",
                            src, dst
                        )?;
                    }
                }
            }
        }
        writeln!(w, "}}")?;
        Ok(())
    }

    /// Write SCGraph DOT using function names from Program.
    pub fn to_dot<W: Write>(&self, program: &Program, mut w: W) -> IoResult<()> {
        writeln!(w, "digraph SCGraph {{")?;
        writeln!(w, "  graph [rankdir=LR];")?;
        writeln!(
            w,
            "  node  [shape=record, fontsize=10, fontname=Helvetica];"
        )?;
        writeln!(w, "  edge  [fontname=Helvetica];")?;

        // Stable order
        let mut nodes: Vec<_> = self.nodes.iter().collect();
        nodes.sort_by_key(|n| (n.function_id.index(), n.instance, n.hop_id.index()));

        for n in nodes.iter() {
            let name = func_name(program, n.function_id);
            writeln!(
                w,
                "  f{}_i{}_h{} [label=\"{}#{}\\nHop {}\\n\"];",
                n.function_id.index(),
                n.instance,
                n.hop_id.index(),
                esc(name),
                n.instance,
                n.hop_id.index()
            )?;
        }

        let mut edges = self.edges.clone();
        edges.sort_by_key(|e| {
            (
                match e.edge_type {
                    EdgeType::S => 0usize,
                    EdgeType::C => 1usize,
                },
                e.source.function_id.index(),
                e.source.instance,
                e.source.hop_id.index(),
                e.target.function_id.index(),
                e.target.instance,
                e.target.hop_id.index(),
            )
        });
        for e in edges.iter() {
            let src = format!(
                "f{}_i{}_h{}",
                e.source.function_id.index(),
                e.source.instance,
                e.source.hop_id.index()
            );
            let dst = format!(
                "f{}_i{}_h{}",
                e.target.function_id.index(),
                e.target.instance,
                e.target.hop_id.index()
            );
            match e.edge_type {
                EdgeType::S => writeln!(w, "  {} -> {} [color=black, label=\"S\"];", src, dst)?,
                EdgeType::C => writeln!(
                    w,
                    "  {} -> {} [dir=none, style=dashed, color=darkorange, label=\"C\"];",
                    src, dst
                )?,
            }
        }
        writeln!(w, "}}")?;
        Ok(())
    }
}

impl CombinedSCGraph {
    pub fn to_dot<W: Write>(&self, program: &Program, mut w: W) -> IoResult<()> {
        writeln!(w, "digraph CombinedSCGraph {{")?;
        writeln!(w, "  graph [rankdir=LR];")?;
        writeln!(
            w,
            "  node  [shape=record, fontsize=10, fontname=Helvetica];"
        )?;
        writeln!(w, "  edge  [fontname=Helvetica];")?;

        for v in &self.vertices {
            let piece = &v.pieces[0]; // current design: one piece per vertex
            let name = func_name(program, piece.function_id);
            let mut hop_nums: Vec<_> = piece.hop_ids.iter().map(|h| h.index()).collect();
            hop_nums.sort();
            let hop_line = if hop_nums.len() == 1 {
                format!("Hop {}", hop_nums[0])
            } else {
                format!(
                    "Hop {}",
                    hop_nums
                        .iter()
                        .map(|h| h.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            };
            let label = format!("{}#{}\\n{}\\n", name, piece.instance, hop_line);
            writeln!(w, "  cv{} [label=\"{}\"];", v.id, esc(&label))?;
        }

        let mut edges = self.edges.clone();
        edges.sort_by_key(|e| {
            (
                match e.edge_type {
                    EdgeType::S => 0usize,
                    EdgeType::C => 1usize,
                },
                e.source,
                e.target,
            )
        });
        for e in edges.iter() {
            let src = format!("cv{}", e.source);
            let dst = format!("cv{}", e.target);
            match e.edge_type {
                EdgeType::S => writeln!(w, "  {} -> {} [color=black, label=\"S\"];", src, dst)?,
                EdgeType::C => writeln!(
                    w,
                    "  {} -> {} [dir=none, style=dashed, color=darkorange, label=\"C\"];",
                    src, dst
                )?,
            }
        }
        writeln!(w, "}}")?;
        Ok(())
    }
}

// PrettyPrint trait implementations (context-free versions produce raw SCGraph without names)
// We add context-taking helper traits for convenience.

pub struct SCGraphDotPrinter<'a> {
    pub graph: &'a SCGraph,
    pub program: &'a Program,
}
pub struct CombinedSCGraphDotPrinter<'a> {
    pub graph: &'a CombinedSCGraph,
    pub program: &'a Program,
}

impl<'a> PrettyPrint for SCGraphDotPrinter<'a> {
    fn pretty_print(&self, writer: &mut impl Write) -> IoResult<()> {
        self.graph.to_dot(self.program, writer)
    }
}

impl<'a> PrettyPrint for CombinedSCGraphDotPrinter<'a> {
    fn pretty_print(&self, writer: &mut impl Write) -> IoResult<()> {
        self.graph.to_dot(self.program, writer)
    }
}

// Helper constructors
impl<'a> SCGraphDotPrinter<'a> {
    pub fn new(graph: &'a SCGraph, program: &'a Program) -> Self {
        Self { graph, program }
    }
}
impl<'a> CombinedSCGraphDotPrinter<'a> {
    pub fn new(graph: &'a CombinedSCGraph, program: &'a Program) -> Self {
        Self { graph, program }
    }
}
