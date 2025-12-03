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
    /// Interpolate color using lightness of orange from light (fast) to dark (slow) based on ratio [0, 1]
    fn interpolate_color(ratio: f64) -> String {
        let ratio = ratio.clamp(0.0, 1.0);
        // Orange base color: RGB(255, 165, 0)
        // Interpolate from light orange to dark orange by adjusting lightness
        // Light orange (ratio=0): #ff5e00ff (lighter)
        // Dark orange (ratio=1): #572100ff (darker)
        let r = (255.0 - ratio * (255.0 - 86.0)) as u8;
        let g = (214.0 - ratio * (214.0 - 33.0)) as u8;
        let b = 0 as u8;
        format!("#{:02X}{:02X}{:02X}", r, g, b)
    }

    /// Write SCGraph DOT using function names from Program.
    /// If c_edge_infos is provided, adds timing and elimination information with legend.
    pub fn to_dot<W: Write>(
        &self,
        program: &Program,
        c_edge_infos: Option<&[crate::cli::summary::CEdgeVerificationInfo]>,
        mut w: W,
    ) -> IoResult<()> {
        writeln!(w, "digraph SCGraph {{")?;
        writeln!(w, "  graph [rankdir=LR];")?;
        writeln!(
            w,
            "  node  [shape=record, fontsize=10, fontname=Helvetica];"
        )?;
        writeln!(w, "  edge  [fontname=Helvetica];")?;

        // If timing info is provided, calculate dynamic range and add legend
        let (edge_info_map, min_time_ms, max_time_ms) = if let Some(infos) = c_edge_infos {
            let mut map = std::collections::HashMap::new();
            let mut min_time = f64::MAX;
            let mut max_time = 0.0f64;

            for info in infos {
                let key = (
                    info.source_function_id,
                    info.source_instance,
                    info.source_hop_id,
                    info.target_function_id,
                    info.target_instance,
                    info.target_hop_id,
                );
                let time_ms = info.duration.as_secs_f64() * 1000.0;
                min_time = min_time.min(time_ms);
                max_time = max_time.max(time_ms);
                map.insert(key, info);
            }

            // Add legend with continuous color bar
            writeln!(w, "  // Legend for C-edge coloring")?;
            writeln!(w, "  subgraph cluster_legend {{")?;
            writeln!(w, "    label=\"Verification Time (ms)\";")?;
            writeln!(w, "    style=dashed;")?;

            // Create 6 color gradient boxes from green to red
            let num_legend_steps = 6;
            for i in 0..num_legend_steps {
                let ratio = i as f64 / (num_legend_steps - 1) as f64;
                let time_val = min_time + ratio * (max_time - min_time);
                let color = Self::interpolate_color(ratio);
                writeln!(
                    w,
                    "    legend_{} [label=\"{:.1}\", color=\"{}\", style=filled, shape=box];",
                    i, time_val, color
                )?;
            }

            // Link them horizontally
            write!(w, "    ")?;
            for i in 0..num_legend_steps - 1 {
                write!(w, "legend_{} -> ", i)?;
            }
            writeln!(w, "legend_{} [style=invis];", num_legend_steps - 1)?;

            // Add style legend
            writeln!(
                w,
                "    legend_eliminated [label=\"Eliminated\", style=\"dotted,bold\", shape=box];"
            )?;
            writeln!(
                w,
                "    legend_remaining [label=\"Remaining\", style=dashed, shape=box];"
            )?;
            writeln!(
                w,
                "    legend_eliminated -> legend_remaining [style=invis];"
            )?;
            writeln!(w, "  }}")?;

            (Some(map), min_time, max_time)
        } else {
            (None, 0.0, 1.0)
        };

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
                    // If timing info is available, use it
                    if let Some(ref map) = edge_info_map {
                        let key = (
                            e.source.function_id.index(),
                            e.source.instance,
                            e.source.hop_id.index(),
                            e.target.function_id.index(),
                            e.target.instance,
                            e.target.hop_id.index(),
                        );

                        if let Some(info) = map.get(&key) {
                            let duration_us = info.duration.as_micros();
                            let duration_ms = info.duration.as_secs_f64() * 1000.0;

                            // Calculate color based on position in dynamic range
                            let ratio = if max_time_ms > min_time_ms {
                                (duration_ms - min_time_ms) / (max_time_ms - min_time_ms)
                            } else {
                                0.5
                            };
                            let color = Self::interpolate_color(ratio);

                            // Determine style based on elimination (use bold for dotted to make it thicker)
                            let style = if info.eliminated {
                                "dotted,bold"
                            } else {
                                "dashed"
                            };
                            let penwidth = if info.eliminated { ",penwidth=2.0" } else { "" };

                            let label = if info.is_timeout {
                                "C (timeout)".to_string()
                            } else if duration_us < 1000 {
                                format!("C ({}µs)", duration_us)
                            } else {
                                format!("C ({:.1}ms)", duration_ms)
                            };

                            writeln!(
                                w,
                                "  {} -> {} [dir=none, style=\"{}\", color=\"{}\", label=\"{}\"{}];",
                                src, dst, style, color, label, penwidth
                            )?;
                        } else {
                            // Key not found in map - use default
                            writeln!(
                                w,
                                "  {} -> {} [dir=none, style=dashed, color=darkorange, label=\"C\"];",
                                src, dst
                            )?;
                        }
                    } else {
                        // No timing info provided - use default
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
        self.graph.to_dot(self.program, None, writer)
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
