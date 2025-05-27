use crate::graph::{EdgeType, SCGraph};
use std::io::Write;

pub fn print_dot_graph<W: Write>(graph: &SCGraph, writer: &mut W) -> std::io::Result<()> {
    writeln!(writer, "graph SCGraph {{")?;
    writeln!(writer, "  layout=neato;")?;
    writeln!(writer, "  node [shape=box, style=rounded];")?;
    writeln!(writer, "")?;

    // Output vertices (hops)
    for (i, hop) in graph.hops.iter().enumerate() {
        let function_name = match graph.get_function_name(i) {
            Some(name) => name.clone(),
            None => "unknown".to_string(),
        };
        let label = format!("Hop {}\\n{}\\n{}", i, function_name, hop.node.name);
        writeln!(writer, "  {} [label=\"{}\"];", i, label)?;
    }

    writeln!(writer, "")?;

    // Output edges (undirected)
    for edge in &graph.edges {
        let (color, style) = match edge.edge_type {
            EdgeType::S => ("blue", "solid"),
            EdgeType::C => ("red", "dashed"),
        };
        writeln!(
            writer,
            "  {} -- {} [color={}, style={}, label=\"{:?}\"];",
            edge.v1, edge.v2, color, style, edge.edge_type
        )?;
    }

    writeln!(writer, "}}")?;
    Ok(())
}

pub fn save_dot_file(graph: &SCGraph, filename: &str) -> std::io::Result<()> {
    use std::fs::File;

    let mut file = File::create(filename)?;
    print_dot_graph(graph, &mut file)
}
