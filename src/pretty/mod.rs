use std::io::Write;

// mod ast_printer;  // Temporarily disabled - needs update for new AST
mod ast_printer;
// mod cfg_printer;
// mod combined_dot_printer;
// mod dot_printer;
// mod sc_graph_printer;

// pub use ast_printer::{print_program, AstPrinter};  // Temporarily disabled
pub use ast_printer::{print_program_visitor, print_program_visitor_custom, AstPrinter};
// pub use cfg_printer::CfgPrinter;
// pub use combined_dot_printer::CombinedDotPrinter;
// pub use dot_printer::DotPrinter;
// pub use sc_graph_printer::SCGraphPrinter;

/// Unified trait for pretty printing different data structures.
/// All printers implement this trait to provide a consistent interface.
pub trait PrettyPrinter<T> {
    /// Print the data structure to the given writer.
    ///
    /// # Arguments
    /// * `data` - The data structure to print
    /// * `writer` - Where to write the output (stdout, file, etc.)
    ///
    /// # Returns
    /// Result indicating success or failure of the write operation
    fn print(&self, data: &T, writer: &mut dyn Write) -> std::io::Result<()>;

    /// Print the data structure to a string.
    /// This is a convenience method that uses print() internally.
    fn print_to_string(&self, data: &T) -> std::io::Result<String> {
        let mut buffer = Vec::new();
        self.print(data, &mut buffer)?;
        Ok(String::from_utf8_lossy(&buffer).to_string())
    }
}
