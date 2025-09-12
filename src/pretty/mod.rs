use std::io::{self, Write};

pub mod ast_printer;

// Re-export for convenience
pub use ast_printer::AstPrinter;

// The minimal trait. It simply requires a type to know how to write
// itself to any destination that implements `io::Write`.
pub trait PrettyPrint {
    // The implementer is now fully responsible for all formatting.
    fn pretty_print(&self, writer: &mut impl Write) -> io::Result<()>;
}
