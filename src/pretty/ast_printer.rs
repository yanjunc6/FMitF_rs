//! AST Pretty Printer

use crate::ast::Program;

/// Pretty printer for AST structures
pub struct AstPrinter {}

impl AstPrinter {
    /// Create a new AST printer with default settings
    pub fn new() -> Self {
        Self {}
    }

    /// Print a program to a string
    pub fn print_program(&mut self, program: &Program) -> String {
        format!("AST with {} declarations", program.declarations.len())
    }
}

impl Default for AstPrinter {
    fn default() -> Self {
        Self::new()
    }
}

/// Convenience function to pretty-print a program
pub fn print_program(program: &Program) -> String {
    let mut printer = AstPrinter::new();
    printer.print_program(program)
}
