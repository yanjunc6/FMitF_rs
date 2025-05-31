// In pretty.rs or printer.rs
pub trait PrettyPrint {
    fn pretty_print(&self, printer: &mut Printer) -> fmt::Result;
}

pub struct Printer {
    indent: usize,
    width: usize,
    colored: bool,
    is_verbose: bool,
}

impl Printer {
    pub fn indent(&mut self) { self.indent += 2; }
    pub fn dedent(&mut self) { self.indent -= 2; }
    
    pub fn print_indented(&mut self, text: &str) -> fmt::Result {
        writeln!(&mut self.buffer, "{:indent$}{}", "", text, indent = self.indent)
    }
}
