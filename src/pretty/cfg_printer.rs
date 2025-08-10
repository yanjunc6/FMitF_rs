use super::PrettyPrinter;
use crate::cfg::{
    BasicBlockId, CfgProgram, CfgVisitor, FunctionCfg, FunctionId, HopId, LValue, Operand, Rvalue,
    Statement, StmtVisitor, VarId, Variable,
};
use std::cell::RefCell;
use std::io::Write;

/// Pretty printer for CFG structures using visitor pattern.
/// Provides human-readable output of the control flow graph.
pub struct CfgPrinter;

impl CfgPrinter {
    /// Creates a new CFG printer.
    pub fn new() -> Self {
        Self
    }
}

/// Internal visitor implementation with writer context
struct CfgPrintVisitor<'a> {
    writer: &'a mut dyn Write,
    program: &'a crate::cfg::CfgProgram,
    indent_level: RefCell<usize>,
    indent_size: usize,
}

impl<'a> CfgPrintVisitor<'a> {
    fn new(writer: &'a mut dyn Write, program: &'a crate::cfg::CfgProgram) -> Self {
        Self {
            writer,
            program,
            indent_level: RefCell::new(0),
            indent_size: 2,
        }
    }

    fn write_indent(&mut self) -> std::io::Result<()> {
        let level = *self.indent_level.borrow();
        for _ in 0..(level * self.indent_size) {
            write!(self.writer, " ")?;
        }
        Ok(())
    }

    fn increase_indent(&self) {
        *self.indent_level.borrow_mut() += 1;
    }

    fn decrease_indent(&self) {
        let mut level = self.indent_level.borrow_mut();
        if *level > 0 {
            *level -= 1;
        }
    }

    fn format_type(&self, ty: &crate::ast::TypeName) -> String {
        use crate::ast::TypeName;
        match ty {
            TypeName::Int => "int".to_string(),
            TypeName::Float => "float".to_string(),
            TypeName::String => "string".to_string(),
            TypeName::Bool => "bool".to_string(),
            TypeName::Array {
                element_type, size, ..
            } => match size {
                Some(n) => format!("{}[{}]", self.format_type(element_type), n),
                None => format!("{}[]", self.format_type(element_type)),
            },
            TypeName::Table(name) => name.clone(),
        }
    }
}

impl<'a> CfgVisitor<std::io::Result<()>> for CfgPrintVisitor<'a> {
    fn visit_program(&mut self, program: &CfgProgram) -> std::io::Result<()> {
        writeln!(self.writer, "CFG Program:")?;
        writeln!(self.writer)?;

        // Print global constants
        self.write_indent()?;
        writeln!(self.writer, "Global Constants:")?;
        self.increase_indent();
        for var_id in &program.root_variables {
            let var_info = &program.variables[*var_id];
            self.write_indent()?;
            writeln!(
                self.writer,
                "const {}: {}",
                var_info.name,
                self.format_type(&var_info.ty)
            )?;
        }
        self.decrease_indent();

        // Print tables
        let table_count = program.root_tables.len();
        if table_count > 0 {
            self.write_indent()?;
            writeln!(self.writer, "Tables ({} total):", table_count)?;
            self.increase_indent();
            for &table_id in &program.root_tables {
                if let Some(table_info) = program.tables.get(table_id) {
                    self.write_indent()?;
                    writeln!(
                        self.writer,
                        "table {} (id: {:?}) {{",
                        table_info.name, table_id
                    )?;
                    self.write_indent()?;
                    writeln!(self.writer, "}}")?;
                }
            }
            self.decrease_indent();
            writeln!(self.writer)?;
        }

        // Print functions
        let func_count = program.root_functions.len();
        if func_count > 0 {
            self.write_indent()?;
            writeln!(self.writer, "Functions ({} total):", func_count)?;
            self.increase_indent();
            for &func_id in &program.root_functions {
                if program.functions.get(func_id).is_some() {
                    self.visit_function(program, func_id)?;
                    writeln!(self.writer)?;
                }
            }
            self.decrease_indent();
        }

        Ok(())
    }

    fn visit_function(&mut self, program: &CfgProgram, id: FunctionId) -> std::io::Result<()> {
        let function = &program.functions[id];

        self.write_indent()?;
        writeln!(
            self.writer,
            "function {} (id: {:?}) -> {:?} {{",
            function.name, id, function.return_type
        )?;

        // Print hops
        self.increase_indent();
        for &hop_id in &function.hops {
            self.visit_hop(program, hop_id)?;
        }
        self.decrease_indent();

        self.write_indent()?;
        writeln!(self.writer, "}}")?;
        Ok(())
    }

    fn visit_hop(&mut self, program: &CfgProgram, id: HopId) -> std::io::Result<()> {
        let hop = &program.hops[id];

        self.write_indent()?;
        writeln!(self.writer, "hop_{:?} (func: {:?}) {{", id, hop.function_id)?;

        // Print all blocks in this hop
        self.increase_indent();
        for &block_id in &hop.blocks {
            self.visit_basic_block(program, block_id)?;
        }
        self.decrease_indent();

        self.write_indent()?;
        writeln!(self.writer, "}}")?;
        Ok(())
    }

    fn visit_basic_block(&mut self, program: &CfgProgram, id: BasicBlockId) -> std::io::Result<()> {
        let block = &program.blocks[id];

        self.write_indent()?;
        writeln!(
            self.writer,
            "block{} (hop: {:?}) {{",
            id.index(),
            block.hop_id
        )?;

        self.increase_indent();

        // Print all statements in this block using the statement visitor
        for (stmt_index, stmt) in block.statements.iter().enumerate() {
            self.write_indent()?;
            let stmt_str = self.visit_statement(stmt);
            writeln!(self.writer, "[{}] {}", stmt_index, stmt_str)?;
        }

        // Print control flow edges
        if !block.successors.is_empty() {
            self.write_indent()?;
            writeln!(self.writer, "successors:")?;
            self.increase_indent();
            for edge in &block.successors {
                self.write_indent()?;
                writeln!(
                    self.writer,
                    "-> block{} ({:?})",
                    edge.to.index(),
                    edge.edge_type
                )?;
            }
            self.decrease_indent();
        }

        self.decrease_indent();

        self.write_indent()?;
        writeln!(self.writer, "}}")?;
        Ok(())
    }

    fn visit_global_constants(&mut self, program: &CfgProgram, id: VarId) -> std::io::Result<()> {
        if let Some(var) = program.variables.get(id) {
            write!(self.writer, "{}: {}", var.name, self.format_type(&var.ty))?;
        }
        Ok(())
    }
}

impl<'a> StmtVisitor<String> for CfgPrintVisitor<'a> {
    fn visit_statement(&mut self, stmt: &Statement) -> String {
        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => {
                let lv_str = self.visit_lvalue(lvalue);
                let rv_str = self.visit_rvalue(rvalue);
                format!("{} = {}", lv_str, rv_str)
            }
        }
    }

    fn visit_lvalue(&mut self, lv: &LValue) -> String {
        match lv {
            LValue::Variable { var } => {
                format!("var_{:?}", var.index())
            }
            LValue::ArrayElement { array, index } => {
                let index_str = self.visit_operand(index);
                format!("var_{:?}[{}]", array.index(), index_str)
            }
            LValue::TableField {
                table,
                pk_values,
                field,
                ..
            } => {
                let pk_strs: Vec<String> =
                    pk_values.iter().map(|op| self.visit_operand(op)).collect();
                let table_name = self
                    .program
                    .tables
                    .get(*table)
                    .map(|t| t.name.as_str())
                    .unwrap_or("<unknown_table>");
                let field_name = self
                    .program
                    .fields
                    .get(*field)
                    .map(|f| f.name.as_str())
                    .unwrap_or("<unknown_field>");
                format!("{}[{}].{}", table_name, pk_strs.join(", "), field_name)
            }
        }
    }

    fn visit_rvalue(&mut self, rv: &Rvalue) -> String {
        match rv {
            Rvalue::Use(operand) => self.visit_operand(operand),
            Rvalue::TableAccess {
                table,
                pk_values,
                field,
                ..
            } => {
                let pk_strs: Vec<String> =
                    pk_values.iter().map(|op| self.visit_operand(op)).collect();
                let table_name = self
                    .program
                    .tables
                    .get(*table)
                    .map(|t| t.name.as_str())
                    .unwrap_or("<unknown_table>");
                let field_name = self
                    .program
                    .fields
                    .get(*field)
                    .map(|f| f.name.as_str())
                    .unwrap_or("<unknown_field>");
                format!("{}[{}].{}", table_name, pk_strs.join(", "), field_name)
            }
            Rvalue::ArrayAccess { array, index } => {
                let array_str = self.visit_operand(array);
                let index_str = self.visit_operand(index);
                format!("{}[{}]", array_str, index_str)
            }
            Rvalue::UnaryOp { op, operand } => {
                let operand_str = self.visit_operand(operand);
                format!("{:?}({})", op, operand_str)
            }
            Rvalue::BinaryOp { op, left, right } => {
                let left_str = self.visit_operand(left);
                let right_str = self.visit_operand(right);
                format!("({} {:?} {})", left_str, op, right_str)
            }
        }
    }

    fn visit_operand(&mut self, op: &Operand) -> String {
        match op {
            Operand::Var(var_id) => format!("var_{:?}", var_id.index()),
            Operand::Const(constant) => self.visit_constant(constant),
        }
    }

    fn visit_constant(&mut self, c: &crate::cfg::Constant) -> String {
        match c {
            crate::cfg::Constant::Int(i) => i.to_string(),
            crate::cfg::Constant::Float(f) => f.to_string(),
            crate::cfg::Constant::Bool(b) => b.to_string(),
            crate::cfg::Constant::String(s) => format!("\"{}\"", s),
            crate::cfg::Constant::Array(arr) => {
                let elements: Vec<String> = arr.iter().map(|c| self.visit_constant(c)).collect();
                format!("[{}]", elements.join(", "))
            }
        }
    }

    fn visit_variable(&mut self, _program: &CfgProgram, v: &Variable) -> String {
        v.name.clone()
    }
}

impl PrettyPrinter<CfgProgram> for CfgPrinter {
    fn print(&self, program: &CfgProgram, writer: &mut dyn Write) -> std::io::Result<()> {
        let mut visitor = CfgPrintVisitor::new(writer, program);
        visitor.visit_program(program)
    }
}

impl PrettyPrinter<FunctionCfg> for CfgPrinter {
    fn print(&self, function: &FunctionCfg, writer: &mut dyn Write) -> std::io::Result<()> {
        writeln!(
            writer,
            "function {} -> {:?} {{",
            function.name, function.return_type
        )?;
        writeln!(writer, "  // {} hops", function.hops.len())?;
        writeln!(writer, "}}")?;
        Ok(())
    }
}
