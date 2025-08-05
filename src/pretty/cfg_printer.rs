use super::PrettyPrinter;
use crate::cfg::{
    BasicBlock, CfgProgram, EdgeType, FunctionCfg, HopCfg, LValue, Operand, Rvalue, Statement,
};
use std::io::Write;

/// Pretty printer for CFG structures.
/// Provides human-readable output of the control flow graph.
pub struct CfgPrinter {
    /// Indentation level for nested structures
    indent_size: usize,
    /// Whether to show detailed block information
    show_details: bool,
}

impl CfgPrinter {
    /// Creates a new CFG printer with default settings.
    pub fn new() -> Self {
        Self {
            indent_size: 2,
            show_details: true,
        }
    }

    /// Creates a new CFG printer with custom settings.
    pub fn with_options(indent_size: usize, show_details: bool) -> Self {
        Self {
            indent_size,
            show_details,
        }
    }

    /// Helper to write indentation
    fn write_indent(&self, writer: &mut dyn Write, level: usize) -> std::io::Result<()> {
        for _ in 0..(level * self.indent_size) {
            write!(writer, " ")?;
        }
        Ok(())
    }

    /// Print the entire CFG program
    fn print_program(&self, program: &CfgProgram, writer: &mut dyn Write) -> std::io::Result<()> {
        writeln!(writer, "CFG Program:")?;
        writeln!(writer)?;

        // Print tables
        if program.tables.len() > 0 {
            self.write_indent(writer, 1)?;
            writeln!(writer, "Tables:")?;
            for (table_id, table_info) in program.tables.iter() {
                self.write_indent(writer, 2)?;
                writeln!(writer, "table {} (id: {:?}) {{", table_info.name, table_id)?;

                self.write_indent(writer, 3)?;
                write!(writer, "fields: ")?;
                for (i, &field_id) in table_info.fields.iter().enumerate() {
                    if i > 0 {
                        write!(writer, ", ")?;
                    }
                    if let Some(field) = program.fields.get(field_id) {
                        write!(writer, "{}", field.name)?;
                    } else {
                        write!(writer, "field_{:?}", field_id)?;
                    }
                }
                writeln!(writer)?;

                if !table_info.primary_keys.is_empty() {
                    self.write_indent(writer, 3)?;
                    write!(writer, "primary_keys: ")?;
                    for (i, &pk_field_id) in table_info.primary_keys.iter().enumerate() {
                        if i > 0 {
                            write!(writer, ", ")?;
                        }
                        if let Some(field) = program.fields.get(pk_field_id) {
                            write!(writer, "{}", field.name)?;
                        } else {
                            write!(writer, "field_{:?}", pk_field_id)?;
                        }
                    }
                    writeln!(writer)?;
                }

                self.write_indent(writer, 2)?;
                writeln!(writer, "}}")?;
            }
            writeln!(writer)?;
        }

        // Print functions
        if program.functions.len() > 0 {
            self.write_indent(writer, 1)?;
            writeln!(writer, "Functions:")?;
            for (func_id, function) in program.functions.iter() {
                self.print_function(function, func_id, writer, 2)?;
                writeln!(writer)?;
            }
        }

        Ok(())
    }

    /// Print a function with its hops and control flow
    fn print_function(
        &self,
        function: &FunctionCfg,
        func_id: crate::cfg::FunctionId,
        writer: &mut dyn Write,
        level: usize,
    ) -> std::io::Result<()> {
        self.write_indent(writer, level)?;
        writeln!(
            writer,
            "function {} (id: {:?}) -> {:?} {{",
            function.name, func_id, function.return_type
        )?;

        // Print parameters
        if !function.parameters.is_empty() {
            self.write_indent(writer, level + 1)?;
            write!(writer, "parameters: ")?;
            for (i, &param_id) in function.parameters.iter().enumerate() {
                if i > 0 {
                    write!(writer, ", ")?;
                }
                write!(writer, "var_{:?}", param_id)?;
            }
            writeln!(writer)?;
        }

        // Print hop order
        if !function.hop_order.is_empty() {
            self.write_indent(writer, level + 1)?;
            write!(writer, "hop_order: ")?;
            for (i, &hop_id) in function.hop_order.iter().enumerate() {
                if i > 0 {
                    write!(writer, " -> ")?;
                }
                write!(writer, "hop_{:?}", hop_id)?;
            }
            writeln!(writer)?;
            writeln!(writer)?;
        }

        // Print hops
        for (hop_id, hop) in function.hops.iter() {
            self.print_hop(hop, hop_id, function, writer, level + 1)?;
            writeln!(writer)?;
        }

        self.write_indent(writer, level)?;
        writeln!(writer, "}}")?;
        Ok(())
    }

    /// Print a hop with its basic blocks
    fn print_hop(
        &self,
        hop: &HopCfg,
        hop_id: crate::cfg::HopId,
        function: &FunctionCfg,
        writer: &mut dyn Write,
        level: usize,
    ) -> std::io::Result<()> {
        self.write_indent(writer, level)?;
        writeln!(writer, "hop_{:?} {{", hop_id)?;

        if let Some(entry_block) = hop.entry_block {
            self.write_indent(writer, level + 1)?;
            writeln!(writer, "entry_block: block_{:?}", entry_block)?;
        }

        // Print all blocks in this hop
        for &block_id in &hop.blocks {
            if let Some(block) = function.blocks.get(block_id) {
                self.print_basic_block(block, block_id, writer, level + 1)?;
            }
        }

        self.write_indent(writer, level)?;
        writeln!(writer, "}}")?;
        Ok(())
    }

    /// Print a basic block with its statements and control flow
    fn print_basic_block(
        &self,
        block: &BasicBlock,
        block_id: crate::cfg::BasicBlockId,
        writer: &mut dyn Write,
        level: usize,
    ) -> std::io::Result<()> {
        self.write_indent(writer, level)?;
        writeln!(writer, "block_{:?} {{", block_id)?;

        // Print statements
        if !block.statements.is_empty() {
            self.write_indent(writer, level + 1)?;
            writeln!(writer, "statements:")?;
            for (i, stmt) in block.statements.iter().enumerate() {
                self.write_indent(writer, level + 2)?;
                write!(writer, "{}: ", i)?;
                self.print_statement(stmt, writer)?;
                writeln!(writer)?;
            }
        }

        // Print predecessors
        if !block.predecessors.is_empty() && self.show_details {
            self.write_indent(writer, level + 1)?;
            write!(writer, "predecessors: ")?;
            for (i, edge) in block.predecessors.iter().enumerate() {
                if i > 0 {
                    write!(writer, ", ")?;
                }
                write!(
                    writer,
                    "block_{:?} ({})",
                    edge.from,
                    self.edge_type_to_string(&edge.edge_type)
                )?;
            }
            writeln!(writer)?;
        }

        // Print successors
        if !block.successors.is_empty() && self.show_details {
            self.write_indent(writer, level + 1)?;
            write!(writer, "successors: ")?;
            for (i, edge) in block.successors.iter().enumerate() {
                if i > 0 {
                    write!(writer, ", ")?;
                }
                write!(
                    writer,
                    "block_{:?} ({})",
                    edge.to,
                    self.edge_type_to_string(&edge.edge_type)
                )?;
            }
            writeln!(writer)?;
        }

        self.write_indent(writer, level)?;
        writeln!(writer, "}}")?;
        Ok(())
    }

    /// Print a statement
    fn print_statement(&self, stmt: &Statement, writer: &mut dyn Write) -> std::io::Result<()> {
        match stmt {
            Statement::Assign { lvalue, rvalue, .. } => {
                write!(
                    writer,
                    "{} = {}",
                    self.lvalue_to_string(lvalue),
                    self.rvalue_to_string(rvalue)
                )?;
            }
        }
        Ok(())
    }

    /// Convert LValue to string representation
    fn lvalue_to_string(&self, lvalue: &LValue) -> String {
        match lvalue {
            LValue::Variable { var } => {
                format!("var_{:?}", var)
            }
            LValue::ArrayElement { array, index } => {
                format!("var_{:?}[{}]", array, self.operand_to_string(index))
            }
            LValue::TableField {
                table,
                pk_fields,
                pk_values,
                field,
            } => {
                let mut result = format!("table_{:?}.", table);
                for (i, &pk_field) in pk_fields.iter().enumerate() {
                    if i > 0 {
                        result.push(',');
                    }
                    result.push_str(&format!(
                        "field_{:?}[{}]",
                        pk_field,
                        self.operand_to_string(&pk_values[i])
                    ));
                }
                result.push_str(&format!(".field_{:?}", field));
                result
            }
        }
    }

    /// Convert edge type to string representation
    fn edge_type_to_string(&self, edge_type: &EdgeType) -> String {
        match edge_type {
            EdgeType::Unconditional => "unconditional".to_string(),
            EdgeType::ConditionalTrue { condition } => {
                format!("if {}", self.operand_to_string(condition))
            }
            EdgeType::ConditionalFalse { condition } => {
                format!("if !{}", self.operand_to_string(condition))
            }
            EdgeType::Return { value } => {
                if let Some(val) = value {
                    format!("return {}", self.operand_to_string(val))
                } else {
                    "return".to_string()
                }
            }
            EdgeType::Abort => "abort".to_string(),
            EdgeType::HopExit { next_hop } => {
                if let Some(hop_id) = next_hop {
                    format!("hop_exit -> hop_{:?}", hop_id)
                } else {
                    "hop_exit".to_string()
                }
            }
        }
    }

    /// Convert operand to string representation
    fn operand_to_string(&self, operand: &Operand) -> String {
        match operand {
            Operand::Var(var_id) => format!("var_{:?}", var_id),
            Operand::Const(constant) => format!("{:?}", constant),
        }
    }

    /// Convert rvalue to string representation
    fn rvalue_to_string(&self, rvalue: &Rvalue) -> String {
        match rvalue {
            Rvalue::Use(operand) => self.operand_to_string(operand),
            Rvalue::BinaryOp { op, left, right } => {
                format!(
                    "({} {:?} {})",
                    self.operand_to_string(left),
                    op,
                    self.operand_to_string(right)
                )
            }
            Rvalue::UnaryOp { op, operand } => {
                format!("({:?} {})", op, self.operand_to_string(operand))
            }
            Rvalue::TableAccess {
                table,
                pk_fields,
                pk_values,
                field,
            } => {
                let mut access = format!("table_{:?}", table);
                for (i, &pk_field) in pk_fields.iter().enumerate() {
                    access.push_str(&format!(
                        ".field_{:?}[{}]",
                        pk_field,
                        self.operand_to_string(&pk_values[i])
                    ));
                }
                access.push_str(&format!(".field_{:?}", field));
                access
            }
            Rvalue::ArrayAccess { array, index } => {
                format!(
                    "{}[{}]",
                    self.operand_to_string(array),
                    self.operand_to_string(index)
                )
            }
        }
    }
}

impl Default for CfgPrinter {
    fn default() -> Self {
        Self::new()
    }
}

impl PrettyPrinter<CfgProgram> for CfgPrinter {
    fn print(&self, program: &CfgProgram, writer: &mut dyn Write) -> std::io::Result<()> {
        self.print_program(program, writer)
    }
}

impl PrettyPrinter<FunctionCfg> for CfgPrinter {
    fn print(&self, function: &FunctionCfg, writer: &mut dyn Write) -> std::io::Result<()> {
        // For standalone function printing, we'll use a placeholder function ID
        // This is a workaround since we need a valid FunctionId but don't have one
        self.write_indent(writer, 0)?;
        writeln!(
            writer,
            "function {} -> {:?} {{",
            function.name, function.return_type
        )?;

        // Print parameters
        if !function.parameters.is_empty() {
            self.write_indent(writer, 1)?;
            write!(writer, "parameters: ")?;
            for (i, &param_id) in function.parameters.iter().enumerate() {
                if i > 0 {
                    write!(writer, ", ")?;
                }
                write!(writer, "var_{:?}", param_id)?;
            }
            writeln!(writer)?;
        }

        // Print hop order
        if !function.hop_order.is_empty() {
            self.write_indent(writer, 1)?;
            write!(writer, "hop_order: ")?;
            for (i, &hop_id) in function.hop_order.iter().enumerate() {
                if i > 0 {
                    write!(writer, " -> ")?;
                }
                write!(writer, "hop_{:?}", hop_id)?;
            }
            writeln!(writer)?;
            writeln!(writer)?;
        }

        // Print hops
        for (hop_id, hop) in function.hops.iter() {
            self.print_hop(hop, hop_id, function, writer, 1)?;
            writeln!(writer)?;
        }

        self.write_indent(writer, 0)?;
        writeln!(writer, "}}")?;
        Ok(())
    }
}
