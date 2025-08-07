use super::PrettyPrinter;
use crate::cfg::{
    BasicBlock, CfgProgram, EdgeType, FunctionCfg, HopCfg, LValue, Operand, Rvalue, Statement,
    VarId, VariableKind,
};
use std::collections::HashMap;
use std::io::Write;

/// Pretty printer for CFG structures.
/// Provides human-readable output of the control flow graph.
pub struct CfgPrinter<'a> {
    /// Indentation level for nested structures
    indent_size: usize,
    /// Whether to show detailed block information
    show_details: bool,
    /// Reference to the full program for variable name lookup
    program: Option<&'a CfgProgram>,
    /// Counter for generating temporary variable names
    temp_counter: std::cell::RefCell<usize>,
    /// Map from VarId to generated names for temporaries
    temp_name_map: std::cell::RefCell<HashMap<VarId, String>>,
}

impl<'a> CfgPrinter<'a> {
    /// Creates a new CFG printer with default settings.
    pub fn new() -> Self {
        Self {
            indent_size: 2,
            show_details: true,
            program: None,
            temp_counter: std::cell::RefCell::new(1),
            temp_name_map: std::cell::RefCell::new(HashMap::new()),
        }
    }

    /// Creates a new CFG printer with a program reference for variable name lookup.
    pub fn with_program(program: &'a CfgProgram) -> Self {
        Self {
            indent_size: 2,
            show_details: true,
            program: Some(program),
            temp_counter: std::cell::RefCell::new(1),
            temp_name_map: std::cell::RefCell::new(HashMap::new()),
        }
    }

    /// Creates a new CFG printer with custom settings.
    pub fn with_options(indent_size: usize, show_details: bool) -> Self {
        Self {
            indent_size,
            show_details,
            program: None,
            temp_counter: std::cell::RefCell::new(1),
            temp_name_map: std::cell::RefCell::new(HashMap::new()),
        }
    }

    /// Get a human-readable name for a variable
    fn get_variable_name(&self, var_id: VarId) -> String {
        if let Some(program) = self.program {
            if let Some(var) = program.variables.get(var_id) {
                // Use the actual variable name if available
                if var.name.is_empty() || var.kind == VariableKind::Temporary {
                    // Generate a temporary name for unnamed or temporary variables
                    let mut temp_map = self.temp_name_map.borrow_mut();
                    if let Some(name) = temp_map.get(&var_id) {
                        name.clone()
                    } else {
                        let mut counter = self.temp_counter.borrow_mut();
                        let temp_name = format!("t{}", *counter);
                        *counter += 1;
                        temp_map.insert(var_id, temp_name.clone());
                        temp_name
                    }
                } else {
                    var.name.clone()
                }
            } else {
                format!("var_{:?}", var_id)
            }
        } else {
            // Fallback when no program reference is available
            format!("var_{:?}", var_id)
        }
    }

    /// Get type information for a variable
    fn get_variable_type(&self, var_id: VarId) -> Option<String> {
        if let Some(program) = self.program {
            if let Some(var) = program.variables.get(var_id) {
                Some(self.format_type(&var.ty))
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Format a type name
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
                self.print_function(program, function, func_id, writer, 2)?;
                writeln!(writer)?;
            }
        }

        Ok(())
    }

    /// Print a function with its hops and control flow
    fn print_function(
        &self,
        program: &CfgProgram,
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
                let param_name = self.get_variable_name(param_id);
                if let Some(param_type) = self.get_variable_type(param_id) {
                    write!(writer, "{}: {}", param_name, param_type)?;
                } else {
                    write!(writer, "{}", param_name)?;
                }
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
                write!(writer, "hop{}", hop_id.index())?;
            }
            writeln!(writer)?;
            writeln!(writer)?;
        }

        // Print hops
        for (hop_id, hop) in crate::cfg::cfg_api::function_hops_iter(program, func_id) {
            self.print_hop(program, hop, hop_id, function, writer, level + 1)?;
            writeln!(writer)?;
        }

        self.write_indent(writer, level)?;
        writeln!(writer, "}}")?;
        Ok(())
    }

    /// Print a hop with its basic blocks
    fn print_hop(
        &self,
        program: &CfgProgram,
        hop: &HopCfg,
        hop_id: crate::cfg::HopId,
        _function: &FunctionCfg,
        writer: &mut dyn Write,
        level: usize,
    ) -> std::io::Result<()> {
        self.write_indent(writer, level)?;
        writeln!(writer, "hop_{:?} {{", hop_id)?;

        if let Some(entry_block) = hop.entry_block {
            self.write_indent(writer, level + 1)?;
            writeln!(writer, "entry_block: block{}", entry_block.index())?;
        }

        // Print all blocks in this hop
        for &block_id in crate::cfg::cfg_api::get_hop_blocks(program, hop_id) {
            let block = crate::cfg::cfg_api::get_basic_block(program, block_id);
            self.print_basic_block(block, block_id, writer, level + 1)?;
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
        writeln!(writer, "block{} {{", block_id.index())?;

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
                    "block{} ({})",
                    edge.from.index(),
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
                    "block{} ({})",
                    edge.to.index(),
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

                // Add type information if available
                if let LValue::Variable { var } = lvalue {
                    if let Some(ty_str) = self.get_variable_type(*var) {
                        write!(writer, "  // : {}", ty_str)?;
                    }
                }
            }
        }
        Ok(())
    }

    /// Convert LValue to string representation
    fn lvalue_to_string(&self, lvalue: &LValue) -> String {
        match lvalue {
            LValue::Variable { var } => self.get_variable_name(*var),
            LValue::ArrayElement { array, index } => {
                format!(
                    "{}[{}]",
                    self.get_variable_name(*array),
                    self.operand_to_string(index)
                )
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
            Operand::Var(var_id) => self.get_variable_name(*var_id),
            Operand::Const(constant) => {
                use crate::cfg::Constant;
                match constant {
                    Constant::Int(n) => n.to_string(),
                    Constant::Float(f) => f.to_string(),
                    Constant::String(s) => format!("\"{}\"", s),
                    Constant::Bool(b) => b.to_string(),
                    Constant::Array(arr) => format!("{:?}", arr), // Fallback for arrays
                }
            }
        }
    }

    /// Convert rvalue to string representation
    fn rvalue_to_string(&self, rvalue: &Rvalue) -> String {
        match rvalue {
            Rvalue::Use(operand) => self.operand_to_string(operand),
            Rvalue::BinaryOp { op, left, right } => {
                format!(
                    "({} {} {})",
                    self.operand_to_string(left),
                    self.binary_op_to_string(op),
                    self.operand_to_string(right)
                )
            }
            Rvalue::UnaryOp { op, operand } => {
                format!(
                    "({}{})",
                    self.unary_op_to_string(op),
                    self.operand_to_string(operand)
                )
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

    /// Convert binary operator to string
    fn binary_op_to_string(&self, op: &crate::ast::BinaryOp) -> String {
        use crate::ast::BinaryOp;
        match op {
            BinaryOp::Add => "+".to_string(),
            BinaryOp::Sub => "-".to_string(),
            BinaryOp::Mul => "*".to_string(),
            BinaryOp::Div => "/".to_string(),
            BinaryOp::Eq => "==".to_string(),
            BinaryOp::Neq => "!=".to_string(),
            BinaryOp::Lt => "<".to_string(),
            BinaryOp::Lte => "<=".to_string(),
            BinaryOp::Gt => ">".to_string(),
            BinaryOp::Gte => ">=".to_string(),
            BinaryOp::And => "&&".to_string(),
            BinaryOp::Or => "||".to_string(),
        }
    }

    /// Convert unary operator to string
    fn unary_op_to_string(&self, op: &crate::ast::UnaryOp) -> String {
        use crate::ast::UnaryOp;
        match op {
            UnaryOp::Not => "!".to_string(),
            UnaryOp::Neg => "-".to_string(),
            UnaryOp::PreIncrement => "++".to_string(),
            UnaryOp::PostIncrement => "++".to_string(),
            UnaryOp::PreDecrement => "--".to_string(),
            UnaryOp::PostDecrement => "--".to_string(),
        }
    }
}

impl<'a> Default for CfgPrinter<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> PrettyPrinter<CfgProgram> for CfgPrinter<'a> {
    fn print(&self, program: &CfgProgram, writer: &mut dyn Write) -> std::io::Result<()> {
        // Create a new printer with program reference for proper variable name lookup
        let printer = CfgPrinter::with_program(program);
        printer.print_program(program, writer)
    }
}

impl<'a> PrettyPrinter<FunctionCfg> for CfgPrinter<'a> {
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
                let param_name = self.get_variable_name(param_id);
                if let Some(param_type) = self.get_variable_type(param_id) {
                    write!(writer, "{}: {}", param_name, param_type)?;
                } else {
                    write!(writer, "{}", param_name)?;
                }
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
        // TODO: This needs to be updated to work with cfg_api
        // The standalone function printer doesn't have access to the full program
        // for (hop_id, hop) in function.hops.iter() {
        //     self.print_hop(program, hop, hop_id, function, writer, 1)?;
        //     writeln!(writer)?;
        // }
        writeln!(
            writer,
            "    // Hops printing disabled - needs program context"
        )?;

        self.write_indent(writer, 0)?;
        writeln!(writer, "}}")?;
        Ok(())
    }
}
