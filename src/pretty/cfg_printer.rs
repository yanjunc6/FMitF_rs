use super::PrettyPrinter;
use crate::{
    cfg::{
        BasicBlockId, CfgProgram, CfgVisitor, FunctionCfg, FunctionId, HopId, LValue, Operand,
        Rvalue, Statement, StmtVisitor, VarId, Variable,
    },
    dataflow::{
        analyze_available_expressions, analyze_live_variables, analyze_reaching_definitions,
        analyze_table_mod_ref, AnalysisLevel, SetLattice, TableAccess,
    },
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

/// CFG printer that uses the visitor pattern to traverse and print the CFG
pub struct CfgPrintVisitor<'a> {
    writer: &'a mut dyn std::io::Write,
    program: &'a CfgProgram,
    indent_level: RefCell<usize>,
    indent_size: usize,
}

impl<'a> CfgPrintVisitor<'a> {
    fn new(writer: &'a mut dyn Write, program: &'a CfgProgram) -> Self {
        Self {
            writer,
            program,
            indent_level: RefCell::new(0),
            indent_size: 2,
        }
    }

    /// Format a set of variables as a human-readable string
    fn format_variable_set(&self, var_set: &std::collections::HashSet<VarId>) -> String {
        if var_set.is_empty() {
            "∅".to_string()
        } else {
            let mut vars: Vec<String> = var_set
                .iter()
                .map(|var_id| {
                    if let Some(var) = self.program.variables.get(*var_id) {
                        var.name.clone()
                    } else {
                        format!("v{}", var_id.index())
                    }
                })
                .collect();
            vars.sort();
            format!("{{{}}}", vars.join(", "))
        }
    }

    /// Format available expressions set as a human-readable string
    fn format_available_expressions(
        &mut self,
        expr_set: &std::collections::HashSet<Rvalue>,
    ) -> String {
        if expr_set.is_empty() {
            "∅".to_string()
        } else {
            let mut exprs: Vec<String> = expr_set
                .iter()
                .map(|rvalue| {
                    // Use visitor pattern to format rvalue properly
                    self.visit_rvalue(rvalue)
                })
                .collect();
            exprs.sort();
            format!("{{{}}}", exprs.join(", "))
        }
    }

    /// Format table access set as a human-readable string
    fn format_table_accesses(&self, access_set: &std::collections::HashSet<TableAccess>) -> String {
        if access_set.is_empty() {
            "∅".to_string()
        } else {
            let mut accesses: Vec<String> = access_set
                .iter()
                .map(|access| {
                    let table_name = if let Some(table) = self.program.tables.get(access.table_id) {
                        table.name.clone()
                    } else {
                        format!("table_{}", access.table_id.index())
                    };
                    let access_type = match access.access_type {
                        crate::dataflow::AccessType::Read => "R",
                        crate::dataflow::AccessType::Write => "W",
                    };
                    format!("{}({})", table_name, access_type)
                })
                .collect();
            accesses.sort();
            format!("{{{}}}", accesses.join(", "))
        }
    }

    /// Format a SetLattice for variable sets
    fn format_variable_set_lattice(&self, lattice: &SetLattice<VarId>) -> String {
        if lattice.is_top() {
            "⊤".to_string()
        } else if let Some(set) = lattice.as_set() {
            self.format_variable_set(set)
        } else {
            "⊥".to_string()
        }
    }

    /// Format a SetLattice for available expressions
    fn format_available_expressions_lattice(&mut self, lattice: &SetLattice<Rvalue>) -> String {
        if lattice.is_top() {
            "⊤".to_string()
        } else if let Some(set) = lattice.as_set() {
            self.format_available_expressions(set)
        } else {
            "⊥".to_string()
        }
    }

    /// Format a SetLattice for table accesses
    fn format_table_accesses_lattice(&self, lattice: &SetLattice<TableAccess>) -> String {
        if lattice.is_top() {
            "⊤".to_string()
        } else if let Some(set) = lattice.as_set() {
            self.format_table_accesses(set)
        } else {
            "⊥".to_string()
        }
    }

    /// Print dataflow information for a basic block
    fn print_dataflow_info(
        &mut self,
        program: &CfgProgram,
        function_id: FunctionId,
        block_id: BasicBlockId,
    ) -> std::io::Result<()> {
        // Get the function from the program
        let function = match program.functions.get(function_id) {
            Some(func) => func,
            None => return Ok(()), // Skip if function not found
        };

        // ============== DATAFLOW ANALYSES SECTION ==============
        // Comment out any of the following lines to disable specific analyses:
        let liveness_results =
            analyze_live_variables(program, function_id, AnalysisLevel::Function);
        let reaching_def_results =
            analyze_reaching_definitions(program, function_id, AnalysisLevel::Function);
        let available_expr_results =
            analyze_available_expressions(program, function_id, AnalysisLevel::Function);
        let table_mod_ref_results =
            analyze_table_mod_ref(function, program, AnalysisLevel::Function);
        // ======================================================

        // Print entry dataflow information
        self.write_indent()?;
        writeln!(self.writer, "╔══ Block Entry Dataflow ══╗")?;

        if let Some(entry_liveness) = liveness_results.block_entry.get(&block_id) {
            self.write_indent()?;
            let live_vars = self.format_variable_set_lattice(entry_liveness);
            writeln!(self.writer, "║ Live Variables: {}", live_vars)?;
        }

        if let Some(entry_reaching_defs) = reaching_def_results.block_entry.get(&block_id) {
            self.write_indent()?;
            let reaching_defs = self.format_variable_set_lattice(entry_reaching_defs);
            writeln!(self.writer, "║ Reaching Definitions: {}", reaching_defs)?;
        }

        if let Some(entry_available_exprs) = available_expr_results.block_entry.get(&block_id) {
            self.write_indent()?;
            let available_exprs = self.format_available_expressions_lattice(entry_available_exprs);
            writeln!(self.writer, "║ Available Expressions: {}", available_exprs)?;
        }

        if let Some(entry_table_accesses) = table_mod_ref_results.block_entry.get(&block_id) {
            self.write_indent()?;
            let table_accesses = self.format_table_accesses_lattice(entry_table_accesses);
            writeln!(self.writer, "║ Table Accesses: {}", table_accesses)?;
        }

        self.write_indent()?;
        writeln!(self.writer, "╚══════════════════════════════╝")?;

        Ok(())
    }

    /// Print exit dataflow information for a basic block
    fn print_exit_dataflow_info(
        &mut self,
        program: &CfgProgram,
        function_id: FunctionId,
        block_id: BasicBlockId,
    ) -> std::io::Result<()> {
        // Get the function from the program
        let function = match program.functions.get(function_id) {
            Some(func) => func,
            None => return Ok(()), // Skip if function not found
        };

        // ============== DATAFLOW ANALYSES SECTION ==============
        // Comment out any of the following lines to disable specific analyses:
        let liveness_results =
            analyze_live_variables(program, function_id, AnalysisLevel::Function);
        let reaching_def_results =
            analyze_reaching_definitions(program, function_id, AnalysisLevel::Function);
        let available_expr_results =
            analyze_available_expressions(program, function_id, AnalysisLevel::Function);
        let table_mod_ref_results =
            analyze_table_mod_ref(function, program, AnalysisLevel::Function);
        // ======================================================

        // Print exit dataflow information
        self.write_indent()?;
        writeln!(self.writer, "╔══ Block Exit Dataflow ═══╗")?;

        if let Some(exit_liveness) = liveness_results.block_exit.get(&block_id) {
            self.write_indent()?;
            let live_vars = self.format_variable_set_lattice(exit_liveness);
            writeln!(self.writer, "║ Live Variables: {}", live_vars)?;
        }

        if let Some(exit_reaching_defs) = reaching_def_results.block_exit.get(&block_id) {
            self.write_indent()?;
            let reaching_defs = self.format_variable_set_lattice(exit_reaching_defs);
            writeln!(self.writer, "║ Reaching Definitions: {}", reaching_defs)?;
        }

        if let Some(exit_available_exprs) = available_expr_results.block_exit.get(&block_id) {
            self.write_indent()?;
            let available_exprs = self.format_available_expressions_lattice(exit_available_exprs);
            writeln!(self.writer, "║ Available Expressions: {}", available_exprs)?;
        }

        if let Some(exit_table_accesses) = table_mod_ref_results.block_exit.get(&block_id) {
            self.write_indent()?;
            let table_accesses = self.format_table_accesses_lattice(exit_table_accesses);
            writeln!(self.writer, "║ Table Accesses: {}", table_accesses)?;
        }

        self.write_indent()?;
        writeln!(self.writer, "╚══════════════════════════════╝")?;

        Ok(())
    }

    /// Print statement-level dataflow information
    fn print_stmt_dataflow_info(
        &mut self,
        program: &CfgProgram,
        function_id: FunctionId,
        block_id: BasicBlockId,
        stmt_index: usize,
    ) -> std::io::Result<()> {
        use super::super::dataflow::StmtLoc;

        // Get the function from the program
        let _function = match program.functions.get(function_id) {
            Some(func) => func,
            None => return Ok(()), // Skip if function not found
        };

        // ============== DATAFLOW ANALYSES SECTION ==============
        // Comment out any of the following lines to disable specific analyses:
        let liveness_results =
            analyze_live_variables(program, function_id, AnalysisLevel::Function);
        let reaching_def_results =
            analyze_reaching_definitions(program, function_id, AnalysisLevel::Function);
        let available_expr_results =
            analyze_available_expressions(program, function_id, AnalysisLevel::Function);
        // ======================================================

        let stmt_loc = StmtLoc {
            block: block_id,
            index: stmt_index,
        };

        // Print statement entry info
        self.increase_indent();
        self.write_indent()?;
        writeln!(self.writer, "│ Entry:")?;
        self.increase_indent();

        if let Some(entry_liveness) = liveness_results.stmt_entry.get(&stmt_loc) {
            self.write_indent()?;
            let live_vars = self.format_variable_set_lattice(entry_liveness);
            writeln!(self.writer, "│ Live: {}", live_vars)?;
        }

        if let Some(entry_reaching_defs) = reaching_def_results.stmt_entry.get(&stmt_loc) {
            self.write_indent()?;
            let reaching_defs = self.format_variable_set_lattice(entry_reaching_defs);
            writeln!(self.writer, "│ Reaching: {}", reaching_defs)?;
        }

        if let Some(entry_available_exprs) = available_expr_results.stmt_entry.get(&stmt_loc) {
            self.write_indent()?;
            let available_exprs = self.format_available_expressions_lattice(entry_available_exprs);
            writeln!(self.writer, "│ Available: {}", available_exprs)?;
        }

        self.decrease_indent();

        // Print statement exit info
        self.write_indent()?;
        writeln!(self.writer, "│ Exit:")?;
        self.increase_indent();

        if let Some(exit_liveness) = liveness_results.stmt_exit.get(&stmt_loc) {
            self.write_indent()?;
            let live_vars = self.format_variable_set_lattice(exit_liveness);
            writeln!(self.writer, "│ Live: {}", live_vars)?;
        }

        if let Some(exit_reaching_defs) = reaching_def_results.stmt_exit.get(&stmt_loc) {
            self.write_indent()?;
            let reaching_defs = self.format_variable_set_lattice(exit_reaching_defs);
            writeln!(self.writer, "│ Reaching: {}", reaching_defs)?;
        }

        if let Some(exit_available_exprs) = available_expr_results.stmt_exit.get(&stmt_loc) {
            self.write_indent()?;
            let available_exprs = self.format_available_expressions_lattice(exit_available_exprs);
            writeln!(self.writer, "│ Available: {}", available_exprs)?;
        }

        self.decrease_indent();
        self.decrease_indent();

        Ok(())
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

    /// Format edge type in a human-readable way
    fn format_edge_type(&self, edge_type: &crate::cfg::EdgeType) -> String {
        use crate::cfg::EdgeType;
        match edge_type {
            EdgeType::Unconditional => "unconditional".to_string(),
            EdgeType::ConditionalTrue { condition } => {
                format!("if {}", self.format_operand(condition))
            }
            EdgeType::ConditionalFalse { condition } => {
                format!("if !{}", self.format_operand(condition))
            }
            EdgeType::Return { value } => match value {
                Some(op) => format!("return {}", self.format_operand(op)),
                None => "return".to_string(),
            },
            EdgeType::Abort => "abort".to_string(),
            EdgeType::HopExit { next_hop } => match next_hop {
                Some(hop_id) => format!("hop_exit -> hop_{}", hop_id.index()),
                None => "hop_exit".to_string(),
            },
        }
    }

    /// Format operand for edge descriptions (immutable version)
    fn format_operand(&self, op: &Operand) -> String {
        match op {
            Operand::Var(var_id) => {
                if let Some(var) = self.program.variables.get(*var_id) {
                    self.format_variable_name(var)
                } else {
                    format!("var_{}", var_id.index())
                }
            }
            Operand::Const(constant) => self.format_constant(constant),
        }
    }

    /// Format variable name based on its kind and name
    fn format_variable_name(&self, var: &Variable) -> String {
        use crate::cfg::VariableKind;
        match var.kind {
            VariableKind::Temporary => {
                if var.name.starts_with("_t") {
                    format!("tmp_{}", var.name.strip_prefix("_t").unwrap_or("?"))
                } else {
                    var.name.clone()
                }
            }
            VariableKind::Global => format!("global_{}", var.name),
            VariableKind::Parameter => format!("param_{}", var.name),
            VariableKind::Local => var.name.clone(),
        }
    }

    /// Format return type in a readable way
    fn format_return_type(&self, ret_type: &crate::cfg::ReturnType) -> String {
        use crate::cfg::ReturnType;
        match ret_type {
            ReturnType::Void => "void".to_string(),
            ReturnType::Type(ty) => self.format_type(ty),
        }
    }

    /// Format variable kind in a readable way  
    fn format_variable_kind(&self, kind: &crate::cfg::VariableKind) -> String {
        use crate::cfg::VariableKind;
        match kind {
            VariableKind::Global => "global".to_string(),
            VariableKind::Parameter => "parameter".to_string(),
            VariableKind::Local => "local".to_string(),
            VariableKind::Temporary => "temporary".to_string(),
        }
    }

    /// Format binary operators in a readable way
    fn format_binary_op(&self, op: &crate::ast::BinaryOp) -> String {
        use crate::ast::BinaryOp;
        match op {
            BinaryOp::Add => "+".to_string(),
            BinaryOp::Sub => "-".to_string(),
            BinaryOp::Mul => "*".to_string(),
            BinaryOp::Div => "/".to_string(),
            BinaryOp::Lt => "<".to_string(),
            BinaryOp::Lte => "<=".to_string(),
            BinaryOp::Gt => ">".to_string(),
            BinaryOp::Gte => ">=".to_string(),
            BinaryOp::Eq => "==".to_string(),
            BinaryOp::Neq => "!=".to_string(),
            BinaryOp::And => "&&".to_string(),
            BinaryOp::Or => "||".to_string(),
        }
    }

    /// Format unary operators in a readable way
    fn format_unary_op(&self, op: &crate::ast::UnaryOp) -> String {
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

    /// Format constant for edge descriptions (immutable version)
    fn format_constant(&self, c: &crate::cfg::Constant) -> String {
        match c {
            crate::cfg::Constant::Int(i) => i.to_string(),
            crate::cfg::Constant::Float(f) => f.to_string(),
            crate::cfg::Constant::Bool(b) => b.to_string(),
            crate::cfg::Constant::String(s) => format!("\"{}\"", s),
            crate::cfg::Constant::Array(arr) => {
                let elements: Vec<String> = arr.iter().map(|c| self.format_constant(c)).collect();
                format!("[{}]", elements.join(", "))
            }
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
            "function {} (id: {}) -> {} [type: {:?}, hops: {}, blocks: {}] {{",
            function.name,
            id.index(),
            self.format_return_type(&function.return_type),
            function.function_type,
            function.hops.len(),
            function.blocks.len()
        )?;

        self.increase_indent();

        // Print parameters
        if !function.parameters.is_empty() {
            self.write_indent()?;
            writeln!(self.writer, "parameters:")?;
            self.increase_indent();
            for &param_id in &function.parameters {
                if let Some(param) = program.variables.get(param_id) {
                    self.write_indent()?;
                    writeln!(
                        self.writer,
                        "{}: {}",
                        param.name,
                        self.format_type(&param.ty)
                    )?;
                }
            }
            self.decrease_indent();
            writeln!(self.writer)?;
        }

        // Print local variables
        if !function.local_variables.is_empty() {
            self.write_indent()?;
            writeln!(self.writer, "local_variables:")?;
            self.increase_indent();
            for &var_id in &function.local_variables {
                if let Some(var) = program.variables.get(var_id) {
                    self.write_indent()?;
                    writeln!(
                        self.writer,
                        "{}: {} ({})",
                        self.format_variable_name(var),
                        self.format_type(&var.ty),
                        self.format_variable_kind(&var.kind)
                    )?;
                }
            }
            self.decrease_indent();
            writeln!(self.writer)?;
        }

        // Print hops
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
        writeln!(
            self.writer,
            "hop_{} (func: {}) [blocks: {}] {{",
            id.index(),
            hop.function_id.index(),
            hop.blocks.len()
        )?;

        // Print all blocks in this hop
        self.increase_indent();
        for &block_id in &hop.blocks {
            // Print entry dataflow information
            self.print_dataflow_info(program, hop.function_id, block_id)?;

            // Print the basic block
            self.visit_basic_block(program, block_id)?;

            // Print exit dataflow information
            self.print_exit_dataflow_info(program, hop.function_id, block_id)?;
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
            "block_{} (hop: hop_{}) [stmts: {}] {{",
            id.index(),
            block.hop_id.index(),
            block.statements.len()
        )?;

        self.increase_indent();

        // Get the function ID from the hop
        let function_id = program.hops[block.hop_id].function_id;

        // Print all statements in this block with their dataflow information
        for (stmt_index, stmt) in block.statements.iter().enumerate() {
            self.write_indent()?;
            let stmt_str = self.visit_statement(stmt);
            writeln!(self.writer, "[{}] {}", stmt_index, stmt_str)?;

            // Print statement-level dataflow information
            self.print_stmt_dataflow_info(program, function_id, id, stmt_index)?;
        }

        // Print control flow edges
        if !block.successors.is_empty() {
            self.write_indent()?;
            writeln!(self.writer, "successors:")?;
            self.increase_indent();
            for edge in &block.successors {
                self.write_indent()?;
                let edge_desc = self.format_edge_type(&edge.edge_type);
                writeln!(self.writer, "-> block_{} ({})", edge.to.index(), edge_desc)?;
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
                if let Some(var_info) = self.program.variables.get(*var) {
                    self.format_variable_name(var_info)
                } else {
                    format!("var_{}", var.index())
                }
            }
            LValue::ArrayElement { array, index } => {
                let array_name = if let Some(var_info) = self.program.variables.get(*array) {
                    self.format_variable_name(var_info)
                } else {
                    format!("var_{}", array.index())
                };
                let index_str = self.visit_operand(index);
                format!("{}[{}]", array_name, index_str)
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
                let op_str = self.format_unary_op(op);
                format!("{}{}", op_str, operand_str)
            }
            Rvalue::BinaryOp { op, left, right } => {
                let left_str = self.visit_operand(left);
                let right_str = self.visit_operand(right);
                let op_str = self.format_binary_op(op);
                format!("({} {} {})", left_str, op_str, right_str)
            }
        }
    }

    fn visit_operand(&mut self, op: &Operand) -> String {
        match op {
            Operand::Var(var_id) => {
                if let Some(var_info) = self.program.variables.get(*var_id) {
                    self.format_variable_name(var_info)
                } else {
                    format!("var_{}", var_id.index())
                }
            }
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
        self.format_variable_name(v)
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
