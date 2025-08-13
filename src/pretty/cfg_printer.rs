use super::PrettyPrinter;
use crate::{
    cfg::{
        BasicBlockId, CfgProgram, CfgVisitor, Constant, FunctionCfg, FunctionId, HopId, LValue,
        Operand, RValue, Statement, StmtVisitor, VarId, Variable,
    },
    dataflow::{
        analyze_available_expressions, analyze_constants, analyze_copies, analyze_live_variables,
        analyze_reaching_definitions, analyze_table_mod_ref, AvailExpr, DataflowResults,
        Definition, Flat, LiveVar, MapLattice, SetLattice, StmtLoc, TableAccess,
    },
};
use std::cell::RefCell;
use std::io::Write;

// ============== DATAFLOW OUTPUT CONTROL CONSTANTS ==============
const SHOW_LIVENESS: bool = true;
const SHOW_REACHING_DEFINITIONS: bool = true;
const SHOW_AVAILABLE_EXPRESSIONS: bool = true;
const SHOW_TABLE_MOD_REF: bool = true;
const SHOW_CONSTANT_ANALYSIS: bool = false; // Usually too verbose
const SHOW_COPY_ANALYSIS: bool = false; // Usually too verbose
                                        // ===============================================================

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
    // Cache dataflow results per function to avoid recomputation
    current_function_id: RefCell<Option<FunctionId>>,
    liveness_cache: RefCell<Option<DataflowResults<SetLattice<LiveVar>>>>,
    reaching_def_cache: RefCell<Option<DataflowResults<SetLattice<Definition>>>>,
    available_expr_cache: RefCell<Option<DataflowResults<SetLattice<AvailExpr>>>>,
    table_mod_ref_cache: RefCell<Option<DataflowResults<SetLattice<TableAccess>>>>,
    constant_cache: RefCell<Option<DataflowResults<MapLattice<VarId, Constant>>>>,
    copy_cache: RefCell<Option<DataflowResults<MapLattice<VarId, VarId>>>>,
}

impl<'a> CfgPrintVisitor<'a> {
    fn new(writer: &'a mut dyn Write, program: &'a CfgProgram) -> Self {
        Self {
            writer,
            program,
            indent_level: RefCell::new(0),
            indent_size: 2,
            current_function_id: RefCell::new(None),
            liveness_cache: RefCell::new(None),
            reaching_def_cache: RefCell::new(None),
            available_expr_cache: RefCell::new(None),
            table_mod_ref_cache: RefCell::new(None),
            constant_cache: RefCell::new(None),
            copy_cache: RefCell::new(None),
        }
    }

    /// Ensure dataflow results are computed and cached for the current function
    fn ensure_dataflow_cache(&self, function_id: FunctionId) {
        let mut current_id = self.current_function_id.borrow_mut();
        if *current_id != Some(function_id) {
            // Function changed, recompute all analyses
            *current_id = Some(function_id);

            if let Some(function) = self.program.functions.get(function_id) {
                if SHOW_LIVENESS {
                    *self.liveness_cache.borrow_mut() =
                        Some(analyze_live_variables(function, self.program));
                }

                if SHOW_REACHING_DEFINITIONS {
                    *self.reaching_def_cache.borrow_mut() =
                        Some(analyze_reaching_definitions(function, self.program));
                }

                if SHOW_AVAILABLE_EXPRESSIONS {
                    *self.available_expr_cache.borrow_mut() =
                        Some(analyze_available_expressions(function, self.program));
                }

                if SHOW_TABLE_MOD_REF {
                    *self.table_mod_ref_cache.borrow_mut() =
                        Some(analyze_table_mod_ref(function, self.program));
                }

                if SHOW_CONSTANT_ANALYSIS {
                    *self.constant_cache.borrow_mut() =
                        Some(analyze_constants(function, self.program));
                }

                if SHOW_COPY_ANALYSIS {
                    *self.copy_cache.borrow_mut() = Some(analyze_copies(function, self.program));
                }
            }
        }
    }

    /// Format dataflow information for a statement using visitor pattern
    fn format_dataflow_info(&self, function_id: FunctionId, stmt_loc: StmtLoc) -> String {
        self.ensure_dataflow_cache(function_id);
        let mut parts = Vec::new();

        if SHOW_LIVENESS {
            if let Some(ref results) = *self.liveness_cache.borrow() {
                if let Some(live_vars) = results.stmt_entry.get(&stmt_loc) {
                    parts.push(format!("live {}", self.format_liveness_lattice(live_vars)));
                }
            }
        }

        if SHOW_REACHING_DEFINITIONS {
            if let Some(ref results) = *self.reaching_def_cache.borrow() {
                if let Some(reaching_defs) = results.stmt_entry.get(&stmt_loc) {
                    parts.push(format!(
                        "reaching {}",
                        self.format_definitions_lattice(reaching_defs)
                    ));
                }
            }
        }

        if SHOW_AVAILABLE_EXPRESSIONS {
            if let Some(ref results) = *self.available_expr_cache.borrow() {
                if let Some(avail_exprs) = results.stmt_entry.get(&stmt_loc) {
                    parts.push(format!(
                        "available {}",
                        self.format_avail_expr_lattice(avail_exprs)
                    ));
                }
            }
        }

        if SHOW_TABLE_MOD_REF {
            if let Some(ref results) = *self.table_mod_ref_cache.borrow() {
                if let Some(table_accesses) = results.stmt_entry.get(&stmt_loc) {
                    parts.push(format!(
                        "tables {}",
                        self.format_table_accesses_lattice(table_accesses)
                    ));
                }
            }
        }

        if SHOW_CONSTANT_ANALYSIS {
            if let Some(ref results) = *self.constant_cache.borrow() {
                if let Some(constants) = results.stmt_entry.get(&stmt_loc) {
                    parts.push(format!(
                        "constants {}",
                        self.format_constants_lattice(constants)
                    ));
                }
            }
        }

        if SHOW_COPY_ANALYSIS {
            if let Some(ref results) = *self.copy_cache.borrow() {
                if let Some(copies) = results.stmt_entry.get(&stmt_loc) {
                    parts.push(format!("copies {}", self.format_copies_lattice(copies)));
                }
            }
        }

        if parts.is_empty() {
            String::new()
        } else {
            parts.join(", ")
        }
    }

    /// Format liveness lattice using visitor pattern
    fn format_liveness_lattice(&self, lattice: &SetLattice<LiveVar>) -> String {
        if let Some(live_set) = lattice.as_set() {
            self.format_liveness_set(live_set)
        } else {
            "⊤".to_string() // Top
        }
    }

    /// Format a set of live variables using visitor pattern
    fn format_liveness_set(&self, live_set: &std::collections::HashSet<LiveVar>) -> String {
        if live_set.is_empty() {
            "∅".to_string()
        } else {
            let mut vars: Vec<String> = live_set
                .iter()
                .map(|live_var| self.format_variable(live_var.0))
                .collect();
            vars.sort();
            format!("{{{}}}", vars.join(", "))
        }
    }

    /// Format definitions lattice using visitor pattern
    fn format_definitions_lattice(&self, lattice: &SetLattice<Definition>) -> String {
        if let Some(def_set) = lattice.as_set() {
            self.format_definitions_set(def_set)
        } else {
            "⊤".to_string() // Top
        }
    }

    /// Format a set of definitions using visitor pattern
    fn format_definitions_set(&self, def_set: &std::collections::HashSet<Definition>) -> String {
        if def_set.is_empty() {
            "∅".to_string()
        } else {
            let mut defs: Vec<String> = def_set
                .iter()
                .map(|def| self.format_definition(def))
                .collect();
            defs.sort();
            format!("{{{}}}", defs.join(", "))
        }
    }

    /// Format a single definition using visitor pattern
    fn format_definition(&self, def: &Definition) -> String {
        let var_name = self.format_variable(def.var);
        format!("{}@{}", var_name, def.loc.block.index())
    }

    /// Format available expressions lattice using visitor pattern  
    fn format_avail_expr_lattice(&self, lattice: &SetLattice<AvailExpr>) -> String {
        if let Some(expr_set) = lattice.as_set() {
            self.format_avail_expr_set(expr_set)
        } else {
            "⊤".to_string()
        }
    }

    /// Format available expressions set using visitor pattern
    fn format_avail_expr_set(&self, expr_set: &std::collections::HashSet<AvailExpr>) -> String {
        if expr_set.is_empty() {
            "∅".to_string()
        } else {
            let mut exprs: Vec<String> = expr_set
                .iter()
                .map(|expr| self.format_avail_expr(expr))
                .collect();
            exprs.sort();
            format!("{{{}}}", exprs.join(", "))
        }
    }

    /// Format a single available expression using visitor pattern
    fn format_avail_expr(&self, expr: &AvailExpr) -> String {
        // Since fields are private, use debug representation as fallback
        format!("{:?}", expr)
    }

    /// Format constants lattice using visitor pattern
    fn format_constants_lattice(&self, lattice: &MapLattice<VarId, Constant>) -> String {
        if lattice.is_top() {
            "⊤".to_string()
        } else {
            self.format_constants_map(lattice)
        }
    }

    /// Format constants map using visitor pattern
    fn format_constants_map(&self, const_map: &MapLattice<VarId, Constant>) -> String {
        let mut entries = Vec::new();

        // Since MapLattice doesn't expose its internal map directly,
        // we need to iterate through all variables in the program to check their values
        for (var_id, _var) in self.program.variables.iter() {
            match const_map.get(&var_id) {
                Flat::Value(constant) => {
                    let var_name = self.format_variable(var_id);
                    let const_str = self.format_constant(&constant);
                    entries.push(format!("{}={}", var_name, const_str));
                }
                _ => {} // Skip Bottom and Top values
            }
        }

        if entries.is_empty() {
            "∅".to_string()
        } else {
            entries.sort();
            format!("{{{}}}", entries.join(", "))
        }
    }

    /// Format copies lattice using visitor pattern
    fn format_copies_lattice(&self, lattice: &MapLattice<VarId, VarId>) -> String {
        if lattice.is_top() {
            "⊤".to_string()
        } else {
            self.format_copies_map(lattice)
        }
    }

    /// Format copies map using visitor pattern
    fn format_copies_map(&self, copy_map: &MapLattice<VarId, VarId>) -> String {
        let mut entries = Vec::new();

        // Iterate through all variables to check their copy relations
        for (var_id, _var) in self.program.variables.iter() {
            match copy_map.get(&var_id) {
                Flat::Value(source_var_id) => {
                    let var_name = self.format_variable(var_id);
                    let src_name = self.format_variable(source_var_id);
                    entries.push(format!("{}←{}", var_name, src_name));
                }
                _ => {} // Skip Bottom and Top values
            }
        }

        if entries.is_empty() {
            "∅".to_string()
        } else {
            entries.sort();
            format!("{{{}}}", entries.join(", "))
        }
    }

    /// Format a set of variables using visitor pattern
    fn format_variable_set_lattice(&self, lattice: &SetLattice<VarId>) -> String {
        if let Some(var_set) = lattice.as_set() {
            self.format_variable_set(var_set)
        } else {
            "⊤".to_string() // Top
        }
    }

    /// Format a variable set using visitor pattern  
    fn format_variable_set(&self, var_set: &std::collections::HashSet<VarId>) -> String {
        if var_set.is_empty() {
            "∅".to_string()
        } else {
            let mut vars: Vec<String> = var_set
                .iter()
                .map(|var_id| self.format_variable(*var_id))
                .collect();
            vars.sort();
            format!("{{{}}}", vars.join(", "))
        }
    }

    /// Format a single variable using visitor pattern
    fn format_variable(&self, var_id: VarId) -> String {
        if let Some(var) = self.program.variables.get(var_id) {
            var.name.clone()
        } else {
            format!("v{}", var_id.index())
        }
    }

    /// Format available expressions using visitor pattern
    fn format_available_expressions_lattice(&self, lattice: &SetLattice<RValue>) -> String {
        if let Some(expr_set) = lattice.as_set() {
            self.format_available_expressions(expr_set)
        } else {
            "⊤".to_string()
        }
    }

    /// Format available expressions set using visitor pattern
    fn format_available_expressions(&self, expr_set: &std::collections::HashSet<RValue>) -> String {
        if expr_set.is_empty() {
            "∅".to_string()
        } else {
            let mut exprs: Vec<String> = expr_set
                .iter()
                .map(|rvalue| {
                    // Create a temporary mutable visitor to format rvalue
                    let mut temp_visitor = CfgPrintVisitor {
                        writer: &mut std::io::sink(), // Dummy writer since we only need string output
                        program: self.program,
                        indent_level: RefCell::new(0),
                        indent_size: 2,
                        current_function_id: RefCell::new(None),
                        liveness_cache: RefCell::new(None),
                        reaching_def_cache: RefCell::new(None),
                        available_expr_cache: RefCell::new(None),
                        table_mod_ref_cache: RefCell::new(None),
                        constant_cache: RefCell::new(None),
                        copy_cache: RefCell::new(None),
                    };
                    temp_visitor.visit_rvalue(rvalue)
                })
                .collect();
            exprs.sort();
            format!("{{{}}}", exprs.join(", "))
        }
    }

    /// Format table accesses using visitor pattern
    fn format_table_accesses_lattice(&self, lattice: &SetLattice<TableAccess>) -> String {
        if let Some(access_set) = lattice.as_set() {
            self.format_table_accesses(access_set)
        } else {
            "⊤".to_string()
        }
    }

    /// Format table accesses set using visitor pattern
    fn format_table_accesses(&self, access_set: &std::collections::HashSet<TableAccess>) -> String {
        if access_set.is_empty() {
            "∅".to_string()
        } else {
            let mut accesses: Vec<String> = access_set
                .iter()
                .map(|access| self.format_table_access(access))
                .collect();
            accesses.sort();
            format!("{{{}}}", accesses.join(", "))
        }
    }

    /// Format a single table access using visitor pattern
    fn format_table_access(&self, access: &TableAccess) -> String {
        let table_name = if let Some(table) = self.program.tables.get(access.table) {
            table.name.clone()
        } else {
            format!("table_{}", access.table.index())
        };
        let access_type = match access.access_type {
            crate::dataflow::AccessType::Read => "R",
            crate::dataflow::AccessType::Write => "W",
        };
        format!("{}({})", table_name, access_type)
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
            // Print the basic block with inline dataflow information
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
            "block_{} (hop: hop_{}) [stmts: {}] {{",
            id.index(),
            block.hop_id.index(),
            block.statements.len()
        )?;

        self.increase_indent();

        // Get the function ID from the hop
        let function_id = program.hops[block.hop_id].function_id;

        // Print all statements in this block with inline dataflow information
        for (stmt_index, stmt) in block.statements.iter().enumerate() {
            let stmt_loc = StmtLoc {
                block: id,
                index: stmt_index,
            };
            let stmt_str = self.visit_statement(stmt);
            let dataflow_info = self.format_dataflow_info(function_id, stmt_loc);

            self.write_indent()?;
            if dataflow_info.is_empty() {
                // No dataflow info, just print the statement
                writeln!(self.writer, "[{}] {}", stmt_index, stmt_str)?;
            } else {
                // Format: [0] a = b                    live {a}, reaching {b} ...
                let stmt_part = format!("[{}] {}", stmt_index, stmt_str);
                let available_width = 80; // Target line width
                let stmt_width = stmt_part.len();

                if stmt_width + dataflow_info.len() + 4 <= available_width {
                    // Fits on one line with padding
                    let padding = available_width - stmt_width - dataflow_info.len();
                    writeln!(
                        self.writer,
                        "{}{}{}",
                        stmt_part,
                        " ".repeat(padding),
                        dataflow_info
                    )?;
                } else {
                    // Need to wrap to next line
                    writeln!(self.writer, "{}", stmt_part)?;
                    // Split dataflow info if it's too long
                    if dataflow_info.len() > 60 {
                        // Split on commas and wrap long analyses
                        let parts: Vec<&str> = dataflow_info.split(", ").collect();
                        let mut current_line = String::new();
                        for (i, part) in parts.iter().enumerate() {
                            if i > 0 && current_line.len() + part.len() + 2 > 60 {
                                self.write_indent()?;
                                writeln!(
                                    self.writer,
                                    "                                  {}",
                                    current_line
                                )?;
                                current_line.clear();
                            }
                            if !current_line.is_empty() {
                                current_line.push_str(", ");
                            }
                            current_line.push_str(part);
                        }
                        if !current_line.is_empty() {
                            self.write_indent()?;
                            writeln!(
                                self.writer,
                                "                                  {}",
                                current_line
                            )?;
                        }
                    } else {
                        self.write_indent()?;
                        writeln!(
                            self.writer,
                            "                                  {}",
                            dataflow_info
                        )?;
                    }
                }
            }
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

    fn visit_rvalue(&mut self, rv: &RValue) -> String {
        match rv {
            RValue::Use(operand) => self.visit_operand(operand),
            RValue::TableAccess {
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
            RValue::ArrayAccess { array, index } => {
                let array_str = self.visit_operand(array);
                let index_str = self.visit_operand(index);
                format!("{}[{}]", array_str, index_str)
            }
            RValue::UnaryOp { op, operand } => {
                let operand_str = self.visit_operand(operand);
                let op_str = self.format_unary_op(op);
                format!("{}{}", op_str, operand_str)
            }
            RValue::BinaryOp { op, left, right } => {
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
