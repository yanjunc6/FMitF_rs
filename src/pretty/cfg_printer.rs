//! CFG Pretty Printer
//!
//! This module provides pretty printing for the Control Flow Graph (CFG) representation.

use crate::cfg::*;
use crate::dataflow::ExprKind;
use crate::dataflow::{
    analyze_available_expressions, analyze_live_variables, analyze_reaching_definitions,
    analyze_table_mod_ref, AccessType, AvailExpr, DataflowResults, Definition, LiveVar, SetLattice,
    StmtLoc, TableAccess,
};
use crate::pretty::PrettyPrint;
use std::io::{self, Write};

// Debug options - simple constants for development
const SHOW_VAR_IDS: bool = true;
const SHOW_TYPE_INFO: bool = false;
const SHOW_LIVENESS: bool = false;
const SHOW_REACHING_DEF: bool = false;
const SHOW_TABLE_MOD_REF: bool = false;
const SHOW_AVAILABLE_EXPRS: bool = false;

// Options for granularity of analysis printing
const SHOW_ANALYSIS_PER_BLOCK: bool = true; // Show analysis at block entry/exit
const SHOW_ANALYSIS_PER_STMT: bool = false; // Show analysis before/after each statement

/// A pretty printer for CFG nodes.
pub struct CfgPrinter<W: Write> {
    writer: W,
    indent_level: usize,
    liveness: Option<DataflowResults<SetLattice<LiveVar>>>,
    reaching_def: Option<DataflowResults<SetLattice<Definition>>>,
    table_mod_ref: Option<DataflowResults<SetLattice<TableAccess>>>,
    available_exprs: Option<DataflowResults<SetLattice<AvailExpr>>>,
}

impl<W: Write> CfgPrinter<W> {
    pub fn new(writer: W) -> Self {
        Self {
            writer,
            indent_level: 0,
            liveness: None,
            reaching_def: None,
            table_mod_ref: None,
            available_exprs: None,
        }
    }

    fn write_indent(&mut self) -> io::Result<()> {
        for _ in 0..self.indent_level {
            write!(self.writer, "  ")?;
        }
        Ok(())
    }

    fn indent(&mut self) {
        self.indent_level += 1;
    }

    fn dedent(&mut self) {
        if self.indent_level > 0 {
            self.indent_level -= 1;
        }
    }

    pub fn print_program(&mut self, program: &Program) -> io::Result<()> {
        writeln!(self.writer, "=== CFG Program ===")?;
        writeln!(self.writer)?;

        // Print global constants
        if program.global_consts.len() > 0 {
            writeln!(self.writer, "=== Global Constants ===")?;
            for (id, global_const) in &program.global_consts {
                self.print_global_const(program, id, global_const)?;
            }
            writeln!(self.writer)?;
        }

        // Print user-defined types
        if program.user_defined_types.len() > 0 {
            writeln!(self.writer, "=== User-Defined Types ===")?;
            for (id, user_type) in &program.user_defined_types {
                self.print_user_defined_type(program, id, user_type)?;
            }
            writeln!(self.writer)?;
        }

        // Print tables
        if program.tables.len() > 0 {
            writeln!(self.writer, "=== Tables ===")?;
            for (id, table) in &program.tables {
                self.print_table(program, id, table)?;
            }
            writeln!(self.writer)?;
        }

        // Print functions
        if program.functions.len() > 0 {
            writeln!(self.writer, "=== Functions ===")?;
            for (id, function) in &program.functions {
                self.print_function(program, id, function)?;
                writeln!(self.writer)?;
            }
        }

        Ok(())
    }

    fn print_global_const(
        &mut self,
        program: &Program,
        id: GlobalConstId,
        global_const: &GlobalConst,
    ) -> io::Result<()> {
        self.write_indent()?;
        write!(self.writer, "const {}", global_const.name)?;
        if SHOW_VAR_IDS {
            write!(self.writer, "[gc{}]", id.index())?;
        }
        write!(self.writer, ": ")?;
        self.print_type_info(program, global_const.ty)?;
        write!(self.writer, " = ")?;
        self.print_constant_value(&global_const.init)?;
        writeln!(self.writer, ";")?;
        Ok(())
    }

    fn print_user_defined_type(
        &mut self,
        program: &Program,
        id: UserDefinedTypeId,
        user_type: &UserDefinedType,
    ) -> io::Result<()> {
        self.write_indent()?;
        write!(self.writer, "type {}", user_type.name)?;
        if SHOW_VAR_IDS {
            write!(self.writer, "[udt{}]", id.index())?;
        }

        // Generic parameters
        if !user_type.generic_params.is_empty() {
            write!(self.writer, "<")?;
            for (i, param_id) in user_type.generic_params.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ", ")?;
                }
                let param = &program.generic_params[*param_id];
                write!(self.writer, "{}", param.name)?;
                if SHOW_VAR_IDS {
                    write!(self.writer, "[gp{}]", param_id.index())?;
                }
            }
            write!(self.writer, ">")?;
        }

        writeln!(self.writer, ";")?;
        Ok(())
    }

    fn print_table(&mut self, program: &Program, id: TableId, table: &Table) -> io::Result<()> {
        self.write_indent()?;
        write!(self.writer, "table {}", table.name)?;
        if SHOW_VAR_IDS {
            write!(self.writer, "[tab{}]", id.index())?;
        }
        writeln!(self.writer, " {{")?;
        self.indent();

        // Primary key fields - show with types and "primary" prefix
        if !table.primary_key_fields.is_empty() {
            for field_id in &table.primary_key_fields {
                let field = &program.table_fields[*field_id];
                self.write_indent()?;
                write!(self.writer, "primary {}", field.name)?;
                if SHOW_VAR_IDS {
                    write!(self.writer, "[f{}]", field_id.index())?;
                }
                write!(self.writer, ": ")?;
                self.print_type_info(program, field.field_type)?;
                writeln!(self.writer, ";")?;
            }
        }

        // Other fields - show with types
        for field_id in &table.other_fields {
            let field = &program.table_fields[*field_id];
            self.write_indent()?;
            write!(self.writer, "{}", field.name)?;
            if SHOW_VAR_IDS {
                write!(self.writer, "[f{}]", field_id.index())?;
            }
            write!(self.writer, ": ")?;
            self.print_type_info(program, field.field_type)?;
            writeln!(self.writer, ";")?;
        }

        // Node partition information
        if let Some(partition_func_id) = table.node_partition {
            let partition_func = &program.functions[partition_func_id];
            self.write_indent()?;
            write!(self.writer, "node_partition: {}", partition_func.name)?;
            if SHOW_VAR_IDS {
                write!(self.writer, "[fn{}]", partition_func_id.index())?;
            }
            if !table.node_partition_args.is_empty() {
                write!(self.writer, "(")?;
                for (i, arg) in table.node_partition_args.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    match arg {
                        PartitionArg::Field(field_id) => {
                            let field = &program.table_fields[*field_id];
                            write!(self.writer, "{}", field.name)?;
                            if SHOW_VAR_IDS {
                                write!(self.writer, "[f{}]", field_id.index())?;
                            }
                        }
                        PartitionArg::Constant(val) => match val {
                            ConstantValue::Int(i) => write!(self.writer, "{}", i)?,
                            ConstantValue::Float(f) => write!(self.writer, "{}", f)?,
                            ConstantValue::Bool(b) => write!(self.writer, "{}", b)?,
                            ConstantValue::String(s) => write!(self.writer, "\"{}\"", s)?,
                        },
                    }
                }
                write!(self.writer, ")")?;
            }
            writeln!(self.writer, ";")?;
        }

        // Invariants
        if !table.invariants.is_empty() {
            self.write_indent()?;
            write!(self.writer, "invariants: ")?;
            for (i, invariant_id) in table.invariants.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ", ")?;
                }
                let invariant_func = &program.functions[*invariant_id];
                write!(self.writer, "{}", invariant_func.name)?;
                if SHOW_VAR_IDS {
                    write!(self.writer, "[fn{}]", invariant_id.index())?;
                }
            }
            writeln!(self.writer, ";")?;
        }

        self.dedent();
        self.write_indent()?;
        writeln!(self.writer, "}}")?;
        Ok(())
    }

    fn print_function(
        &mut self,
        program: &Program,
        id: FunctionId,
        function: &Function,
    ) -> io::Result<()> {
        if SHOW_LIVENESS {
            if !matches!(function.kind, crate::cfg::FunctionKind::Operator) {
                let liveness_results = analyze_live_variables(function, program);
                self.liveness = Some(liveness_results);
            } else {
                self.liveness = None;
            }
        }

        if SHOW_REACHING_DEF {
            if !matches!(function.kind, crate::cfg::FunctionKind::Operator) {
                let reaching_def_results = analyze_reaching_definitions(function, program);
                self.reaching_def = Some(reaching_def_results);
            } else {
                self.reaching_def = None;
            }
        }

        if SHOW_TABLE_MOD_REF {
            if !matches!(function.kind, crate::cfg::FunctionKind::Operator) {
                let table_mod_ref_results = analyze_table_mod_ref(function, program);
                self.table_mod_ref = Some(table_mod_ref_results);
            } else {
                self.table_mod_ref = None;
            }
        }

        if SHOW_AVAILABLE_EXPRS {
            if !matches!(function.kind, crate::cfg::FunctionKind::Operator) {
                let available_exprs_results = analyze_available_expressions(function, program);
                self.available_exprs = Some(available_exprs_results);
            } else {
                self.available_exprs = None;
            }
        }

        // Print decorators
        for decorator in &function.decorators {
            self.write_indent()?;
            writeln!(self.writer, "@{}", decorator.name)?;
        }

        self.write_indent()?;
        match function.kind {
            FunctionKind::Function => write!(self.writer, "function")?,
            FunctionKind::Transaction => write!(self.writer, "transaction")?,
            FunctionKind::Partition => write!(self.writer, "partition")?,
            FunctionKind::Lambda => write!(self.writer, "lambda")?,
            FunctionKind::Operator => write!(self.writer, "operator")?,
            FunctionKind::Invariant => write!(self.writer, "invariant")?,
            FunctionKind::Assumption => write!(self.writer, "assumption")?,
        }
        write!(self.writer, " {}", function.name)?;
        if SHOW_VAR_IDS {
            write!(self.writer, "[fn{}]", id.index())?;
        }

        // Generic parameters
        if !function.signature.quantified_params.is_empty() {
            write!(self.writer, "<")?;
            for (i, param_id) in function.signature.quantified_params.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ", ")?;
                }
                let param = &program.generic_params[*param_id];
                write!(self.writer, "{}", param.name)?;
                if SHOW_VAR_IDS {
                    write!(self.writer, "[gp{}]", param_id.index())?;
                }
            }
            write!(self.writer, ">")?;
        }

        // Parameters
        write!(self.writer, "(")?;
        for (i, param_id) in function.params.iter().enumerate() {
            if i > 0 {
                write!(self.writer, ", ")?;
            }
            let param = &program.variables[*param_id];
            write!(self.writer, "{}", param.name)?;
            if SHOW_VAR_IDS {
                write!(self.writer, "[v{}]", param_id.index())?;
            }
            write!(self.writer, ": ")?;
            self.print_type_info(program, param.ty)?;
        }
        write!(self.writer, ")")?;

        // Function signature return type
        if SHOW_TYPE_INFO {
            write!(self.writer, " : ")?;
            self.print_type_info(program, function.signature.ty)?;
        }

        // Print assumptions if any
        if !function.assumptions.is_empty() {
            writeln!(self.writer)?;
            self.indent();
            self.write_indent()?;
            write!(self.writer, "assumptions: ")?;
            for (i, assumption_id) in function.assumptions.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ", ")?;
                }
                let assumption_func = &program.functions[*assumption_id];
                write!(self.writer, "{}", assumption_func.name)?;
                if SHOW_VAR_IDS {
                    write!(self.writer, "[fn{}]", assumption_id.index())?;
                }
            }
            writeln!(self.writer, ";")?;
            self.dedent();
        } else {
            writeln!(self.writer)?;
        }

        // Print function body
        if function.kind == FunctionKind::Transaction {
            // For transactions, use braces to group all hops
            writeln!(self.writer, " {{")?;
            self.indent();

            if let Some(entry_hop) = function.entry_hop {
                self.write_indent()?;
                writeln!(self.writer, "entry_hop: hop{}", entry_hop.index())?;
                writeln!(self.writer)?;
            }

            for hop_id in &function.hops {
                self.print_hop(program, *hop_id)?;
            }

            self.dedent();
            writeln!(self.writer, "}}")?;
        } else if function.kind == FunctionKind::Lambda {
            // For lambda functions, print with consistent bracing like transactions
            writeln!(self.writer)?;
            writeln!(self.writer, "{{")?;
            self.indent();

            if let Some(entry_block) = function.entry_block {
                self.write_indent()?;
                writeln!(self.writer, "entry_block: bb{}", entry_block.index())?;

                for block_id in &function.all_blocks {
                    self.print_basic_block(program, *block_id)?;
                }
            }

            self.dedent();
            writeln!(self.writer, "}}")?;
        } else {
            // For regular functions, print basic blocks without braces
            writeln!(self.writer)?;

            if let Some(entry_block) = function.entry_block {
                self.indent();
                self.write_indent()?;
                writeln!(self.writer, "entry_block: bb{}", entry_block.index())?;
                self.dedent();
            }

            for block_id in &function.all_blocks {
                self.print_basic_block(program, *block_id)?;
            }
        }

        Ok(())
    }

    fn print_hop(&mut self, program: &Program, id: HopId) -> io::Result<()> {
        let hop = &program.hops[id];

        self.write_indent()?;
        write!(self.writer, "hop{}", id.index())?;

        // Print decorators if any
        if !hop.decorators.is_empty() {
            write!(self.writer, " @")?;
            for (i, decorator) in hop.decorators.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, " @")?;
                }
                write!(self.writer, "{}", decorator.name)?;
            }
        }

        writeln!(self.writer, " {{")?;
        self.indent();

        // Print entry block information
        if let Some(entry_block) = hop.entry_block {
            self.write_indent()?;
            writeln!(self.writer, "entry_block: bb{}", entry_block.index())?;
        }

        // Print all blocks in this hop
        for block_id in &hop.blocks {
            self.print_basic_block(program, *block_id)?;
        }

        self.dedent();
        self.write_indent()?;
        writeln!(self.writer, "}}")?;
        writeln!(self.writer)?;
        Ok(())
    }

    fn print_basic_block(&mut self, program: &Program, id: BasicBlockId) -> io::Result<()> {
        let block = &program.basic_blocks[id];

        self.write_indent()?;
        write!(self.writer, "bb{}", id.index())?;
        if !block.predecessors.is_empty() {
            write!(self.writer, " (preds: ")?;
            for (i, pred) in block.predecessors.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ", ")?;
                }
                write!(self.writer, "bb{}", pred.index())?;
            }
            write!(self.writer, ")")?;
        }
        writeln!(self.writer, ":")?;
        self.indent();

        // Print liveness info
        if SHOW_LIVENESS && SHOW_ANALYSIS_PER_BLOCK {
            let live_in_set = if let Some(liveness) = &self.liveness {
                liveness.block_entry.get(&id).cloned()
            } else {
                None
            };
            if let Some(live_in) = live_in_set {
                self.write_indent()?;
                write!(self.writer, "livein: ")?;
                self.print_liveness_set(program, &live_in)?;
                writeln!(self.writer)?;
            }
        }

        // Print reaching definitions info
        if SHOW_REACHING_DEF && SHOW_ANALYSIS_PER_BLOCK {
            let reaching_def_in_set = if let Some(reaching_def) = &self.reaching_def {
                reaching_def.block_entry.get(&id).cloned()
            } else {
                None
            };
            if let Some(reaching_def_in) = reaching_def_in_set {
                self.write_indent()?;
                write!(self.writer, "reaching_def_in: ")?;
                self.print_reaching_def_set(program, &reaching_def_in)?;
                writeln!(self.writer)?;
            }
        }

        // Print table mod/ref info
        if SHOW_TABLE_MOD_REF && SHOW_ANALYSIS_PER_BLOCK {
            let table_mod_ref_in_set = if let Some(table_mod_ref) = &self.table_mod_ref {
                table_mod_ref.block_entry.get(&id).cloned()
            } else {
                None
            };
            if let Some(table_mod_ref_in) = table_mod_ref_in_set {
                self.write_indent()?;
                write!(self.writer, "table_mod_ref_in: ")?;
                self.print_table_mod_ref_set(program, &table_mod_ref_in)?;
                writeln!(self.writer)?;
            }
        }

        // Print available expressions info
        if SHOW_AVAILABLE_EXPRS && SHOW_ANALYSIS_PER_BLOCK {
            let available_exprs_in_set = if let Some(available_exprs) = &self.available_exprs {
                available_exprs.block_entry.get(&id).cloned()
            } else {
                None
            };
            if let Some(available_exprs_in) = available_exprs_in_set {
                self.write_indent()?;
                write!(self.writer, "available_exprs_in: ")?;
                self.print_available_exprs_set(program, &available_exprs_in)?;
                writeln!(self.writer)?;
            }
        }

        // Print instructions
        for (idx, instruction) in block.instructions.iter().enumerate() {
            // Print per-statement entry analysis
            if SHOW_ANALYSIS_PER_STMT {
                let stmt_loc = StmtLoc {
                    block: id,
                    index: idx,
                };
                self.print_stmt_analysis_entry(program, &stmt_loc)?;
            }

            self.print_instruction(program, instruction)?;

            // Print per-statement exit analysis
            if SHOW_ANALYSIS_PER_STMT {
                let stmt_loc = StmtLoc {
                    block: id,
                    index: idx,
                };
                self.print_stmt_analysis_exit(program, &stmt_loc)?;
            }
        }

        // Print terminator
        self.print_terminator(program, &block.terminator)?;

        // Print liveness info
        if SHOW_LIVENESS && SHOW_ANALYSIS_PER_BLOCK {
            let live_out_set = if let Some(liveness) = &self.liveness {
                liveness.block_exit.get(&id).cloned()
            } else {
                None
            };
            if let Some(live_out) = live_out_set {
                self.write_indent()?;
                write!(self.writer, "liveout: ")?;
                self.print_liveness_set(program, &live_out)?;
                writeln!(self.writer)?;
            }
        }

        // Print reaching definitions info
        if SHOW_REACHING_DEF && SHOW_ANALYSIS_PER_BLOCK {
            let reaching_def_out_set = if let Some(reaching_def) = &self.reaching_def {
                reaching_def.block_exit.get(&id).cloned()
            } else {
                None
            };
            if let Some(reaching_def_out) = reaching_def_out_set {
                self.write_indent()?;
                write!(self.writer, "reaching_def_out: ")?;
                self.print_reaching_def_set(program, &reaching_def_out)?;
                writeln!(self.writer)?;
            }
        }

        // Print table mod/ref info
        if SHOW_TABLE_MOD_REF && SHOW_ANALYSIS_PER_BLOCK {
            let table_mod_ref_out_set = if let Some(table_mod_ref) = &self.table_mod_ref {
                table_mod_ref.block_exit.get(&id).cloned()
            } else {
                None
            };
            if let Some(table_mod_ref_out) = table_mod_ref_out_set {
                self.write_indent()?;
                write!(self.writer, "table_mod_ref_out: ")?;
                self.print_table_mod_ref_set(program, &table_mod_ref_out)?;
                writeln!(self.writer)?;
            }
        }

        // Print available expressions info
        if SHOW_AVAILABLE_EXPRS && SHOW_ANALYSIS_PER_BLOCK {
            let available_exprs_out_set = if let Some(available_exprs) = &self.available_exprs {
                available_exprs.block_exit.get(&id).cloned()
            } else {
                None
            };
            if let Some(available_exprs_out) = available_exprs_out_set {
                self.write_indent()?;
                write!(self.writer, "available_exprs_out: ")?;
                self.print_available_exprs_set(program, &available_exprs_out)?;
                writeln!(self.writer)?;
            }
        }

        self.dedent();
        Ok(())
    }

    fn print_instruction(
        &mut self,
        program: &Program,
        instruction: &Instruction,
    ) -> io::Result<()> {
        self.write_indent()?;
        match &instruction.kind {
            InstructionKind::Assign { dest, src } => {
                self.print_variable_info(program, *dest)?;
                write!(self.writer, " = ")?;
                self.print_operand(program, src)?;
            }
            InstructionKind::BinaryOp {
                dest,
                op,
                left,
                right,
            } => {
                self.print_variable_info(program, *dest)?;
                write!(self.writer, " = ")?;
                self.print_operand(program, left)?;
                write!(self.writer, " {} ", self.binary_op_to_string(op))?;
                self.print_operand(program, right)?;
            }
            InstructionKind::UnaryOp { dest, op, operand } => {
                self.print_variable_info(program, *dest)?;
                write!(self.writer, " = {}", self.unary_op_to_string(op))?;
                self.print_operand(program, operand)?;
            }
            InstructionKind::Call { dest, func, args } => {
                if let Some(dest) = dest {
                    self.print_variable_info(program, *dest)?;
                    write!(self.writer, " = ")?;
                }
                let function = &program.functions[*func];
                write!(self.writer, "{}", function.name)?;
                if SHOW_VAR_IDS {
                    write!(self.writer, "[fn{}]", func.index())?;
                }
                write!(self.writer, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.print_operand(program, arg)?;
                }
                write!(self.writer, ")")?;
            }
            InstructionKind::TableGet {
                dest,
                table,
                keys,
                field,
            } => {
                self.print_variable_info(program, *dest)?;
                write!(self.writer, " = ")?;
                let table_info = &program.tables[*table];
                write!(self.writer, "{}[", table_info.name)?;
                for (i, key) in keys.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.print_operand(program, key)?;
                }
                write!(self.writer, "]")?;
                if let Some(field_id) = field {
                    let field_info = &program.table_fields[*field_id];
                    write!(self.writer, ".{}", field_info.name)?;
                }
            }
            InstructionKind::TableSet {
                table,
                keys,
                field,
                value,
            } => {
                let table_info = &program.tables[*table];
                write!(self.writer, "{}[", table_info.name)?;
                for (i, key) in keys.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.print_operand(program, key)?;
                }
                write!(self.writer, "]")?;
                if let Some(field_id) = field {
                    let field_info = &program.table_fields[*field_id];
                    write!(self.writer, ".{}", field_info.name)?;
                }
                write!(self.writer, " = ")?;
                self.print_operand(program, value)?;
            }
            InstructionKind::Assert { condition, message } => {
                write!(self.writer, "assert ")?;
                self.print_operand(program, condition)?;
                if !message.is_empty() {
                    write!(self.writer, ", \"{}\"", message)?;
                }
            }
        }
        writeln!(self.writer, ";")?;
        Ok(())
    }

    fn print_terminator(&mut self, program: &Program, terminator: &Terminator) -> io::Result<()> {
        self.write_indent()?;
        match terminator {
            Terminator::Jump(block) => {
                writeln!(self.writer, "jump bb{};", block.index())?;
            }
            Terminator::Branch {
                condition,
                if_true,
                if_false,
            } => {
                write!(self.writer, "branch ")?;
                self.print_operand(program, condition)?;
                writeln!(
                    self.writer,
                    " ? bb{} : bb{};",
                    if_true.index(),
                    if_false.index()
                )?;
            }
            Terminator::Return(value) => {
                write!(self.writer, "return")?;
                if let Some(val) = value {
                    write!(self.writer, " ")?;
                    self.print_operand(program, val)?;
                }
                writeln!(self.writer, ";")?;
            }
            Terminator::HopExit { next_hop } => {
                write!(self.writer, "hop_exit hop{}", next_hop.index())?;
                writeln!(self.writer, ";")?;
            }
            Terminator::Abort => {
                writeln!(self.writer, "abort;")?;
            }
        }
        Ok(())
    }

    fn print_liveness_set(
        &mut self,
        program: &Program,
        liveness: &SetLattice<LiveVar>,
    ) -> io::Result<()> {
        write!(self.writer, "{{")?;
        if let Some(set) = liveness.as_set() {
            for (i, live_var) in set.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ", ")?;
                }
                let var = &program.variables[live_var.0];
                write!(self.writer, "{}", var.name)?;
                if SHOW_VAR_IDS {
                    write!(self.writer, "[v{}]", live_var.0.index())?;
                }
            }
        } else {
            write!(self.writer, "TOP")?;
        }
        write!(self.writer, "}}")?;
        Ok(())
    }

    fn print_reaching_def_set(
        &mut self,
        program: &Program,
        reaching_def: &SetLattice<Definition>,
    ) -> io::Result<()> {
        write!(self.writer, "{{")?;
        if let Some(set) = reaching_def.as_set() {
            for (i, def) in set.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ", ")?;
                }
                let var = &program.variables[def.var];
                write!(self.writer, "{}", var.name)?;
                if SHOW_VAR_IDS {
                    write!(self.writer, "[v{}]", def.var.index())?;
                }
                write!(
                    self.writer,
                    "@bb{}.{}",
                    def.loc.block.index(),
                    def.loc.index
                )?;
            }
        } else {
            write!(self.writer, "TOP")?;
        }
        write!(self.writer, "}}")?;
        Ok(())
    }

    fn print_table_mod_ref_set(
        &mut self,
        program: &Program,
        table_mod_ref: &SetLattice<TableAccess>,
    ) -> io::Result<()> {
        write!(self.writer, "{{")?;
        if let Some(set) = table_mod_ref.as_set() {
            for (i, access) in set.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ", ")?;
                }
                let table = &program.tables[access.table];
                let field = &program.table_fields[access.field];
                write!(self.writer, "{}.{}", table.name, field.name)?;
                match access.access_type {
                    AccessType::Read => write!(self.writer, "(R)")?,
                    AccessType::Write => write!(self.writer, "(W)")?,
                }
            }
        } else {
            write!(self.writer, "TOP")?;
        }
        write!(self.writer, "}}")?;
        Ok(())
    }

    fn print_available_exprs_set(
        &mut self,
        program: &Program,
        available_exprs: &SetLattice<AvailExpr>,
    ) -> io::Result<()> {
        write!(self.writer, "{{")?;
        if let Some(set) = available_exprs.as_set() {
            for (i, expr) in set.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ", ")?;
                }
                let dest_var = &program.variables[expr.dest];
                write!(self.writer, "{}=", dest_var.name)?;
                if SHOW_VAR_IDS {
                    write!(self.writer, "[v{}]", expr.dest.index())?;
                }
                self.print_expr_kind(program, &expr.op)?;
            }
        } else {
            write!(self.writer, "TOP")?;
        }
        write!(self.writer, "}}")?;
        Ok(())
    }

    fn print_expr_kind(&mut self, program: &Program, expr: &ExprKind) -> io::Result<()> {
        match expr {
            ExprKind::BinaryOp { op, left, right } => {
                write!(self.writer, "(")?;
                self.print_operand(program, left)?;
                write!(self.writer, " {} ", self.binary_op_to_string(op))?;
                self.print_operand(program, right)?;
                write!(self.writer, ")")?;
            }
            ExprKind::UnaryOp { op, operand } => {
                write!(self.writer, "{}", self.unary_op_to_string(op))?;
                self.print_operand(program, operand)?;
            }
            ExprKind::Use(operand) => {
                self.print_operand(program, operand)?;
            }
            ExprKind::Call { func, args } => {
                let function = &program.functions[*func];
                write!(self.writer, "{}(", function.name)?;
                if SHOW_VAR_IDS {
                    write!(self.writer, "[fn{}]", func.index())?;
                }
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.print_operand(program, arg)?;
                }
                write!(self.writer, ")")?;
            }
        }
        Ok(())
    }

    fn print_stmt_analysis_entry(
        &mut self,
        program: &Program,
        stmt_loc: &StmtLoc,
    ) -> io::Result<()> {
        let mut has_any = false;

        // Liveness
        if SHOW_LIVENESS {
            let live_in = if let Some(liveness) = &self.liveness {
                liveness.stmt_entry.get(stmt_loc).cloned()
            } else {
                None
            };
            if let Some(live_in) = live_in {
                self.write_indent()?;
                write!(self.writer, "  [stmt_in] livein: ")?;
                self.print_liveness_set(program, &live_in)?;
                writeln!(self.writer)?;
                has_any = true;
            }
        }

        // Reaching definitions
        if SHOW_REACHING_DEF {
            let rd_in = if let Some(reaching_def) = &self.reaching_def {
                reaching_def.stmt_entry.get(stmt_loc).cloned()
            } else {
                None
            };
            if let Some(rd_in) = rd_in {
                self.write_indent()?;
                write!(self.writer, "  [stmt_in] reaching_def: ")?;
                self.print_reaching_def_set(program, &rd_in)?;
                writeln!(self.writer)?;
                has_any = true;
            }
        }

        // Table mod/ref
        if SHOW_TABLE_MOD_REF {
            let tmr_in = if let Some(table_mod_ref) = &self.table_mod_ref {
                table_mod_ref.stmt_entry.get(stmt_loc).cloned()
            } else {
                None
            };
            if let Some(tmr_in) = tmr_in {
                self.write_indent()?;
                write!(self.writer, "  [stmt_in] table_mod_ref: ")?;
                self.print_table_mod_ref_set(program, &tmr_in)?;
                writeln!(self.writer)?;
                has_any = true;
            }
        }

        // Available expressions
        if SHOW_AVAILABLE_EXPRS {
            let ae_in = if let Some(available_exprs) = &self.available_exprs {
                available_exprs.stmt_entry.get(stmt_loc).cloned()
            } else {
                None
            };
            if let Some(ae_in) = ae_in {
                self.write_indent()?;
                write!(self.writer, "  [stmt_in] available_exprs: ")?;
                self.print_available_exprs_set(program, &ae_in)?;
                writeln!(self.writer)?;
                has_any = true;
            }
        }

        if has_any {
            // Add a blank line for readability
        }
        Ok(())
    }

    fn print_stmt_analysis_exit(
        &mut self,
        program: &Program,
        stmt_loc: &StmtLoc,
    ) -> io::Result<()> {
        let mut has_any = false;

        // Liveness
        if SHOW_LIVENESS {
            let live_out = if let Some(liveness) = &self.liveness {
                liveness.stmt_exit.get(stmt_loc).cloned()
            } else {
                None
            };
            if let Some(live_out) = live_out {
                self.write_indent()?;
                write!(self.writer, "  [stmt_out] liveout: ")?;
                self.print_liveness_set(program, &live_out)?;
                writeln!(self.writer)?;
                has_any = true;
            }
        }

        // Reaching definitions
        if SHOW_REACHING_DEF {
            let rd_out = if let Some(reaching_def) = &self.reaching_def {
                reaching_def.stmt_exit.get(stmt_loc).cloned()
            } else {
                None
            };
            if let Some(rd_out) = rd_out {
                self.write_indent()?;
                write!(self.writer, "  [stmt_out] reaching_def: ")?;
                self.print_reaching_def_set(program, &rd_out)?;
                writeln!(self.writer)?;
                has_any = true;
            }
        }

        // Table mod/ref
        if SHOW_TABLE_MOD_REF {
            let tmr_out = if let Some(table_mod_ref) = &self.table_mod_ref {
                table_mod_ref.stmt_exit.get(stmt_loc).cloned()
            } else {
                None
            };
            if let Some(tmr_out) = tmr_out {
                self.write_indent()?;
                write!(self.writer, "  [stmt_out] table_mod_ref: ")?;
                self.print_table_mod_ref_set(program, &tmr_out)?;
                writeln!(self.writer)?;
                has_any = true;
            }
        }

        // Available expressions
        if SHOW_AVAILABLE_EXPRS {
            let ae_out = if let Some(available_exprs) = &self.available_exprs {
                available_exprs.stmt_exit.get(stmt_loc).cloned()
            } else {
                None
            };
            if let Some(ae_out) = ae_out {
                self.write_indent()?;
                write!(self.writer, "  [stmt_out] available_exprs: ")?;
                self.print_available_exprs_set(program, &ae_out)?;
                writeln!(self.writer)?;
                has_any = true;
            }
        }

        if has_any {
            // Add a blank line for readability
        }
        Ok(())
    }

    fn print_operand(&mut self, program: &Program, operand: &Operand) -> io::Result<()> {
        match operand {
            Operand::Variable(var_id) => {
                self.print_variable_info(program, *var_id)?;
            }
            Operand::Constant(constant) => {
                self.print_constant_value(constant)?;
            }
            Operand::Global(global_id) => {
                let global = &program.global_consts[*global_id];
                write!(self.writer, "{}", global.name)?;
                if SHOW_VAR_IDS {
                    write!(self.writer, "[gc{}]", global_id.index())?;
                }
            }
            Operand::Table(table_id) => {
                let table = &program.tables[*table_id];
                write!(self.writer, "<table {}>", table.name)?;
                if SHOW_VAR_IDS {
                    write!(self.writer, "[tab{}]", table_id.index())?;
                }
            }
        }
        Ok(())
    }

    fn print_variable_info(&mut self, program: &Program, var_id: VariableId) -> io::Result<()> {
        let variable = &program.variables[var_id];
        write!(self.writer, "{}", variable.name)?;
        if SHOW_VAR_IDS {
            write!(self.writer, "[v{}]", var_id.index())?;
        }

        // For lambda variables, also show the function ID
        if let VariableKind::Lambda(func_id) = variable.kind {
            if SHOW_VAR_IDS {
                write!(self.writer, "[fn{}]", func_id.index())?;
            }
        }

        if SHOW_TYPE_INFO {
            write!(self.writer, ":")?;
            self.print_type_info(program, variable.ty)?;
        }
        Ok(())
    }

    fn print_type_info(&mut self, program: &Program, type_id: TypeId) -> io::Result<()> {
        let type_info = &program.types[type_id];
        match type_info {
            Type::Primitive(prim) => match prim {
                PrimitiveType::Int => write!(self.writer, "int")?,
                PrimitiveType::Float => write!(self.writer, "float")?,
                PrimitiveType::Bool => write!(self.writer, "bool")?,
                PrimitiveType::String => write!(self.writer, "string")?,
            },
            Type::Function {
                param_types,
                return_type,
            } => {
                write!(self.writer, "(")?;
                for (i, param_type) in param_types.iter().enumerate() {
                    self.print_type_info(program, *param_type)?;
                    if i < param_types.len() - 1 {
                        write!(self.writer, ", ")?;
                    }
                }
                write!(self.writer, ") -> ")?;
                self.print_type_info(program, **return_type)?;
            }
            Type::List(element_type) => {
                write!(self.writer, "[")?;
                self.print_type_info(program, *element_type)?;
                write!(self.writer, "]")?;
            }
            Type::Declared { type_id, args } => {
                let user_type = &program.user_defined_types[*type_id];
                write!(self.writer, "{}", user_type.name)?;
                if !args.is_empty() {
                    write!(self.writer, "<")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(self.writer, ", ")?;
                        }
                        self.print_type_info(program, *arg)?;
                    }
                    write!(self.writer, ">")?;
                }
            }
            Type::Table(table_id) => {
                let table = &program.tables[*table_id];
                write!(self.writer, "table_{}", table.name)?;
            }
            Type::Row { table_id } => {
                let table = &program.tables[*table_id];
                write!(self.writer, "row_{}", table.name)?;
            }
            Type::GenericParam(param_id) => {
                let param = &program.generic_params[*param_id];
                write!(self.writer, "{}", param.name)?;
            }
            Type::Void => {
                write!(self.writer, "void")?;
            }
        }
        Ok(())
    }

    fn print_constant_value(&mut self, constant: &ConstantValue) -> io::Result<()> {
        match constant {
            ConstantValue::Int(val) => write!(self.writer, "{}", val)?,
            ConstantValue::Float(val) => write!(self.writer, "{}", val)?,
            ConstantValue::Bool(val) => write!(self.writer, "{}", val)?,
            ConstantValue::String(val) => write!(self.writer, "\"{}\"", val)?,
        }
        Ok(())
    }

    fn binary_op_to_string(&self, op: &BinaryOp) -> &'static str {
        match op {
            BinaryOp::AddInt | BinaryOp::AddFloat => "+",
            BinaryOp::SubInt | BinaryOp::SubFloat => "-",
            BinaryOp::MulInt | BinaryOp::MulFloat => "*",
            BinaryOp::DivInt | BinaryOp::DivFloat => "/",
            BinaryOp::ModInt => "%",
            BinaryOp::EqInt | BinaryOp::EqFloat | BinaryOp::Eq => "==",
            BinaryOp::NeqInt | BinaryOp::NeqFloat | BinaryOp::Neq => "!=",
            BinaryOp::LtInt | BinaryOp::LtFloat => "<",
            BinaryOp::LeqInt | BinaryOp::LeqFloat => "<=",
            BinaryOp::GtInt | BinaryOp::GtFloat => ">",
            BinaryOp::GeqInt | BinaryOp::GeqFloat => ">=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
        }
    }

    fn unary_op_to_string(&self, op: &UnaryOp) -> &'static str {
        match op {
            UnaryOp::NegInt | UnaryOp::NegFloat => "-",
            UnaryOp::NotBool => "!",
        }
    }
}

impl PrettyPrint for crate::cfg::Program {
    fn pretty_print(&self, writer: &mut impl Write) -> io::Result<()> {
        let mut printer = CfgPrinter::new(writer);
        printer.print_program(self)
    }
}

impl crate::cfg::Program {
    /// Pretty print with debug information
    pub fn pretty_print_with_debug(&self, writer: &mut impl Write) -> io::Result<()> {
        let mut printer = CfgPrinter::new(writer);
        printer.print_program(self)
    }
}
