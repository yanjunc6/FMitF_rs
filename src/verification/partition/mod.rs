use std::collections::{BTreeSet, HashMap};

use crate::cfg::{
    FieldId, FunctionId, FunctionKind, HopId, Instruction, InstructionKind, Operand,
    Program as CfgProgram, TableId, VariableId,
};
use crate::util::Span;
use crate::verification::base_generator::BaseVerificationGenerator;
use crate::verification::errors::Results;
use crate::verification::scope::SliceId;
use crate::verification::strategy::VerificationStrategy;
use crate::verification::Boogie::{
    BoogieBinOp, BoogieError, BoogieExpr, BoogieExprKind, BoogieLine, BoogieProcedure,
    BoogieProgram, BoogieVarDecl, ErrorMessage,
};

use super::Boogie::gen_Boogie::BoogieProgramGenerator;

/// Verification manager that checks hop-level partition consistency.
pub struct HopPartitionVerificationManager;

impl HopPartitionVerificationManager {
    pub fn new() -> Self {
        Self
    }

    /// Generate Boogie programs asserting consistent partition arguments per hop.
    pub fn generate_partition_verification(
        &mut self,
        cfg_program: &CfgProgram,
    ) -> Results<Vec<BoogieProgram>> {
        let mut programs = Vec::new();

        for &function_id in &cfg_program.all_transactions {
            let function = &cfg_program.functions[function_id];
            if function.kind != FunctionKind::Transaction || function.entry_hop.is_none() {
                continue;
            }

            let mut base = BaseVerificationGenerator::new(cfg_program);
            base.generator.program.name = format!("hop_partition_{}", function.name);

            let mut verifier = HopPartitionVerifier::new(&mut base, cfg_program, function_id);
            let procedure = verifier.run()?;
            base.generator.program.procedures.push(procedure);

            programs.push(base.generator.program);
        }

        Ok(programs)
    }
}

#[derive(Clone)]
struct PartitionAccess {
    table_id: TableId,
    operands: Vec<Operand>,
    variable_ids: Vec<Option<VariableId>>,
    first_span: Span,
}

struct HopPartitionVerifier<'a> {
    base: &'a mut BaseVerificationGenerator,
    cfg_program: &'a CfgProgram,
    function_id: FunctionId,
    hop_index_to_id: HashMap<SliceId, HopId>,
    current_slice: Option<SliceId>,
    current_accesses: HashMap<FunctionId, PartitionAccess>,
    modifies: BTreeSet<String>,
}

impl<'a> HopPartitionVerifier<'a> {
    fn new(
        base: &'a mut BaseVerificationGenerator,
        cfg_program: &'a CfgProgram,
        function_id: FunctionId,
    ) -> Self {
        let mut hop_index_to_id = HashMap::new();
        let function = &cfg_program.functions[function_id];
        for &hop_id in &function.hops {
            hop_index_to_id.insert(hop_id.index(), hop_id);
        }

        Self {
            base,
            cfg_program,
            function_id,
            hop_index_to_id,
            current_slice: None,
            current_accesses: HashMap::new(),
            modifies: BTreeSet::new(),
        }
    }

    fn run(&mut self) -> Results<BoogieProcedure> {
        let function = &self.cfg_program.functions[self.function_id];
        let params = generate_procedure_params(self.cfg_program, function);

        let procedure = BoogieProcedure {
            name: format!("verify_hop_partitions_{}", function.name),
            params,
            local_vars: Vec::new(),
            modifies: Vec::new(),
            lines: Vec::new(),
        };

        self.base.generator.program.procedures.push(procedure);
        let proc_index = self.base.generator.program.procedures.len() - 1;
        self.base.generator.set_current_procedure(proc_index);

        self.base.add_comment_to_current_procedure(format!(
            "Hop partition verification for function '{}'",
            function.name
        ));

        self.walk_and_verify(function, self.cfg_program)?;

        // Emit epilogue labels for each hop we visited so control-flow terminators remain valid.
        let mut slice_ids: Vec<_> = self.hop_index_to_id.keys().copied().collect();
        slice_ids.sort_unstable();
        for slice_id in slice_ids {
            self.base.get_mut_scope().set_current_slice(slice_id);
            let epilogue_label = self.base.get_mut_scope().get_scoped_name("epilogue");
            self.base.add_line(BoogieLine::Label(epilogue_label));
        }

        if let Some(proc) = self.base.generator.program.procedures.last_mut() {
            proc.modifies = self.modifies.iter().cloned().collect();
        }

        self.base.generator.clear_current_procedure();
        self.current_slice = None;
        self.current_accesses.clear();
        self.modifies.clear();
        let finalized = self.base.generator.program.procedures.pop().unwrap();
        Ok(finalized)
    }

    fn process_instruction(&mut self, instruction: &Instruction, slice_id: SliceId) -> Results<()> {
        self.ensure_hop_context(slice_id);
        match &instruction.kind {
            InstructionKind::TableGet { table, keys, .. } => {
                self.handle_table_access(slice_id, *table, keys, &instruction.span)
            }
            InstructionKind::TableSet {
                table, keys, field, ..
            } => {
                self.record_table_write(*table, *field);
                self.handle_table_access(slice_id, *table, keys, &instruction.span)
            }
            _ => Ok(()),
        }
    }

    fn record_table_write(&mut self, table_id: TableId, field: Option<FieldId>) {
        let table_decl = &self.cfg_program.tables[table_id];
        let push_field = |field_id: FieldId, modifies: &mut BTreeSet<String>| {
            let field_decl = &self.cfg_program.table_fields[field_id];
            let name = BoogieProgramGenerator::gen_table_field_var_name(
                &table_decl.name,
                &field_decl.name,
            );
            modifies.insert(name);
        };

        match field {
            Some(field_id) => {
                push_field(field_id, &mut self.modifies);
            }
            None => {
                for &field_id in table_decl
                    .primary_key_fields
                    .iter()
                    .chain(table_decl.other_fields.iter())
                {
                    push_field(field_id, &mut self.modifies);
                }
            }
        }
    }

    fn ensure_hop_context(&mut self, slice_id: SliceId) {
        if self.current_slice != Some(slice_id) {
            self.current_slice = Some(slice_id);
            self.current_accesses.clear();
        }
    }

    fn handle_table_access(
        &mut self,
        slice_id: SliceId,
        table_id: TableId,
        keys: &[Operand],
        span: &Span,
    ) -> Results<()> {
        let table = &self.cfg_program.tables[table_id];
        let Some(partition_function_id) = table.node_partition else {
            return Ok(());
        };

        let partition_operands = extract_partition_operands(self.cfg_program, table_id, keys);
        if partition_operands.is_empty() {
            return Ok(());
        }

        if let Some(previous) = self.current_accesses.get(&partition_function_id).cloned() {
            self.emit_partition_check(
                slice_id,
                partition_function_id,
                &previous,
                table_id,
                &partition_operands,
                span,
            )?;
        } else {
            let variable_ids = partition_operands
                .iter()
                .map(|operand| match operand {
                    Operand::Variable(var_id) => Some(*var_id),
                    _ => None,
                })
                .collect();
            self.current_accesses.insert(
                partition_function_id,
                PartitionAccess {
                    table_id,
                    operands: partition_operands.clone(),
                    variable_ids,
                    first_span: span.clone(),
                },
            );
        }
        Ok(())
    }

    fn emit_partition_check(
        &mut self,
        slice_id: SliceId,
        partition_function_id: FunctionId,
        previous: &PartitionAccess,
        table_id: TableId,
        current_operands: &[Operand],
        span: &Span,
    ) -> Results<()> {
        if previous.operands.len() != current_operands.len() {
            return Ok(());
        }

        let mut comparisons = Vec::new();
        for (prev, curr) in previous.operands.iter().zip(current_operands.iter()) {
            let prev_name = self
                .base
                .get_operand_name(prev, slice_id, self.cfg_program)?;
            let prev_expr =
                self.base
                    .generator
                    .convert_operand(self.cfg_program, prev, prev_name)?;

            let curr_name = self
                .base
                .get_operand_name(curr, slice_id, self.cfg_program)?;
            let curr_expr =
                self.base
                    .generator
                    .convert_operand(self.cfg_program, curr, curr_name)?;
            comparisons.push(BoogieExpr {
                kind: BoogieExprKind::BinOp(
                    Box::new(prev_expr),
                    BoogieBinOp::Eq,
                    Box::new(curr_expr),
                ),
            });
        }

        if comparisons.is_empty() {
            return Ok(());
        }

        let combined = comparisons
            .into_iter()
            .reduce(|acc, expr| BoogieExpr {
                kind: BoogieExprKind::BinOp(Box::new(acc), BoogieBinOp::And, Box::new(expr)),
            })
            .unwrap();

        let hop_id = match self.hop_index_to_id.get(&slice_id) {
            Some(id) => *id,
            None => return Ok(()),
        };

        let previous_table = &self.cfg_program.tables[previous.table_id];
        let table = &self.cfg_program.tables[table_id];
        let partition_func = &self.cfg_program.functions[partition_function_id];
        let variable_summary = previous
            .variable_ids
            .iter()
            .enumerate()
            .map(|(idx, opt_id)| match opt_id {
                Some(var_id) => {
                    let var = &self.cfg_program.variables[*var_id];
                    format!("k{}={}", idx, var.name)
                }
                None => format!("k{}_literal", idx),
            })
            .collect::<Vec<_>>()
            .join(", ");

        self.base.add_comment_to_current_procedure(format!(
            "Partition check hop {} func '{}' tables '{}'=>'{}' keys [{}] first_span {:?} current_span {:?}",
            hop_id.index(),
            partition_func.name,
            previous_table.name,
            table.name,
            variable_summary,
            previous.first_span,
            span
        ));

        let error_msg = ErrorMessage {
            boogie_error: BoogieError::PartitionFunctionInconsistency {
                partition_function_id: partition_function_id.index(),
                function_id: self.function_id.index(),
                hop_id: hop_id.index(),
                table_id: table_id.index(),
                span: Some(span.clone()),
            },
        };

        self.base
            .add_assertion_to_current_procedure(combined, error_msg);

        Ok(())
    }
}

impl VerificationStrategy for HopPartitionVerifier<'_> {
    fn base(&mut self) -> &mut BaseVerificationGenerator {
        self.base
    }

    fn after_instruction(&mut self, instruction: &Instruction, slice_id: SliceId) -> Results<()> {
        self.process_instruction(instruction, slice_id)
    }
}

fn generate_procedure_params(
    cfg_program: &CfgProgram,
    function: &crate::cfg::Function,
) -> Vec<BoogieVarDecl> {
    function
        .params
        .iter()
        .map(|param_id| {
            let var = &cfg_program.variables[*param_id];
            let var_type = BoogieProgramGenerator::convert_type_id(cfg_program, &var.ty);
            BoogieVarDecl {
                var_name: var.name.clone(),
                var_type,
                is_const: false,
            }
        })
        .collect()
}

fn extract_partition_operands(
    cfg_program: &CfgProgram,
    table_id: TableId,
    keys: &[Operand],
) -> Vec<Operand> {
    let table = &cfg_program.tables[table_id];
    let mut operands = Vec::new();

    for field_id in &table.node_partition_args {
        if let Some(pk_idx) = table
            .primary_key_fields
            .iter()
            .position(|pk_field| pk_field == field_id)
        {
            if pk_idx < keys.len() {
                operands.push(keys[pk_idx].clone());
            }
        }
    }

    operands
}
