use std::collections::{BTreeSet, HashMap};

use crate::cfg::{
    FunctionId, FunctionKind, HopId, Instruction, InstructionKind, Operand, Program as CfgProgram,
    TableId,
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

            let modifies: Vec<String> = collect_modified_field_names(cfg_program, function_id)
                .into_iter()
                .collect();

            let mut verifier =
                HopPartitionVerifier::new(&mut base, cfg_program, function_id, modifies);
            let procedure = verifier.run()?;
            base.generator.program.procedures.push(procedure);

            programs.push(base.generator.program);
        }

        Ok(programs)
    }
}

struct HopPartitionVerifier<'a> {
    base: &'a mut BaseVerificationGenerator,
    cfg_program: &'a CfgProgram,
    function_id: FunctionId,
    hop_index_to_id: HashMap<SliceId, HopId>,
    hop_partition_state: HashMap<SliceId, HashMap<FunctionId, Vec<Operand>>>,
    modifies: Vec<String>,
}

impl<'a> HopPartitionVerifier<'a> {
    fn new(
        base: &'a mut BaseVerificationGenerator,
        cfg_program: &'a CfgProgram,
        function_id: FunctionId,
        modifies: Vec<String>,
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
            hop_partition_state: HashMap::new(),
            modifies,
        }
    }

    fn run(&mut self) -> Results<BoogieProcedure> {
        let function = &self.cfg_program.functions[self.function_id];
        let params = generate_procedure_params(self.cfg_program, function);
        let modifies = self.modifies.clone();

        let procedure = BoogieProcedure {
            name: format!("verify_hop_partitions_{}", function.name),
            params,
            local_vars: Vec::new(),
            modifies,
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

        let mut slice_ids: Vec<_> = self.hop_index_to_id.keys().copied().collect();
        slice_ids.sort_unstable();
        for slice_id in slice_ids {
            self.base.get_mut_scope().set_current_slice(slice_id);
            let epilogue_label = self.base.get_mut_scope().get_scoped_name("epilogue");
            self.base.add_line(BoogieLine::Label(epilogue_label));
        }

        self.base.generator.clear_current_procedure();
        let finalized = self.base.generator.program.procedures.pop().unwrap();
        Ok(finalized)
    }

    fn process_instruction(&mut self, instruction: &Instruction, slice_id: SliceId) -> Results<()> {
        match &instruction.kind {
            InstructionKind::TableGet { table, keys, .. }
            | InstructionKind::TableSet { table, keys, .. } => {
                self.record_table_access(*table, keys, slice_id, &instruction.span)
            }
            _ => Ok(()),
        }
    }

    fn record_table_access(
        &mut self,
        table_id: TableId,
        keys: &[Operand],
        slice_id: SliceId,
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

        let previous_operands = self
            .hop_partition_state
            .get(&slice_id)
            .and_then(|entries| entries.get(&partition_function_id))
            .cloned();

        if let Some(prev_ops) = previous_operands.as_ref() {
            self.emit_partition_check(
                slice_id,
                partition_function_id,
                table_id,
                prev_ops,
                &partition_operands,
                span,
            )?;
        }

        self.hop_partition_state
            .entry(slice_id)
            .or_insert_with(HashMap::new)
            .insert(partition_function_id, partition_operands);
        Ok(())
    }

    fn emit_partition_check(
        &mut self,
        slice_id: SliceId,
        partition_function_id: FunctionId,
        table_id: TableId,
        previous_operands: &[Operand],
        current_operands: &[Operand],
        span: &Span,
    ) -> Results<()> {
        if previous_operands.len() != current_operands.len() {
            return Ok(());
        }

        self.base.get_mut_scope().set_current_slice(slice_id);

        let mut comparisons = Vec::new();
        for (prev, curr) in previous_operands.iter().zip(current_operands.iter()) {
            let prev_expr = self.operand_expr(prev, slice_id)?;
            let curr_expr = self.operand_expr(curr, slice_id)?;
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

        let table = &self.cfg_program.tables[table_id];
        let partition_func = &self.cfg_program.functions[partition_function_id];

        self.base.add_comment_to_current_procedure(format!(
            "Partition check: hop {}, partition '{}', table '{}'",
            hop_id.index(),
            partition_func.name,
            table.name
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

    fn operand_expr(&mut self, operand: &Operand, slice_id: SliceId) -> Results<BoogieExpr> {
        self.base.get_mut_scope().set_current_slice(slice_id);
        match operand {
            Operand::Variable(var_id) => {
                let name = {
                    let scope = self.base.get_mut_scope();
                    scope.get_scoped_variable_name(self.cfg_program, *var_id)
                };
                let var_type = BoogieProgramGenerator::convert_type_id(
                    self.cfg_program,
                    &self.cfg_program.variables[*var_id].ty,
                );
                self.base
                    .generator
                    .ensure_local_variable_exists(&name, var_type);
                Ok(BoogieExpr {
                    kind: BoogieExprKind::Var(name),
                })
            }
            Operand::Constant(constant) => self.base.generator.convert_constant(constant),
            Operand::Global(global_id) => {
                let global_const = &self.cfg_program.global_consts[*global_id];
                if global_const.name == "__slice__" {
                    Ok(BoogieExpr {
                        kind: BoogieExprKind::IntConst(slice_id as i64),
                    })
                } else {
                    Ok(BoogieExpr {
                        kind: BoogieExprKind::Var(global_const.name.clone()),
                    })
                }
            }
            Operand::Table(table_id) => {
                let table = &self.cfg_program.tables[*table_id];
                Ok(BoogieExpr {
                    kind: BoogieExprKind::Var(format!("TBL_{}", table.name)),
                })
            }
        }
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

fn collect_modified_field_names(
    cfg_program: &CfgProgram,
    function_id: FunctionId,
) -> BTreeSet<String> {
    let mut modifies = BTreeSet::new();
    let function = &cfg_program.functions[function_id];

    for &hop_id in &function.hops {
        let hop = &cfg_program.hops[hop_id];
        for &block_id in &hop.blocks {
            let block = &cfg_program.basic_blocks[block_id];
            for instruction in &block.instructions {
                if let InstructionKind::TableSet { table, field, .. } = &instruction.kind {
                    if let Some(field_id) = field {
                        let table_decl = &cfg_program.tables[*table];
                        let field_decl = &cfg_program.table_fields[*field_id];
                        let name = BoogieProgramGenerator::gen_table_field_var_name(
                            &table_decl.name,
                            &field_decl.name,
                        );
                        modifies.insert(name);
                    }
                }
            }
        }
    }

    modifies
}
