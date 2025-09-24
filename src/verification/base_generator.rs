// In src/verification/base_generator.rs

use super::Boogie::{
    gen_Boogie::BoogieProgramGenerator, BoogieExpr, BoogieExprKind, BoogieLine, BoogieProgram,
};
use crate::cfg::{BasicBlock, Instruction, Operand, Program as CfgProgram};
use crate::verification::{
    errors::Results,
    scope::{ExecutionScope, SliceId},
};

pub struct BaseVerificationGenerator {
    pub cfg_program: CfgProgram,
    // The scope is now a fundamental part of the generator.
    scope: ExecutionScope,
    pub generator: BoogieProgramGenerator,
}

impl BaseVerificationGenerator {
    pub fn new(cfg_program: CfgProgram) -> Self {
        let boogie_program_name = "verification".to_string();
        let generator = BoogieProgramGenerator::from_cfg(boogie_program_name, &cfg_program);
        Self {
            cfg_program,
            scope: ExecutionScope::default(),
            generator,
        }
    }

    pub fn get_boogie_program(&self) -> &BoogieProgram {
        &self.generator.program
    }

    pub fn get_mut_scope(&mut self) -> &mut ExecutionScope {
        &mut self.scope
    }

    pub fn add_line(&mut self, line: BoogieLine) {
        if let Some(p) = self.generator.get_current_procedure_mut() {
            p.lines.push(line);
        }
    }

    pub fn add_lines(&mut self, lines: Vec<BoogieLine>) {
        if let Some(p) = self.generator.get_current_procedure_mut() {
            p.lines.extend(lines);
        }
    }

    // This is the new central method for converting instructions.
    pub fn convert_instruction(
        &mut self,
        instruction: &Instruction,
        slice_id: SliceId,
    ) -> Results<Vec<BoogieLine>> {
        self.scope.set_current_slice(slice_id);
        let mut lines = Vec::new();
        match instruction {
            Instruction::Assign { dest, src } => {
                let dest_name = self
                    .scope
                    .get_scoped_variable_name(&self.cfg_program, *dest);
                let dest_type = BoogieProgramGenerator::convert_type_id(
                    &self.cfg_program,
                    &self.cfg_program.variables[*dest].ty,
                );
                self.generator
                    .ensure_local_variable_exists(&dest_name, dest_type);

                let src_name = self.get_operand_name(src, slice_id)?;
                let src_expr = self
                    .generator
                    .convert_operand(&self.cfg_program, src, src_name)?;

                lines.push(BoogieLine::Assign(dest_name, src_expr));
            }
            Instruction::BinaryOp {
                dest,
                op,
                left,
                right,
            } => {
                let dest_name = self
                    .scope
                    .get_scoped_variable_name(&self.cfg_program, *dest);
                let dest_type = BoogieProgramGenerator::convert_type_id(
                    &self.cfg_program,
                    &self.cfg_program.variables[*dest].ty,
                );
                self.generator
                    .ensure_local_variable_exists(&dest_name, dest_type);

                let left_name = self.get_operand_name(left, slice_id)?;
                let right_name = self.get_operand_name(right, slice_id)?;

                let left_expr =
                    self.generator
                        .convert_operand(&self.cfg_program, left, left_name)?;
                let right_expr =
                    self.generator
                        .convert_operand(&self.cfg_program, right, right_name)?;

                let boogie_op = BoogieProgramGenerator::convert_binary_op(op);
                let bin_expr = BoogieExpr {
                    kind: BoogieExprKind::BinOp(
                        Box::new(left_expr),
                        boogie_op,
                        Box::new(right_expr),
                    ),
                };
                lines.push(BoogieLine::Assign(dest_name, bin_expr));
            }
            Instruction::UnaryOp { dest, op, operand } => {
                let dest_name = self
                    .scope
                    .get_scoped_variable_name(&self.cfg_program, *dest);
                let dest_type = BoogieProgramGenerator::convert_type_id(
                    &self.cfg_program,
                    &self.cfg_program.variables[*dest].ty,
                );
                self.generator
                    .ensure_local_variable_exists(&dest_name, dest_type);

                let operand_name = self.get_operand_name(operand, slice_id)?;
                let expr =
                    self.generator
                        .convert_operand(&self.cfg_program, operand, operand_name)?;

                let boogie_op = self.generator.convert_unary_op(op)?;
                let un_expr = BoogieExpr {
                    kind: BoogieExprKind::UnOp(boogie_op, Box::new(expr)),
                };
                lines.push(BoogieLine::Assign(dest_name, un_expr));
            }
            Instruction::Call { dest, func, args } => {
                let func_decl = &self.cfg_program.functions[*func];
                let func_name = func_decl.name.clone();
                let mut boogie_args = Vec::new();
                for arg in args {
                    let arg_name = self.get_operand_name(arg, slice_id)?;
                    boogie_args.push(self.generator.convert_operand(
                        &self.cfg_program,
                        arg,
                        arg_name,
                    )?);
                }
                let call_expr = BoogieExpr {
                    kind: BoogieExprKind::FunctionCall {
                        name: func_name,
                        args: boogie_args,
                    },
                };
                if let Some(dest_id) = dest {
                    let dest_name = self
                        .scope
                        .get_scoped_variable_name(&self.cfg_program, *dest_id);
                    let dest_type = BoogieProgramGenerator::convert_type_id(
                        &self.cfg_program,
                        &self.cfg_program.variables[*dest_id].ty,
                    );
                    self.generator
                        .ensure_local_variable_exists(&dest_name, dest_type);
                    lines.push(BoogieLine::Assign(dest_name, call_expr));
                } else {
                    // This would be a procedure call, which needs a different BoogieLine variant if it's not an expression
                }
            }
            Instruction::TableGet {
                dest,
                table,
                keys,
                field,
            } => {
                let dest_name = self
                    .scope
                    .get_scoped_variable_name(&self.cfg_program, *dest);
                let dest_type = BoogieProgramGenerator::convert_type_id(
                    &self.cfg_program,
                    &self.cfg_program.variables[*dest].ty,
                );
                self.generator
                    .ensure_local_variable_exists(&dest_name, dest_type);

                let table_decl = &self.cfg_program.tables[*table];
                let field_decl = &self.cfg_program.table_fields[field.unwrap()];
                let var_name = BoogieProgramGenerator::gen_table_field_var_name(
                    &table_decl.name,
                    &field_decl.name,
                );
                let mut key_exprs = Vec::new();
                for key in keys {
                    let key_name = self.get_operand_name(key, slice_id)?;
                    key_exprs.push(self.generator.convert_operand(
                        &self.cfg_program,
                        key,
                        key_name,
                    )?);
                }
                let map_select = BoogieExpr {
                    kind: BoogieExprKind::MapSelect {
                        base: Box::new(BoogieExpr {
                            kind: BoogieExprKind::Var(var_name),
                        }),
                        indices: key_exprs,
                    },
                };
                lines.push(BoogieLine::Assign(dest_name, map_select));
            }
            Instruction::TableSet {
                table,
                keys,
                field,
                value,
            } => {
                let table_decl = &self.cfg_program.tables[*table];
                let field_decl = &self.cfg_program.table_fields[field.unwrap()];
                let var_name = BoogieProgramGenerator::gen_table_field_var_name(
                    &table_decl.name,
                    &field_decl.name,
                );

                let mut key_exprs = Vec::new();
                for key in keys {
                    let key_name = self.get_operand_name(key, slice_id)?;
                    key_exprs.push(self.generator.convert_operand(
                        &self.cfg_program,
                        key,
                        key_name,
                    )?);
                }
                let value_name = self.get_operand_name(value, slice_id)?;
                let value_expr =
                    self.generator
                        .convert_operand(&self.cfg_program, value, value_name)?;

                let map_store = BoogieExpr {
                    kind: BoogieExprKind::MapStore {
                        base: Box::new(BoogieExpr {
                            kind: BoogieExprKind::Var(var_name.clone()),
                        }),
                        indices: key_exprs,
                        value: Box::new(value_expr),
                    },
                };
                lines.push(BoogieLine::Assign(var_name, map_store));
            }
            Instruction::Assert { .. } => {
                // let cond_name = self.get_operand_name(condition)?;
                // let cond_expr = self
                //     .generator
                //     .convert_operand(&self.cfg_program, condition, cond_name)?;
                // let boogie_error = super::Boogie::BoogieError::GenericAssertionFailure {
                //     message: message.clone(),
                // };
                // lines.push(BoogieLine::Assert(cond_expr, ErrorMessage { boogie_error }));
                // for now, we skip assertions in the base generator
            }
        }
        Ok(lines)
    }

    fn get_operand_name(&mut self, operand: &Operand, slice_id: SliceId) -> Results<String> {
        self.scope.set_current_slice(slice_id);
        match operand {
            Operand::Variable(var_id) => Ok(self
                .scope
                .get_scoped_variable_name(&self.cfg_program, *var_id)),
            Operand::Constant(c) => Ok(self.generator.convert_constant(c)?.to_string()),
            Operand::Global(g) => {
                let g_const = &self.cfg_program.global_consts[*g];
                Ok(g_const.name.clone())
            }
        }
    }

    pub fn default_generate_block_edges(
        &mut self,
        block: &BasicBlock,
        slice_id: SliceId,
    ) -> Results<Vec<BoogieLine>> {
        self.scope.set_current_slice(slice_id);
        let mut lines = Vec::new();
        match &block.terminator {
            crate::cfg::Terminator::Jump(target) => {
                let target_label = self.scope.get_scoped_label(*target);
                lines.push(BoogieLine::Goto(target_label));
            }
            crate::cfg::Terminator::Branch {
                condition,
                if_true,
                if_false,
            } => {
                let cond_name = self.get_operand_name(condition, slice_id)?;
                let cond_expr =
                    self.generator
                        .convert_operand(&self.cfg_program, condition, cond_name)?;
                let true_label = self.scope.get_scoped_label(*if_true);
                let false_label = self.scope.get_scoped_label(*if_false);
                lines.push(BoogieLine::If {
                    cond: cond_expr,
                    then_body: vec![Box::new(BoogieLine::Goto(true_label))],
                    else_body: vec![Box::new(BoogieLine::Goto(false_label))],
                });
            }
            crate::cfg::Terminator::Return(ret_val) => {
                if let Some(op) = ret_val {
                    let ret_name = self.get_operand_name(op, slice_id)?;
                    let ret_expr =
                        self.generator
                            .convert_operand(&self.cfg_program, op, ret_name)?;
                    // Assuming a special return variable `__ret`
                    lines.push(BoogieLine::Assign(
                        self.scope.get_scoped_name("__ret"),
                        ret_expr,
                    ));
                }
                lines.push(BoogieLine::Goto(self.scope.get_scoped_name("epilogue")));
            }
            crate::cfg::Terminator::HopExit { .. } => {
                // This needs strategy-specific handling.
                // For now, we can just goto an exit label.
                lines.push(BoogieLine::Goto(self.scope.get_scoped_name("hop_exit")));
            }
            crate::cfg::Terminator::Abort => {
                lines.push(BoogieLine::Assume(super::Boogie::BoogieExpr {
                    kind: super::Boogie::BoogieExprKind::BoolConst(false),
                }));
            }
        }
        Ok(lines)
    }
}
