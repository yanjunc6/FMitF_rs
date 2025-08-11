use crate::cfg::{CfgProgram, FunctionId, LValue, Operand, Rvalue, Statement, VarId};
use crate::optimization::OptimizationPass;
use std::collections::HashMap;

/// Copy Propagation optimization pass
/// Replaces variable copies (x = y) with direct uses of the source variable
pub struct CopyPropagation {
    debug: bool,
}

impl CopyPropagation {
    pub fn new() -> Self {
        Self { debug: false }
    }

    pub fn with_debug(mut self) -> Self {
        self.debug = true;
        self
    }

    /// Run copy propagation optimization on a function
    pub fn run(&self, program: &mut CfgProgram, function_id: FunctionId) -> bool {
        let mut changed = false;

        if let Some(function) = program.functions.get_mut(function_id) {
            for hop_id in function.hops.clone() {
                if let Some(hop) = program.hops.get_mut(hop_id) {
                    for block_id in hop.blocks.clone() {
                        if let Some(block) = program.blocks.get_mut(block_id) {
                            // Build copy map for this block
                            let copy_map = self.build_copy_map(&block.statements);

                            if self.debug && !copy_map.is_empty() {
                                eprintln!("        COPY PROPAGATION DEBUG:");
                                for (target, source) in &copy_map {
                                    eprintln!(
                                        "        Copy identified: {:?} -> {:?}",
                                        target, source
                                    );
                                }
                                eprintln!("        Built copy map with {} entries", copy_map.len());
                            }

                            // Apply copy propagation to statements
                            let mut new_statements = Vec::new();
                            for (idx, stmt) in block.statements.iter().enumerate() {
                                let new_stmt = self.propagate_copies_in_statement(stmt, &copy_map);

                                if self.debug {
                                    if new_stmt != *stmt {
                                        eprintln!(
                                            "        [{}] COPY PROPAGATED: {:?} -> {:?}",
                                            idx, stmt, new_stmt
                                        );
                                        changed = true;
                                    } else {
                                        eprintln!("        [{}] UNCHANGED: {:?}", idx, stmt);
                                    }
                                } else if new_stmt != *stmt {
                                    changed = true;
                                }

                                new_statements.push(new_stmt);
                            }

                            block.statements = new_statements;
                        }
                    }
                }
            }
        }

        changed
    }

    /// Build a map of copy relationships (target -> source)
    /// Only simple copies like `x = y` are tracked
    fn build_copy_map(&self, statements: &[Statement]) -> HashMap<VarId, VarId> {
        let mut copy_map = HashMap::new();

        for stmt in statements {
            match stmt {
                Statement::Assign { lvalue, rvalue, .. } => {
                    if let LValue::Variable { var } = lvalue {
                        if let Rvalue::Use(Operand::Var(source_var)) = rvalue {
                            // This is a simple copy: target = source
                            copy_map.insert(*var, *source_var);
                        }
                    }
                }
            }
        }

        // Follow copy chains and resolve to final sources
        let mut resolved_map = HashMap::new();
        for (&target, &source) in &copy_map {
            resolved_map.insert(target, self.resolve_copy_chain(&copy_map, source));
        }

        if self.debug && copy_map.is_empty() {
            eprintln!("        COPY PROPAGATION DEBUG:");
            eprintln!("        No copies found to propagate");
        }

        resolved_map
    }

    /// Resolve copy chains like x->y, y->z to x->z
    fn resolve_copy_chain(&self, copy_map: &HashMap<VarId, VarId>, mut var: VarId) -> VarId {
        let mut visited = std::collections::HashSet::new();

        while let Some(&next_var) = copy_map.get(&var) {
            if visited.contains(&var) {
                // Cycle detected, return current variable
                break;
            }
            visited.insert(var);
            var = next_var;
        }

        var
    }

    /// Apply copy propagation to a single statement
    fn propagate_copies_in_statement(
        &self,
        stmt: &Statement,
        copy_map: &HashMap<VarId, VarId>,
    ) -> Statement {
        match stmt {
            Statement::Assign {
                lvalue,
                rvalue,
                span,
            } => {
                let new_lvalue = self.propagate_copies_in_lvalue(lvalue, copy_map);
                let new_rvalue = self.propagate_copies_in_rvalue(rvalue, copy_map);

                Statement::Assign {
                    lvalue: new_lvalue,
                    rvalue: new_rvalue,
                    span: span.clone(),
                }
            }
        }
    }

    /// Apply copy propagation to an LValue
    fn propagate_copies_in_lvalue(
        &self,
        lvalue: &LValue,
        copy_map: &HashMap<VarId, VarId>,
    ) -> LValue {
        match lvalue {
            LValue::Variable { var } => LValue::Variable { var: *var },
            LValue::TableField {
                table,
                pk_fields,
                pk_values,
                field,
            } => {
                let new_pk_values: Vec<Operand> = pk_values
                    .iter()
                    .map(|operand| self.propagate_copies_in_operand(operand, copy_map))
                    .collect();

                LValue::TableField {
                    table: *table,
                    pk_fields: pk_fields.clone(),
                    pk_values: new_pk_values,
                    field: *field,
                }
            }
            LValue::ArrayElement { array, index } => LValue::ArrayElement {
                array: *array, // ArrayElement uses VarId directly, not Operand
                index: self.propagate_copies_in_operand(index, copy_map),
            },
        }
    }

    /// Apply copy propagation to an RValue
    fn propagate_copies_in_rvalue(
        &self,
        rvalue: &Rvalue,
        copy_map: &HashMap<VarId, VarId>,
    ) -> Rvalue {
        match rvalue {
            Rvalue::Use(operand) => {
                Rvalue::Use(self.propagate_copies_in_operand(operand, copy_map))
            }
            Rvalue::TableAccess {
                table,
                pk_fields,
                pk_values,
                field,
            } => {
                let new_pk_values: Vec<Operand> = pk_values
                    .iter()
                    .map(|operand| self.propagate_copies_in_operand(operand, copy_map))
                    .collect();

                Rvalue::TableAccess {
                    table: *table,
                    pk_fields: pk_fields.clone(),
                    pk_values: new_pk_values,
                    field: *field,
                }
            }
            Rvalue::ArrayAccess { array, index } => Rvalue::ArrayAccess {
                array: self.propagate_copies_in_operand(array, copy_map),
                index: self.propagate_copies_in_operand(index, copy_map),
            },
            Rvalue::BinaryOp { op, left, right } => Rvalue::BinaryOp {
                op: op.clone(),
                left: self.propagate_copies_in_operand(left, copy_map),
                right: self.propagate_copies_in_operand(right, copy_map),
            },
            Rvalue::UnaryOp { op, operand } => Rvalue::UnaryOp {
                op: op.clone(),
                operand: self.propagate_copies_in_operand(operand, copy_map),
            },
        }
    }

    /// Apply copy propagation to an operand
    fn propagate_copies_in_operand(
        &self,
        operand: &Operand,
        copy_map: &HashMap<VarId, VarId>,
    ) -> Operand {
        match operand {
            Operand::Var(var_id) => {
                // Replace with source if this is a copy target
                if let Some(&source_var) = copy_map.get(var_id) {
                    Operand::Var(source_var)
                } else {
                    operand.clone()
                }
            }
            Operand::Const(_) => operand.clone(),
        }
    }
}

impl OptimizationPass for CopyPropagation {
    fn optimize_function(&self, program: &mut CfgProgram, func_id: FunctionId) -> bool {
        self.run(program, func_id)
    }

    fn name(&self) -> &'static str {
        "Copy Propagation"
    }
}
