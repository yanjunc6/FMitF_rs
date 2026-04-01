//! Static loop unroll optimization pass.
//!
//! This pass unrolls only canonical single-latch integer loops when an exact
//! trip count is provable at compile time from CFG operands after constant
//! folding. Unknown or unsafe loops are left unchanged.

use super::OptimizationPass;
use crate::cfg::{
    BasicBlock, BasicBlockId, BinaryOp, ConstantValue, FunctionId, FunctionKind, InstructionKind,
    Operand, Program, Terminator, VariableId,
};

pub struct LoopUnrollPass {
    max_trip_count: u32,
}

#[derive(Clone, Copy)]
struct LoopCondInfo {
    induction_var: VariableId,
    cmp_op: BinaryOp,
    bound: i64,
}

impl LoopUnrollPass {
    pub fn new(max_trip_count: u32) -> Self {
        Self { max_trip_count }
    }

    fn try_unroll_header(
        &self,
        program: &mut Program,
        func_id: FunctionId,
        header_id: BasicBlockId,
    ) -> bool {
        let (body_id, exit_id, cond_operand) = match &program.basic_blocks[header_id].terminator {
            Terminator::Branch {
                condition,
                if_true,
                if_false,
            } => (*if_true, *if_false, condition.clone()),
            _ => return false,
        };

        if header_id == body_id || header_id == exit_id || body_id == exit_id {
            return false;
        }

        if !program.functions[func_id].all_blocks.contains(&body_id)
            || !program.functions[func_id].all_blocks.contains(&exit_id)
        {
            return false;
        }

        if program.basic_blocks[header_id].hop_id != program.basic_blocks[body_id].hop_id
            || program.basic_blocks[header_id].hop_id != program.basic_blocks[exit_id].hop_id
        {
            return false;
        }

        if !matches!(program.basic_blocks[body_id].terminator, Terminator::Jump(t) if t == header_id)
        {
            return false;
        }

        // Canonical single-latch shape: header preds are {preheader, body}
        let preds = program.basic_blocks[header_id].predecessors.clone();
        if preds.len() != 2 || !preds.contains(&body_id) {
            return false;
        }
        let preheader_id = match preds.into_iter().find(|p| *p != body_id) {
            Some(v) => v,
            None => return false,
        };

        if !matches!(program.basic_blocks[preheader_id].terminator, Terminator::Jump(t) if t == header_id)
        {
            return false;
        }

        let cond_info =
            match Self::parse_condition(program, &program.basic_blocks[header_id], cond_operand) {
                Some(v) => v,
                None => return false,
            };

        let (step, update_idx) =
            match Self::parse_update(&program.basic_blocks[body_id], cond_info.induction_var) {
                Some(v) => v,
                None => return false,
            };

        if Self::has_other_induction_writes(
            &program.basic_blocks[body_id],
            cond_info.induction_var,
            update_idx,
        ) {
            return false;
        }

        let init_value =
            match Self::parse_init(&program.basic_blocks[preheader_id], cond_info.induction_var) {
                Some(v) => v,
                None => return false,
            };

        let trip_count =
            match Self::compute_trip_count(init_value, cond_info.bound, step, cond_info.cmp_op) {
                Some(v) => v,
                None => return false,
            };

        if trip_count > self.max_trip_count {
            return false;
        }

        self.rewrite_loop(
            program,
            func_id,
            header_id,
            body_id,
            preheader_id,
            exit_id,
            trip_count,
        )
    }

    fn resolve_int_operand(program: &Program, operand: &Operand) -> Option<i64> {
        match operand {
            Operand::Constant(ConstantValue::Int(v)) => Some(*v),
            Operand::Global(global_id) => match &program.global_consts[*global_id].init {
                ConstantValue::Int(v) => Some(*v),
                _ => None,
            },
            _ => None,
        }
    }

    fn parse_condition(
        program: &Program,
        block: &BasicBlock,
        cond_operand: Operand,
    ) -> Option<LoopCondInfo> {
        let cond_var = match cond_operand {
            Operand::Variable(v) => v,
            _ => return None,
        };

        let mut found = None;
        for inst in &block.instructions {
            if let InstructionKind::BinaryOp {
                dest,
                op,
                left,
                right,
            } = &inst.kind
            {
                if *dest != cond_var {
                    continue;
                }
                let (mut cmp_op, induction_var, bound) = match (left, right) {
                    (Operand::Variable(v), rhs) => {
                        let bound = Self::resolve_int_operand(program, rhs)?;
                        (*op, *v, bound)
                    }
                    (lhs, Operand::Variable(v)) => {
                        let bound = Self::resolve_int_operand(program, lhs)?;
                        (Self::flip_cmp(*op)?, *v, bound)
                    }
                    _ => return None,
                };

                cmp_op = match cmp_op {
                    BinaryOp::LtInt | BinaryOp::LeqInt | BinaryOp::GtInt | BinaryOp::GeqInt => {
                        cmp_op
                    }
                    _ => return None,
                };

                found = Some(LoopCondInfo {
                    induction_var,
                    cmp_op,
                    bound,
                });
                break;
            }
        }

        found
    }

    fn flip_cmp(op: BinaryOp) -> Option<BinaryOp> {
        match op {
            BinaryOp::LtInt => Some(BinaryOp::GtInt),
            BinaryOp::LeqInt => Some(BinaryOp::GeqInt),
            BinaryOp::GtInt => Some(BinaryOp::LtInt),
            BinaryOp::GeqInt => Some(BinaryOp::LeqInt),
            _ => None,
        }
    }

    fn parse_update(block: &BasicBlock, induction_var: VariableId) -> Option<(i64, usize)> {
        let mut update_assign_idx = None;

        for (idx, inst) in block.instructions.iter().enumerate().rev() {
            if let InstructionKind::Assign {
                dest,
                src: Operand::Variable(tmp_var),
            } = &inst.kind
            {
                if *dest != induction_var {
                    continue;
                }
                update_assign_idx = Some((idx, *tmp_var));
                break;
            }
        }

        let (assign_idx, tmp_var) = update_assign_idx?;
        if assign_idx == 0 {
            return None;
        }

        let prev = &block.instructions[assign_idx - 1];
        let (op, left, right) = match &prev.kind {
            InstructionKind::BinaryOp {
                dest,
                op,
                left,
                right,
            } if *dest == tmp_var => (*op, left, right),
            _ => return None,
        };

        let step = match op {
            BinaryOp::AddInt => match (left, right) {
                (Operand::Variable(v), Operand::Constant(ConstantValue::Int(c)))
                    if *v == induction_var =>
                {
                    *c
                }
                (Operand::Constant(ConstantValue::Int(c)), Operand::Variable(v))
                    if *v == induction_var =>
                {
                    *c
                }
                _ => return None,
            },
            BinaryOp::SubInt => match (left, right) {
                (Operand::Variable(v), Operand::Constant(ConstantValue::Int(c)))
                    if *v == induction_var =>
                {
                    -*c
                }
                _ => return None,
            },
            _ => return None,
        };

        if step == 0 {
            return None;
        }

        Some((step, assign_idx))
    }

    fn has_other_induction_writes(
        block: &BasicBlock,
        induction_var: VariableId,
        update_assign_idx: usize,
    ) -> bool {
        for (idx, inst) in block.instructions.iter().enumerate() {
            if idx == update_assign_idx {
                continue;
            }

            match &inst.kind {
                InstructionKind::Assign { dest, .. }
                | InstructionKind::BinaryOp { dest, .. }
                | InstructionKind::UnaryOp { dest, .. }
                | InstructionKind::TableGet { dest, .. } => {
                    if *dest == induction_var {
                        return true;
                    }
                }
                InstructionKind::Call { dest, .. } => {
                    if dest == &Some(induction_var) {
                        return true;
                    }
                }
                InstructionKind::TableSet { .. } | InstructionKind::Assert { .. } => {}
            }
        }

        false
    }

    fn parse_init(preheader: &BasicBlock, induction_var: VariableId) -> Option<i64> {
        for inst in preheader.instructions.iter().rev() {
            if let InstructionKind::Assign {
                dest,
                src: Operand::Constant(ConstantValue::Int(v)),
            } = &inst.kind
            {
                if *dest == induction_var {
                    return Some(*v);
                }
            }
        }
        None
    }

    fn compute_trip_count(init: i64, bound: i64, step: i64, cmp: BinaryOp) -> Option<u32> {
        if step == 0 {
            return None;
        }

        let trips: i64 = match cmp {
            BinaryOp::LtInt => {
                if step <= 0 {
                    return None;
                }
                if init >= bound {
                    0
                } else {
                    ((bound - init - 1) / step) + 1
                }
            }
            BinaryOp::LeqInt => {
                if step <= 0 {
                    return None;
                }
                if init > bound {
                    0
                } else {
                    ((bound - init) / step) + 1
                }
            }
            BinaryOp::GtInt => {
                if step >= 0 {
                    return None;
                }
                let pos_step = -step;
                if init <= bound {
                    0
                } else {
                    ((init - bound - 1) / pos_step) + 1
                }
            }
            BinaryOp::GeqInt => {
                if step >= 0 {
                    return None;
                }
                let pos_step = -step;
                if init < bound {
                    0
                } else {
                    ((init - bound) / pos_step) + 1
                }
            }
            _ => return None,
        };

        if trips < 0 {
            return None;
        }

        u32::try_from(trips).ok()
    }

    fn rewrite_loop(
        &self,
        program: &mut Program,
        func_id: FunctionId,
        header_id: BasicBlockId,
        body_id: BasicBlockId,
        preheader_id: BasicBlockId,
        exit_id: BasicBlockId,
        trip_count: u32,
    ) -> bool {
        let hop_id = program.basic_blocks[header_id].hop_id;

        if trip_count == 0 {
            program.basic_blocks[preheader_id].terminator = Terminator::Jump(exit_id);
            Self::remove_pred(
                &mut program.basic_blocks[header_id].predecessors,
                preheader_id,
            );
            Self::remove_pred(&mut program.basic_blocks[header_id].predecessors, body_id);
            Self::remove_pred(&mut program.basic_blocks[body_id].predecessors, header_id);
            Self::add_pred_unique(
                &mut program.basic_blocks[exit_id].predecessors,
                preheader_id,
            );
            return true;
        }

        // Rewire preheader to first unrolled iteration (reuse original body block).
        program.basic_blocks[preheader_id].terminator = Terminator::Jump(body_id);
        Self::remove_pred(
            &mut program.basic_blocks[header_id].predecessors,
            preheader_id,
        );
        Self::remove_pred(&mut program.basic_blocks[header_id].predecessors, body_id);
        Self::remove_pred(&mut program.basic_blocks[body_id].predecessors, header_id);
        Self::add_pred_unique(
            &mut program.basic_blocks[body_id].predecessors,
            preheader_id,
        );

        let mut prev_id = body_id;
        let body_template = program.basic_blocks[body_id].clone();

        for _ in 1..trip_count {
            let mut clone = body_template.clone();
            clone.predecessors.clear();
            clone.predecessors.push(prev_id);
            let new_id = program.basic_blocks.alloc(clone);

            program.functions[func_id].all_blocks.push(new_id);
            program.hops[hop_id].blocks.push(new_id);

            program.basic_blocks[prev_id].terminator = Terminator::Jump(new_id);
            prev_id = new_id;
        }

        program.basic_blocks[prev_id].terminator = Terminator::Jump(exit_id);
        Self::add_pred_unique(&mut program.basic_blocks[exit_id].predecessors, prev_id);

        true
    }

    fn remove_pred(preds: &mut Vec<BasicBlockId>, pred: BasicBlockId) {
        preds.retain(|p| *p != pred);
    }

    fn add_pred_unique(preds: &mut Vec<BasicBlockId>, pred: BasicBlockId) {
        if !preds.contains(&pred) {
            preds.push(pred);
        }
    }
}

impl OptimizationPass for LoopUnrollPass {
    fn optimize_function(&self, program: &mut Program, func_id: FunctionId) -> bool {
        if matches!(program.functions[func_id].kind, FunctionKind::Operator) {
            return false;
        }

        let mut changed = false;

        // Keep iterating because rewriting one loop can expose another simple loop.
        'outer: loop {
            let block_ids = program.functions[func_id].all_blocks.clone();
            for block_id in block_ids {
                if !program.functions[func_id].all_blocks.contains(&block_id) {
                    continue;
                }

                if self.try_unroll_header(program, func_id, block_id) {
                    changed = true;
                    continue 'outer;
                }
            }
            break;
        }

        changed
    }

    fn name(&self) -> &'static str {
        "loop-unroll"
    }
}
