// cfg_api.rs
//
// Visitor pattern and Fold pattern
// – two-layer visitor API for the CFG.
//
//  ┌──────────────┐
//  │  CfgVisitor  │   – “outer” visitor (program / function / hop / bb / stmt)
//  └──────────────┘
//  ┌──────────────┐
//  │ StmtVisitor  │   – “inner” visitor (stmt / lvalue / rvalue / operand / constant)
//  └──────────────┘
//
//  A typical analysis will implement *either* trait or both, depending on scope.

use super::{
    BasicBlock, BasicBlockId, ConstantValue, Function, FunctionId, GlobalConst, GlobalConstId, Hop,
    HopId, Instruction, Operand, Program, Terminator, Variable,
};

// ─────────────────────────────────────────────────────────────────────────────
// CfgVisitor – traverses the *structure* of the program
// ─────────────────────────────────────────────────────────────────────────────

pub trait CfgVisitor<T> {
    fn visit_program(&mut self, program: &Program) -> T;

    fn visit_function(&mut self, program: &Program, id: FunctionId) -> T;

    fn visit_hop(&mut self, program: &Program, id: HopId) -> T;

    fn visit_basic_block(&mut self, program: &Program, id: BasicBlockId) -> T;

    fn visit_global_constants(&mut self, program: &Program, id: GlobalConstId) -> T;
}

pub trait StmtVisitor<T> {
    fn visit_instruction(&mut self, inst: &Instruction) -> T;

    fn visit_terminator(&mut self, term: &Terminator) -> T;

    fn visit_operand(&mut self, op: &Operand) -> T;

    fn visit_constant(&mut self, _c: &ConstantValue) -> T;

    fn visit_variable(&mut self, program: &Program, v: &Variable) -> T;
}

// ─────────────────────────────────────────────────────────────────────────────
// CfgFolder - fold will modfiy the program
// ─────────────────────────────────────────────────────────────────────────────

pub trait CfgFolder {
    fn fold_program(&mut self, program: &mut Program) -> &Program;

    fn fold_function(&mut self, program: &mut Program, id: FunctionId) -> &Function;

    fn fold_hop(&mut self, program: &mut Program, id: HopId) -> &Hop;

    fn fold_basic_block(&mut self, program: &mut Program, id: BasicBlockId) -> &BasicBlock;

    fn fold_global_constants(&mut self, program: &mut Program, id: GlobalConstId) -> &GlobalConst;
}

pub trait StmtFolder {
    fn fold_instruction(&mut self, inst: &mut Instruction) -> &mut Instruction;

    fn fold_terminator(&mut self, term: &mut Terminator) -> &mut Terminator;

    fn fold_operand(&mut self, op: &mut Operand) -> &mut Operand;

    fn fold_constant(&mut self, c: &mut ConstantValue) -> &mut ConstantValue;

    fn fold_variable(&mut self, program: &mut Program, v: &mut Variable) -> &mut Variable;
}
