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
    BasicBlock, BasicBlockId, CfgProgram, Constant, FunctionCfg, FunctionId, HopCfg, HopId, LValue,
    Operand, Rvalue, Statement, VarId, Variable
};

// ─────────────────────────────────────────────────────────────────────────────
// CfgVisitor – traverses the *structure* of the program
// ─────────────────────────────────────────────────────────────────────────────

pub trait CfgVisitor<T> {
    fn visit_program(&mut self, program: &CfgProgram) -> T;

    fn visit_function(&mut self, program: &CfgProgram, id: FunctionId) -> T;

    fn visit_hop(&mut self, program: &CfgProgram, id: HopId) -> T;

    fn visit_basic_block(&mut self, program: &CfgProgram, id: BasicBlockId) -> T;

    fn visit_global_constants(&mut self, program: &CfgProgram, id: VarId) -> T;
}

pub trait StmtVisitor<T> {
    fn visit_statement(&mut self, stmt: &Statement) -> T;

    fn visit_lvalue(&mut self, lv: &LValue) -> T;

    fn visit_rvalue(&mut self, rv: &Rvalue) -> T;

    fn visit_operand(&mut self, op: &Operand) -> T;

    fn visit_constant(&mut self, _c: &Constant) -> T;
        
    fn visit_variable(&mut self, program: &CfgProgram, v: &Variable) -> T;
}

// ─────────────────────────────────────────────────────────────────────────────
// CfgFolder - fold will modfiy the program
// ─────────────────────────────────────────────────────────────────────────────

pub trait CfgFolder {
    fn fold_program(&mut self, program: &mut CfgProgram) -> &CfgProgram;

    fn fold_function(&mut self, program: &mut CfgProgram, id: FunctionId) -> &FunctionCfg;

    fn fold_hop(&mut self, program: &mut CfgProgram, id: HopId) -> &HopCfg;

    fn fold_basic_block(&mut self, program: &mut CfgProgram, id: BasicBlockId) -> &BasicBlock;

    fn fold_global_constants(&mut self, program: &mut CfgProgram, id: VarId) -> &Variable;
}

pub trait StmtFolder {
    fn fold_statement(&mut self, stmt: &mut Statement) -> &mut Statement;

    fn fold_lvalue(&mut self, lv: &mut LValue) -> &mut LValue;

    fn fold_rvalue(&mut self, rv: &mut Rvalue) -> &mut Rvalue;

    fn fold_operand(&mut self, op: &mut Operand) -> &mut Operand;

    fn fold_constant(&mut self, c: &mut Constant) -> &mut Constant;

    fn fold_variable(&mut self, program: &mut CfgProgram, v: &mut Variable) -> &mut Variable;
}
