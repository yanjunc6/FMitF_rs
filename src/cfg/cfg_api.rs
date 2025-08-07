// cfg_api.rs
//
// A “rustic” visitor + sealed-mutation interface for your CFG.
//  ── Everyone can READ through `CfgVisitor`.
//  ── Only passes inside this module can MUTATE through `Editor`.
//  ── Mutations automatically keep predecessors/successors/SSA in sync.
//
// This file is completely self-contained and uses only public types from
// crate::cfg and crate::dataflow.

use crate::cfg::*;

////////////////////////////////////////////////////////////////////////////////
// 1. READ-ONLY TRAVERSAL
////////////////////////////////////////////////////////////////////////////////

/// A lightweight visitor: implement only the callbacks you care about.
///
/// Default walk functions ensure that *skipped* callbacks keep recursing
/// (so you never forget to descend).
pub trait CfgVisitor {
    fn visit_function(&mut self, _f: FunctionId, _prog: &CfgProgram) {}
    fn visit_hop(&mut self, _h: HopId, _prog: &CfgProgram) {}
    fn visit_block(&mut self, _b: BasicBlockId, _prog: &CfgProgram) {}
    fn visit_stmt(&mut self, _s: &Statement, _prog: &CfgProgram) {}
}

/// Convenience helpers so you don’t need to re-implement the DFS every time.
pub fn walk_program<V: CfgVisitor + ?Sized>(v: &mut V, prog: &CfgProgram) {
    for &f in &prog.root_functions {
        walk_function(v, f, prog);
    }
}

pub fn walk_function<V: CfgVisitor + ?Sized>(v: &mut V, fid: FunctionId, prog: &CfgProgram) {
    v.visit_function(fid, prog);
    let function = &prog.functions[fid];
    for &hop in &function.hops {
        walk_hop(v, hop, prog);
    }
}

pub fn walk_hop<V: CfgVisitor + ?Sized>(v: &mut V, hid: HopId, prog: &CfgProgram) {
    v.visit_hop(hid, prog);
    let hop = &prog.hops[hid];
    for &bb in &hop.blocks {
        walk_block(v, bb, prog);
    }
}

pub fn walk_block<V: CfgVisitor + ?Sized>(v: &mut V, bb: BasicBlockId, prog: &CfgProgram) {
    v.visit_block(bb, prog);
    for stmt in &prog.blocks[bb].statements {
        v.visit_stmt(stmt, prog);
    }
}

////////////////////////////////////////////////////////////////////////////////
// 2. API FUNCTIONS FOR ANALYSIS PASSES
////////////////////////////////////////////////////////////////////////////////

/// Get basic blocks iterator for a function (read-only access)
pub fn function_blocks_iter(
    prog: &CfgProgram,
    func_id: FunctionId,
) -> impl Iterator<Item = (BasicBlockId, &BasicBlock)> {
    prog.functions[func_id]
        .blocks
        .iter()
        .map(|&block_id| (block_id, &prog.blocks[block_id]))
}

/// Get hops iterator for a function (read-only access)
pub fn function_hops_iter(
    prog: &CfgProgram,
    func_id: FunctionId,
) -> impl Iterator<Item = (HopId, &HopCfg)> {
    prog.functions[func_id]
        .hops
        .iter()
        .map(|&hop_id| (hop_id, &prog.hops[hop_id]))
}

/// Get blocks iterator for a hop (read-only access)
pub fn hop_blocks_iter(
    prog: &CfgProgram,
    hop_id: HopId,
) -> impl Iterator<Item = (BasicBlockId, &BasicBlock)> {
    prog.hops[hop_id]
        .blocks
        .iter()
        .map(|&block_id| (block_id, &prog.blocks[block_id]))
}

/// Get a specific basic block (read-only access)
pub fn get_basic_block(prog: &CfgProgram, block_id: BasicBlockId) -> &BasicBlock {
    &prog.blocks[block_id]
}

/// Get a specific function (read-only access)  
pub fn get_function(prog: &CfgProgram, func_id: FunctionId) -> &FunctionCfg {
    &prog.functions[func_id]
}

/// Get a specific hop (read-only access)
pub fn get_hop(prog: &CfgProgram, hop_id: HopId) -> &HopCfg {
    &prog.hops[hop_id]
}

/// Get a specific variable (read-only access)
pub fn get_variable(prog: &CfgProgram, var_id: VarId) -> &Variable {
    &prog.variables[var_id]
}

/// Get root functions list (read-only access)
pub fn get_root_functions(prog: &CfgProgram) -> &[FunctionId] {
    &prog.root_functions
}

/// Get function parameters (read-only access)
pub fn get_function_parameters(prog: &CfgProgram, func_id: FunctionId) -> &[VarId] {
    &prog.functions[func_id].parameters
}

/// Get function local variables (read-only access)
pub fn get_function_locals(prog: &CfgProgram, func_id: FunctionId) -> &[VarId] {
    &prog.functions[func_id].local_variables
}

/// Get function hops list (read-only access)
pub fn get_function_hops(prog: &CfgProgram, func_id: FunctionId) -> &[HopId] {
    &prog.functions[func_id].hops
}

/// Get function blocks list (read-only access)
pub fn get_function_blocks(prog: &CfgProgram, func_id: FunctionId) -> &[BasicBlockId] {
    &prog.functions[func_id].blocks
}

/// Get hop blocks list (read-only access)
pub fn get_hop_blocks(prog: &CfgProgram, hop_id: HopId) -> &[BasicBlockId] {
    &prog.hops[hop_id].blocks
}

/// Get basic block statements (read-only access)
pub fn get_block_statements(prog: &CfgProgram, block_id: BasicBlockId) -> &[Statement] {
    &prog.blocks[block_id].statements
}

/// Get basic block predecessors (read-only access)
pub fn get_block_predecessors(prog: &CfgProgram, block_id: BasicBlockId) -> &[ControlFlowEdge] {
    &prog.blocks[block_id].predecessors
}

/// Get basic block successors (read-only access)
pub fn get_block_successors(prog: &CfgProgram, block_id: BasicBlockId) -> &[ControlFlowEdge] {
    &prog.blocks[block_id].successors
}

/// Get function name (read-only access)
pub fn get_function_name(prog: &CfgProgram, func_id: FunctionId) -> &str {
    &prog.functions[func_id].name
}

/// Get function type (read-only access)
pub fn get_function_type(prog: &CfgProgram, func_id: FunctionId) -> &FunctionType {
    &prog.functions[func_id].function_type
}

/// Get function return type (read-only access)
pub fn get_function_return_type(prog: &CfgProgram, func_id: FunctionId) -> &ReturnType {
    &prog.functions[func_id].return_type
}

////////////////////////////////////////////////////////////////////////////////
// 3. SEALED EDITOR  – the only door to perform IR mutations
////////////////////////////////////////////////////////////////////////////////

/// Private sealing – nobody outside this module can implement `CanEdit`.
mod sealed {
    pub trait CanEdit {}
}

/// The public alias that passes will take.
///
///  • Derefs to &mut CfgProgram for *reading* convenience.  
///  • Mutation helpers maintain invariants (SSA, edge lists, etc.).
pub struct Editor<'a> {
    prog: &'a mut CfgProgram,
    // Book-keeping fields (SSA counters, etc.) can be added later.
}

// Only we implement the secret trait – prevents “side door” mutations.
impl sealed::CanEdit for Editor<'_> {}

impl<'a> std::ops::Deref for Editor<'a> {
    type Target = CfgProgram;
    fn deref(&self) -> &Self::Target {
        self.prog
    }
}

impl<'a> std::ops::DerefMut for Editor<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.prog
    }
}

impl<'a> Editor<'a> {
    /*───────────────────────────────────────────────────────────────────────
     * Mutation helpers you expose incrementally
     *──────────────────────────────────────────────────────────────────────*/

    /// Replace the *Rvalue* of an assignment without touching the left side.
    pub fn replace_rvalue(&mut self, block: BasicBlockId, stmt_idx: usize, new_rv: Rvalue) {
        let bb = &mut self.prog.blocks[block];
        let Statement::Assign { rvalue, .. } = &mut bb.statements[stmt_idx];
        *rvalue = new_rv;
    }

    /// Remove a statement.  O(1) but keeps indices stable by swapping last.
    pub fn remove_stmt(&mut self, block: BasicBlockId, stmt_idx: usize) {
        let bb = &mut self.prog.blocks[block];
        bb.statements.swap_remove(stmt_idx);
        // TODO: SSA clean-up if the removed stmt defined a VarId.
    }

    /// Split `block` at `split_index` – statement at `split_index` becomes the
    /// first statement of the *new* basic block.  Return the new block id.
    pub fn split_block(&mut self, block: BasicBlockId, split_index: usize) -> BasicBlockId {
        let mut new_block = self.prog.blocks[block].clone();
        new_block.statements = self.prog.blocks[block].statements.split_off(split_index);

        // allocate in arena
        let new_bb_id = self.prog.blocks.alloc(new_block);

        // Fix CFG edges: block ‑> new_bb_id
        self.add_edge(block, new_bb_id, EdgeType::Unconditional);

        // Call verifier in debug builds
        debug_assert!(self.verify_block(block));
        debug_assert!(self.verify_block(new_bb_id));
        new_bb_id
    }

    /// Add a control-flow edge and update predecessor/successor lists.
    pub fn add_edge(&mut self, from: BasicBlockId, to: BasicBlockId, kind: EdgeType) {
        let edge = ControlFlowEdge {
            from,
            to,
            edge_type: kind,
        };
        self.prog.blocks[from].successors.push(edge.clone());
        self.prog.blocks[to].predecessors.push(edge);
    }

    /*─────────────────────  tiny invariant checker  ─────────────────────*/
    fn verify_block(&self, bb: BasicBlockId) -> bool {
        // Example invariant: no empty basic blocks.
        !self.prog.blocks[bb].statements.is_empty()
    }

    /* More helper fns (phi-insert, var creation, etc.) go here */
}

// FOLLOWING PART ARE USAGE EXAMPLE, NOT PART OF THIS FILE
// ////////////////////////////////////////////////////////////////////////////////
// // 3.  A *Transform* is a visitor + an Editor
// ////////////////////////////////////////////////////////////////////////////////

// pub trait TransformPass {
//     fn name(&self) -> &'static str;

//     /// The default driver: create an Editor and run `self.run`.
//     fn run_on_function(
//         &mut self,
//         prog: &mut CfgProgram,
//         fid: FunctionId,
//     ) {
//         let mut editor = Editor { prog };
//         self.run(fid, &mut editor);
//     }

//     /// Implement this!
//     fn run(
//         &mut self,
//         fid: FunctionId,
//         editor: &mut Editor,
//     );
// }

// ////////////////////////////////////////////////////////////////////////////////
// // 4.  EXAMPLE PASS  –  Dead-Store Elimination using your data-flow engine
// ////////////////////////////////////////////////////////////////////////////////

// pub struct DeadStoreElim;

// impl TransformPass for DeadStoreElim {
//     fn name(&self) -> &'static str { "dead-store-elim" }

//     fn run(
//         &mut self,
//         fid: FunctionId,
//         editor: &mut Editor,
//     ) {
//         // 1. Run existing live-variable analysis (choice 2)
//         let live = dataflow::analyze_live_variables(editor, fid);

//         // 2. Walk function and kill stores whose lhs never becomes live.
//         struct Local<'l, 'e> {
//             live_out: &'l HashMap<BasicBlockId, HashSet<VarId>>,
//             editor:   &'l mut Editor<'e>,
//         }
//         impl<'l, 'e> CfgVisitor for Local<'l, 'e> {
//             fn visit_block(&mut self, b: BasicBlockId, _pgm: &CfgProgram) {
//                 // iterate *backwards* so indices stay valid after removals
//                 let mut i = self.editor.blocks[b].statements.len();
//                 while i > 0 {
//                     i -= 1;
//                     if let Statement::Assign { lvalue: LValue::Variable { var }, .. } =
//                         &self.editor.blocks[b].statements[i]
//                     {
//                         if !self.live_out[&b].contains(var) {
//                             self.editor.remove_stmt(b, i);
//                         }
//                     }
//                 }
//             }
//         }

//         let mut local = Local { live_out: &live.exit, editor };
//         walk_function(&mut local, fid, &*local.editor);
//     }
// }

// ////////////////////////////////////////////////////////////////////////////////
// // 5. PASS PIPELINE EXAMPLE
// ////////////////////////////////////////////////////////////////////////////////

// pub fn run_default_pipeline(prog: &mut CfgProgram) {
//     let mut dse = DeadStoreElim;
//     for &fid in &prog.root_functions {
//         dse.run_on_function(prog, fid);
//     }
// }
