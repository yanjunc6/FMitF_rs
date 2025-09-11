use crate::ast_old::ast_builder::AstBuilder;
use crate::ast_old::*;
use id_arena::Arena;

// ============================================================================
// --- Default Program
// ============================================================================

impl Program {
    fn new() -> Self {
        // Create empty program
        Program {
            declarations: Vec::new(),
            functions: Arena::new(),
            type_decls: Arena::new(),
            const_decls: Arena::new(),
            table_decls: Arena::new(),
            var_decls: Arena::new(),
            params: Arena::new(),
            generic_params: Arena::new(),
            types: Arena::new(),
            statements: Arena::new(),
            expressions: Arena::new(),
            blocks: Arena::new(),
        }
    }
}
