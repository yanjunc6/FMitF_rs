use std::rc::Rc;
use std::collections::HashSet;
use crate::ast::*;
use crate::graph::*;

pub mod boogie;
pub mod build_unit;
pub mod auto_verifier;

pub use boogie::generate_verification_boogie_code;
pub use build_unit::build_verification_unit;
pub use auto_verifier::AutoVerifier;

#[derive(Debug, Clone)]
pub struct VerificationUnit {
    // Node under consideration
    pub node: Rc<NodeDef>,

    // Function 1 and its relevant hops (in order up to and including the conflict)
    pub func1: Rc<FunctionDeclaration>,
    pub hops1: Vec<Rc<HopBlock>>, // f1..=f3

    // Function 2 and its relevant hops (g1..=g2)
    pub func2: Rc<FunctionDeclaration>,
    pub hops2: Vec<Rc<HopBlock>>,

    // Index of last relevant hop in each function (for traceability)
    pub idx1: usize,
    pub idx2: usize,

    // Optionally, any tables that may be read/written by these hops
    pub relevant_tables: HashSet<Rc<TableDeclaration>>,
}