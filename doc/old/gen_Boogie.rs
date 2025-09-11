use super::{
    BoogieProgram, BoogieProcedure, BoogieVarDecl, BoogieType, BoogieLine, BoogieExpr, ErrorMessage, 
    BoogieExprKind, BoogieBinOp, BoogieUnOp
};
use crate::cfg::{
    CfgProgram, BasicBlockId, FunctionId, VarId, TypeName, TableId, FieldId, HopId,
    Variable, TableInfo, FieldInfo, FunctionCfg, HopCfg, BasicBlock, Statement,
    LValue, RValue, Operand, Constant, ControlFlowEdge, EdgeType, FunctionType,
    VariableKind, BinaryOp, UnaryOp
};
use crate::ast::ReturnType;
use std::collections::{HashMap, HashSet};


#[derive(Debug, Clone)]
pub struct BoogieProgramGenerator {
    pub program: BoogieProgram,
}

impl BoogieProgramGenerator {
    pub fn new(program: BoogieProgram) -> Self {
        BoogieProgramGenerator { program }
    }

    pub fn gen_string_axioms(&mut self) {
        // put following stuff to BoogieProgram.other_declarations
        /*
        /*************************************************************************
         *  String model with  +  overloads for int and real
         *************************************************************************/

        // An uninterpreted sort for strings
        type String;

        /* ------------------  core constructors  ------------------ */
        function Concat(s1:String, s2:String) : String;   // s1 + s2
        function IntToString(i:int)            : String;   // "42"
        function RealToString(r:real)          : String;   // "3.14"

        /* ------------------  derived “+” operators --------------- */
        //  s + i
        function StrAddInt  (s:String, i:int)  : String;
        //  i + s
        function IntAddStr  (i:int, s:String)  : String;
        //  s + r
        function StrAddReal (s:String, r:real) : String;
        //  r + s
        function RealAddStr (r:real, s:String) : String;

        /* ------------------  axioms tying everything together ---- */

        // 1.  “+” is just Concat after coercion
        axiom (forall s:String, i:int  :: StrAddInt(s,i)  == Concat(s, IntToString(i)));
        axiom (forall i:int,  s:String:: IntAddStr(i,s)  == Concat(IntToString(i), s));

        axiom (forall s:String, r:real :: StrAddReal(s,r) == Concat(s, RealToString(r)));
        axiom (forall r:real, s:String:: RealAddStr(r,s) == Concat(RealToString(r), s));

        axiom (forall s1,s2,s3:String ::
                Concat(Concat(s1,s2),s3) == Concat(s1,Concat(s2,s3)));          // associativity

        // Optional identity element (empty string)
        const Empty : String;
        axiom (forall s:String :: Concat(Empty,s) == s && Concat(s,Empty) == s);
         */
    }

    pub fn gen_global_const(&mut self) {
        // put CfgProgram.root_variables to BoogieProgram.global_vars, they are all constants
    }

    pub fn gen_table(&mut self) {
        // put CfgProgram.root_tables to BoogieProgram.global_vars
        // For table Data{ primary int ID; primary int B_ID; int VALUE; float PRICE;}
        // generate map: var Data_B_ID:  [int][int]int;
        // and           var Data_VALUE: [int][int]real;
        // Please use gen_table_field_var_name;
    }

    pub fn gen_type(ty: TypeName) {
        match ty {
            TypeName::Int => BoogieType::Int,
            TypeName::Float => BoogieType::Real,
            TypeName::Bool => BoogieType::Bool,
            TypeName::String => BoogieType::UserDefined("String".to_string()),
            TypeName::Table(s) => {
                assert!(false, "Table type should not be used directly in Boogie");
                BoogieType::UserDefined(s.clone())
            },
            TypeName::Array { element_type, _, _ } => todo!() //let it be maps, all the dimention should be integer.
        }
    }

    pub fn gen_table_field_var_name(tableinfo, fieldinfo) -> String {
        todo!() // For now, just Data_VALUE
    }



}

