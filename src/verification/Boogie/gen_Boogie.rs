use super::{
    BoogieBinOp, BoogieExpr, BoogieExprKind, BoogieProcedure, BoogieProgram, BoogieType,
    BoogieUnOp, BoogieVarDecl,
};
use crate::cfg::{self, BinaryOp, ConstantValue, Operand, Program as CfgProgram, UnaryOp};
use crate::verification::errors::Results;
use std::collections::HashMap;
/// Helper functions for generating Boogie programs from CFG programs
/// Focus on building blocks for Boogie generation with prefix support
#[derive(Debug, Clone)]
pub struct BoogieProgramGenerator {
    pub program: BoogieProgram,
    pub current_procedure_index: Option<usize>, // index of current procedure being worked on
}

impl BoogieProgramGenerator {
    /// Create a new generator from a CFG program
    pub fn from_cfg(name: String, cfg_program: &CfgProgram) -> Self {
        let mut generator = BoogieProgramGenerator {
            program: BoogieProgram {
                name,
                global_vars: HashMap::new(),
                other_declarations: Vec::new(),
                global_string_literals: HashMap::new(),
                procedures: Vec::new(),
            },
            current_procedure_index: None,
        };
        generator.gen_string_axioms();
        generator.gen_list_axioms();
        generator.gen_uuid_axioms();
        generator.gen_iterator_axioms();
        generator.gen_global_constants(cfg_program);
        generator.gen_iterator_function_declarations(cfg_program);
        generator.gen_table_variables(cfg_program);
        generator.gen_row_axioms(cfg_program);
        generator
    }

    /// Set the current procedure being worked on by index
    pub fn set_current_procedure(&mut self, procedure_index: usize) {
        self.current_procedure_index = Some(procedure_index);
    }

    /// Clear the current procedure reference
    pub fn clear_current_procedure(&mut self) {
        self.current_procedure_index = None;
    }

    /// Get a reference to the current procedure being worked on
    pub fn get_current_procedure(&self) -> Option<&BoogieProcedure> {
        self.current_procedure_index
            .and_then(|i| self.program.procedures.get(i))
    }

    /// Get a mutable reference to the current procedure being worked on
    pub fn get_current_procedure_mut(&mut self) -> Option<&mut BoogieProcedure> {
        self.current_procedure_index
            .and_then(move |i| self.program.procedures.get_mut(i))
    }

    /// Generate string axioms and add to other_declarations
    pub fn gen_string_axioms(&mut self) {
        let mut decls = Vec::new();
        // Type and operators
        decls.push("type String;".to_string());
        decls.push("const empty: String;".to_string());
        decls.push("function concat(x: String, y: String): String;".to_string());
        // Generic to-string for any type T
        decls.push("function str<a>(x: a): String;".to_string());

        // Axioms (with correct triggers)
        decls.push(
            "axiom (forall s: String :: {concat(empty, s)} concat(empty, s) == s);".to_string(),
        );
        decls.push(
            "axiom (forall s: String :: {concat(s, empty)} concat(s, empty) == s);".to_string(),
        );
        // Injectivity of str<T> (for each instantiation of T)
        decls.push(
            "axiom (forall<a> x: a, y: a :: {str(x), str(y)} str(x) == str(y) ==> x == y);"
                .to_string(),
        );
        // str<T>(x) is never the empty string (uniform for all T)
        decls.push("axiom (forall<a> x: a :: {str(x)} str(x) != empty);".to_string());

        self.program.other_declarations.extend(decls);
    }

    /// Generate polymorphic list type and axioms similar to list.bpl
    pub fn gen_list_axioms(&mut self) {
        let mut decls = Vec::new();
        // Type
        decls.push("type List a;".to_string());
        // Core operations
        decls.push("function length<a>(List a) returns (int);".to_string());
        decls.push("function get<a>(List a, int) returns (a);".to_string());
        decls.push("function append<a>(List a, a) returns (List a);".to_string());
        // The emptyList function takes a witness argument of type 'a'
        decls.push("function emptyList<a>(a) returns (List a);".to_string());

        // Axioms
        // Axiom 1: All empty lists of the same type 'a' are equal
        decls.push("axiom (forall<a> w1:a, w2:a :: emptyList(w1) == emptyList(w2));".to_string());
        // Axiom 2: The length of an empty list is 0
        decls.push("axiom (forall<a> w:a :: length(emptyList(w)) == 0);".to_string());
        // Axiom 3: Appending an element increases the length by 1
        decls.push(
            "axiom (forall<a> l:List a, e:a :: length(append(l, e)) == length(l) + 1);".to_string(),
        );
        // Axiom 4: Getting the last element after an append returns the appended element
        decls.push(
            "axiom (forall<a> l:List a, e:a :: get(append(l, e), length(l)) == e);".to_string(),
        );
        // Axiom 5: Accessing other elements remains the same after an append
        decls.push("axiom (forall<a> l:List a, e:a, i:int :: 0 <= i && i < length(l) ==> get(append(l, e), i) == get(l, i));".to_string());
        // Axiom 6: Length is never negative
        decls.push("axiom (forall<a> l:List a :: length(l) >= 0);".to_string());
        // Axiom 7: Inequality of lists is determined by their lengths.
        decls.push(
            "axiom (forall<a> l1:List a, l2:List a :: (length(l1) != length(l2) ==> l1 != l2));"
                .to_string(),
        );
        // Axiom 9: Appending to equal lists yields equal lists.
        decls.push("axiom (forall<a> l1:List a, l2:List a, e:a, f:a :: (e == f && l1 == l2) ==> append(l1, e) == append(l2, f));".to_string());

        self.program.other_declarations.extend(decls);
    }

    /// Generate UUID type and axioms based on UUID.bpl
    pub fn gen_uuid_axioms(&mut self) {
        let mut decls = Vec::new();

        // UUID type
        decls.push("type UUID;".to_string());

        // Pure generator keyed by a pair of integers
        decls.push("function genUUID(n: int, m: int): UUID;".to_string());

        // Injectivity axioms:
        // 1) Equality of outputs implies equality of both indices
        decls.push(
            "axiom (forall n1: int, m1: int, n2: int, m2: int :: \
             {genUUID(n1, m1), genUUID(n2, m2)} \
             genUUID(n1, m1) == genUUID(n2, m2) ==> (n1 == n2 && m1 == m2));"
                .to_string(),
        );

        // 2) Distinctness of indices implies distinct outputs
        decls.push(
            "axiom (forall n1: int, m1: int, n2: int, m2: int :: \
             {genUUID(n1, m1), genUUID(n2, m2)} \
             (n1 != n2 || m1 != m2) ==> genUUID(n1, m1) != genUUID(n2, m2));"
                .to_string(),
        );

        self.program.other_declarations.extend(decls);
    }

    /// Generate iterator type and axioms for deterministic iteration
    pub fn gen_iterator_axioms(&mut self) {
        let mut decls = Vec::new();

        // Iterator type
        decls.push("type Iterator a;".to_string());

        // Internal model functions
        decls.push("function iter_position<T>(iter: Iterator T) returns (int);".to_string());
        decls.push("function iter_length<T>(iter: Iterator T) returns (int);".to_string());
        decls.push("function iter_n<T>(iter: Iterator T) returns (int);".to_string());
        decls.push("function iter_m<T>(iter: Iterator T) returns (int);".to_string());

        // Model functions for deterministic behavior
        decls.push("function model_iter_length(n: int, m: int) returns (int);".to_string());

        // Public intrinsic functions
        decls.push(
            "function scan<T>(t: Table T, n: int, m: int) returns (Iterator (Table T));"
                .to_string(),
        );
        decls.push("function next<T>(iter: Iterator T) returns (Iterator T);".to_string());
        decls.push("function hasNext<T>(iter: Iterator T) returns (bool);".to_string());

        // Axioms for deterministic length
        decls.push("axiom (forall n: int, m: int :: model_iter_length(n, m) >= 0);".to_string());
        decls.push("axiom (forall n: int, m: int :: model_iter_length(n, m) == 1);".to_string());

        // scan() axioms
        decls.push(
            "axiom (forall<T> t: Table T, n: int, m: int :: iter_position(scan(t, n, m)) == 0);"
                .to_string(),
        );
        decls.push(
            "axiom (forall<T> t: Table T, n: int, m: int :: iter_n(scan(t, n, m)) == n);"
                .to_string(),
        );
        decls.push(
            "axiom (forall<T> t: Table T, n: int, m: int :: iter_m(scan(t, n, m)) == m);"
                .to_string(),
        );
        decls.push("axiom (forall<T> t: Table T, n: int, m: int :: iter_length(scan(t, n, m)) == model_iter_length(n, m));".to_string());

        // next() axioms
        decls.push("axiom (forall<T> iter: Iterator T :: iter_position(next(iter)) == iter_position(iter) + 1);".to_string());
        decls.push(
            "axiom (forall<T> iter: Iterator T :: iter_length(next(iter)) == iter_length(iter));"
                .to_string(),
        );
        decls.push(
            "axiom (forall<T> iter: Iterator T :: iter_n(next(iter)) == iter_n(iter));".to_string(),
        );
        decls.push(
            "axiom (forall<T> iter: Iterator T :: iter_m(next(iter)) == iter_m(iter));".to_string(),
        );

        // hasNext() axiom
        decls.push("axiom (forall<T> iter: Iterator T :: hasNext(iter) <==> iter_position(iter) < iter_length(iter));".to_string());

        self.program.other_declarations.extend(decls);
    }

    /// Generate function declarations for iterator accessor functions
    /// Functions with @iterator decorator get special treatment following iterator.bpl pattern
    fn gen_iterator_function_declarations(&mut self, cfg_program: &CfgProgram) {
        for (func_id, func_decl) in &cfg_program.functions {
            // Check if this function has @iterator decorator
            let has_iterator = func_decl.decorators.iter().any(|d| d.name == "iterator");

            if !has_iterator {
                continue;
            }

            // Check if function also has @rename decorator
            let has_rename = func_decl.decorators.iter().any(|d| d.name == "rename");

            // Use base name or renamed name based on @rename decorator
            let func_name = if has_rename {
                format!("{}#{}", func_decl.name, func_id.index())
            } else {
                func_decl.name.clone()
            };

            // Build parameter types (WITHOUT simplifying Iterator<Table<T>>)
            let param_types: Vec<BoogieType> = func_decl
                .params
                .iter()
                .map(|param_id| {
                    let var = &cfg_program.variables[*param_id];
                    // Use convert_type directly to keep Iterator<Table<T>> structure
                    let ty = &cfg_program.types[var.ty];
                    Self::convert_type(cfg_program, ty)
                })
                .collect();

            // Get return type from function signature
            let return_type = match &cfg_program.types[func_decl.signature.ty] {
                cfg::Type::Function { return_type, .. } => {
                    Self::convert_type_id(cfg_program, return_type)
                }
                _ => Self::convert_type_id(cfg_program, &func_decl.signature.ty),
            };

            // Generate function declaration: function get_FieldName(iter: Iterator (Table TableName)) returns (Type);
            let mut func_decl_str = format!("function {}", func_name);

            // Add parameters
            func_decl_str.push('(');
            for (i, param_type) in param_types.iter().enumerate() {
                if i > 0 {
                    func_decl_str.push_str(", ");
                }
                // Display will format Iterator types as "Iterator (Table T)" automatically
                func_decl_str.push_str(&format!("arg{}: {}", i, param_type));
            }
            func_decl_str.push(')');

            // Add return type
            func_decl_str.push_str(&format!(" returns ({})", return_type));
            func_decl_str.push(';');

            self.program.other_declarations.push(func_decl_str);

            // Generate model function and axiom for get_* functions
            if func_decl.name.starts_with("get_") {
                // Model function name should include the unique function ID suffix
                let model_func_name = if has_rename {
                    format!("model_{}#{}", func_decl.name, func_id.index())
                } else {
                    format!("model_{}", func_decl.name)
                };

                // Declare model function with unique name (no need to check for duplicates since names are unique)
                let model_func_decl = format!(
                    "function {}(n: int, m: int, index: int) returns ({});",
                    model_func_name, return_type
                );
                self.program.other_declarations.push(model_func_decl);

                // Extract the table type for the axiom - keep the full Iterator (Table T) type to match function signature
                let table_type_str = if !param_types.is_empty() {
                    // Use the full param_types[0] which is Iterator<Table<T>> - Display will format it as "Iterator (Table T)"
                    format!("{}", param_types[0])
                } else {
                    "T".to_string()
                };

                // Generate axiom: axiom (forall iter: Iterator (Table Activity) :: hasNext(iter) ==> get_AID#N(iter) == model_get_AID#N(...));
                let axiom = format!(
                    "axiom (forall iter: {} :: hasNext(iter) ==> {}(iter) == {}(iter_n(iter), iter_m(iter), iter_position(iter)));",
                    table_type_str, func_name, model_func_name
                );
                self.program.other_declarations.push(axiom);
            }
        }
    }

    /// Helper function to generate a global constant name for a string literal
    /// For string literal "abc", generates "L_abc"
    /// Uses $ prefix for unicode escapes to ensure uniqueness
    fn gen_string_literal_name(literal: &str) -> String {
        // Sanitize the string for use as identifier
        let sanitized: String = literal
            .chars()
            .map(|c| match c {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => c.to_string(),
                _ => format!("$u{:04X}", c as u32), // Use $uXXXX for unicode escapes
            })
            .collect();

        format!("L_{}", sanitized)
    }

    /// Add a string literal to the global string literals map
    pub fn add_string_literal(&mut self, literal: &str) -> String {
        if literal.is_empty() {
            return "empty".to_string();
        }

        let const_name = Self::gen_string_literal_name(literal);

        if !self
            .program
            .global_string_literals
            .contains_key(&const_name)
        {
            let var_decl = BoogieVarDecl {
                var_name: const_name.clone(),
                var_type: BoogieType::Parametric {
                    name: "String".to_string(),
                    args: vec![],
                },
                is_const: true,
            };
            self.program
                .global_string_literals
                .insert(const_name.clone(), var_decl);
        }

        const_name
    }

    /// Generate global constants from CFG program
    fn gen_global_constants(&mut self, cfg_program: &CfgProgram) {
        for (_, constant) in &cfg_program.global_consts {
            let var_name = constant.name.clone();
            let var_type = BoogieProgramGenerator::convert_type_id(cfg_program, &constant.ty);
            let decl = BoogieVarDecl {
                var_name: var_name.clone(),
                var_type,
                is_const: true,
            };
            self.program.global_vars.insert(var_name, decl);
        }
    }

    /// Generate global table variables from CFG program
    fn gen_table_variables(&mut self, cfg_program: &CfgProgram) {
        // Generate Table type declaration
        self.program
            .other_declarations
            .push("type Table a;".to_string());

        // Declare each table type (e.g., type Wall;)
        for (_, table) in &cfg_program.tables {
            self.program
                .other_declarations
                .push(format!("type {};", table.name));
        }

        for (_, table) in &cfg_program.tables {
            // Generate const TBL_TableName: Table TableName; for each table
            let table_const_name = format!("TBL_{}", table.name);
            let table_type = BoogieType::Parametric {
                name: "Table".to_string(),
                args: vec![BoogieType::Parametric {
                    name: table.name.clone(),
                    args: vec![],
                }],
            };
            let table_const_decl = BoogieVarDecl {
                var_name: table_const_name.clone(),
                var_type: table_type,
                is_const: true,
            };
            self.program
                .global_vars
                .insert(table_const_name, table_const_decl);

            // Generate field variables as before
            for field_id in table
                .primary_key_fields
                .iter()
                .chain(table.other_fields.iter())
            {
                let field = &cfg_program.table_fields[*field_id];
                let var_name =
                    BoogieProgramGenerator::gen_table_field_var_name(&table.name, &field.name);
                let var_type = BoogieProgramGenerator::gen_table_field_type(cfg_program, *field_id);
                let decl = BoogieVarDecl {
                    var_name: var_name.clone(),
                    var_type,
                    is_const: false,
                };
                self.program.global_vars.insert(var_name, decl);
            }
        }
    }

    /// Generate Row type and row constructor axioms for each table
    /// Following the pattern from row.bpl:
    /// 1. Declare Row type (already exists)
    /// 2. For each table, create a constructor function taking all fields
    /// 3. Create equality axiom: rows are equal iff all fields are equal
    pub fn gen_row_axioms(&mut self, cfg_program: &CfgProgram) {
        // Row type declaration
        self.program
            .other_declarations
            .push("type Row a;".to_string());

        // For each table, generate constructor and axioms
        for (_, table) in &cfg_program.tables {
            let table_name = &table.name;

            // Collect all fields (primary keys + other fields) in order
            let all_field_ids: Vec<_> = table
                .primary_key_fields
                .iter()
                .chain(table.other_fields.iter())
                .copied()
                .collect();

            // Build constructor function declaration
            // function construct_Row_TableName(field1: Type1, field2: Type2, ...): Row (Table TableName);
            let mut constructor_decl = format!("function construct_Row_{}", table_name);
            constructor_decl.push('(');

            let mut field_params = Vec::new();
            for field_id in &all_field_ids {
                let field = &cfg_program.table_fields[*field_id];
                let field_type = Self::convert_type_id(cfg_program, &field.field_type);
                field_params.push(format!("{}: {}", field.name, field_type));
            }
            constructor_decl.push_str(&field_params.join(", "));
            constructor_decl.push_str(&format!("): Row (Table {});", table_name));

            self.program.other_declarations.push(constructor_decl);

            // Build equality axiom
            // axiom (forall field1_1: Type1, field2_1: Type2, ..., field1_2: Type1, field2_2: Type2, ... ::
            //     construct_Row_TableName(field1_1, field2_1, ...) == construct_Row_TableName(field1_2, field2_2, ...)
            //     <==> (field1_1 == field1_2 && field2_1 == field2_2 && ...)
            // );

            let mut axiom = String::from("axiom (forall\n    ");

            // Generate quantified variables (two sets: _1 and _2 suffixes)
            let mut vars_1 = Vec::new();
            let mut vars_2 = Vec::new();
            for field_id in &all_field_ids {
                let field = &cfg_program.table_fields[*field_id];
                let field_type = Self::convert_type_id(cfg_program, &field.field_type);
                vars_1.push(format!("{}_1: {}", field.name, field_type));
                vars_2.push(format!("{}_2: {}", field.name, field_type));
            }

            axiom.push_str(&vars_1.join(", "));
            axiom.push_str(",\n    ");
            axiom.push_str(&vars_2.join(", "));
            axiom.push_str("\n    ::\n    ");

            // Left side of equivalence
            axiom.push_str(&format!("construct_Row_{}(", table_name));
            let field_names_1: Vec<_> = all_field_ids
                .iter()
                .map(|fid| format!("{}_1", cfg_program.table_fields[*fid].name))
                .collect();
            axiom.push_str(&field_names_1.join(", "));
            axiom.push_str(") == ");

            axiom.push_str(&format!("construct_Row_{}(", table_name));
            let field_names_2: Vec<_> = all_field_ids
                .iter()
                .map(|fid| format!("{}_2", cfg_program.table_fields[*fid].name))
                .collect();
            axiom.push_str(&field_names_2.join(", "));
            axiom.push_str(")\n    <==>\n    (");

            // Right side of equivalence: conjunction of field equalities
            let field_equalities: Vec<_> = all_field_ids
                .iter()
                .map(|fid| {
                    let fname = &cfg_program.table_fields[*fid].name;
                    format!("{}_1 == {}_2", fname, fname)
                })
                .collect();
            axiom.push_str(&field_equalities.join(" && "));
            axiom.push_str(")\n);");

            self.program.other_declarations.push(axiom);
        }
    }

    pub fn convert_type_id(program: &CfgProgram, type_id: &cfg::TypeId) -> BoogieType {
        let ty = &program.types[*type_id];
        let boogie_ty = Self::convert_type(program, ty);
        // Simplify Iterator<Table<T>> to Iterator<T>
        boogie_ty
    }

    /// Convert CFG TypeName to BoogieType
    pub fn convert_type(program: &CfgProgram, ty: &cfg::Type) -> BoogieType {
        match ty {
            cfg::Type::Primitive(p) => match p {
                cfg::PrimitiveType::Int => BoogieType::Int,
                cfg::PrimitiveType::Float => BoogieType::Real,
                cfg::PrimitiveType::Bool => BoogieType::Bool,
                cfg::PrimitiveType::String => BoogieType::Parametric {
                    name: "String".to_string(),
                    args: vec![],
                },
            },
            cfg::Type::List(elem_ty) => {
                let boogie_base_ty = Self::convert_type_id(program, elem_ty);
                // Represent list as a parametric List type rather than a map
                BoogieType::Parametric {
                    name: "List".to_string(),
                    args: vec![boogie_base_ty],
                }
            }
            cfg::Type::Declared { type_id, args } => {
                let udt = &program.user_defined_types[*type_id];
                let boogie_args: Vec<BoogieType> = args
                    .iter()
                    .map(|arg_type_id| Self::convert_type_id(program, arg_type_id))
                    .collect();
                BoogieType::Parametric {
                    name: udt.name.clone(),
                    args: boogie_args,
                }
            }
            cfg::Type::Table(table_id) => {
                let table = &program.tables[*table_id];
                BoogieType::Parametric {
                    name: "Table".to_string(),
                    args: vec![BoogieType::Parametric {
                        name: table.name.clone(),
                        args: vec![],
                    }],
                }
            }
            cfg::Type::Row { table_id } => {
                let table = &program.tables[*table_id];
                // Represent a row as Row (Table TableName) to match constructor signature
                BoogieType::Parametric {
                    name: "Row".to_string(),
                    args: vec![BoogieType::Parametric {
                        name: "Table".to_string(),
                        args: vec![BoogieType::Parametric {
                            name: table.name.clone(),
                            args: vec![],
                        }],
                    }],
                }
            }
            cfg::Type::GenericParam(p) => {
                let param = &program.generic_params[*p];
                BoogieType::Parametric {
                    name: param.name.clone(),
                    args: vec![],
                }
            }
            cfg::Type::Void => BoogieType::Parametric {
                name: "()".to_string(),
                args: vec![],
            },
            cfg::Type::Function { .. } => {
                // Represent function as an opaque parametric marker
                BoogieType::Parametric {
                    name: "function".to_string(),
                    args: vec![],
                }
            }
        }
    }

    /// Generate table field variable name
    pub fn gen_table_field_var_name(table_name: &str, field_name: &str) -> String {
        format!("{}_{}", table_name, field_name)
    }

    /// Generate Boogie type for a table field (used for both global variables and local snapshots)
    pub fn gen_table_field_type(
        cfg_program: &CfgProgram,
        field_id: crate::cfg::FieldId,
    ) -> BoogieType {
        let field = &cfg_program.table_fields[field_id];
        let table = &cfg_program.tables[field.table_id];
        let key_types: Vec<Box<BoogieType>> = table
            .primary_key_fields
            .iter()
            .map(|f_id| {
                let key_field = &cfg_program.table_fields[*f_id];
                Box::new(Self::convert_type_id(cfg_program, &key_field.field_type))
            })
            .collect();
        let value_type = Box::new(Self::convert_type_id(cfg_program, &field.field_type));
        BoogieType::Map(key_types, value_type)
    }

    /// Helper function to add a local variable to current procedure if it doesn't exist
    /// Returns true if variable was added, false if it already existed
    pub fn ensure_local_variable_exists(&mut self, var_name: &str, var_type: BoogieType) -> bool {
        if let Some(proc) = self.get_current_procedure_mut() {
            if !proc.local_vars.iter().any(|v| v.var_name == var_name)
                && !proc.params.iter().any(|v| v.var_name == var_name)
            {
                proc.local_vars.push(BoogieVarDecl {
                    var_name: var_name.to_string(),
                    var_type,
                    is_const: false,
                });
                return true;
            }
        }
        false
    }

    /// Convert CFG operand to Boogie expression.
    /// Note: This does not handle scoped names. The caller (BaseVerificationGenerator) should provide scoped names.
    pub fn convert_operand(
        &mut self,
        _cfg_program: &CfgProgram,
        operand: &Operand,
        // The name of the variable is passed in directly by the caller
        var_name: String,
    ) -> Results<BoogieExpr> {
        match operand {
            Operand::Variable(_) => Ok(BoogieExpr {
                kind: BoogieExprKind::Var(var_name),
            }),
            Operand::Constant(constant) => self.convert_constant(constant),
            Operand::Global(_) => Ok(BoogieExpr {
                kind: BoogieExprKind::Var(var_name),
            }),
            Operand::Table(_) => Ok(BoogieExpr {
                kind: BoogieExprKind::Var(var_name),
            }),
        }
    }

    /// Convert CFG constant to Boogie expression
    pub fn convert_constant(&mut self, constant: &ConstantValue) -> Results<BoogieExpr> {
        match constant {
            ConstantValue::Int(i) => Ok(BoogieExpr {
                kind: BoogieExprKind::IntConst(*i),
            }),
            ConstantValue::Float(r) => Ok(BoogieExpr {
                kind: BoogieExprKind::RealConst(r.into_inner()),
            }),
            ConstantValue::Bool(b) => Ok(BoogieExpr {
                kind: BoogieExprKind::BoolConst(*b),
            }),
            ConstantValue::String(s) => {
                let const_name = self.add_string_literal(s);
                Ok(BoogieExpr {
                    kind: BoogieExprKind::Var(const_name),
                })
            }
        }
    }

    /// Convert CFG binary operation to Boogie binary operation
    pub fn convert_binary_op(op: &BinaryOp) -> BoogieBinOp {
        match op {
            BinaryOp::AddInt | BinaryOp::AddFloat => BoogieBinOp::Add,
            BinaryOp::SubInt | BinaryOp::SubFloat => BoogieBinOp::Sub,
            BinaryOp::MulInt | BinaryOp::MulFloat => BoogieBinOp::Mul,
            BinaryOp::DivInt | BinaryOp::DivFloat => BoogieBinOp::Div,
            BinaryOp::EqInt | BinaryOp::EqFloat | BinaryOp::Eq => BoogieBinOp::Eq,
            BinaryOp::NeqInt | BinaryOp::NeqFloat | BinaryOp::Neq => BoogieBinOp::Ne,
            BinaryOp::LtInt | BinaryOp::LtFloat => BoogieBinOp::Lt,
            BinaryOp::LeqInt | BinaryOp::LeqFloat => BoogieBinOp::Le,
            BinaryOp::GtInt | BinaryOp::GtFloat => BoogieBinOp::Gt,
            BinaryOp::GeqInt | BinaryOp::GeqFloat => BoogieBinOp::Ge,
            BinaryOp::And => BoogieBinOp::And,
            BinaryOp::Or => BoogieBinOp::Or,
            _ => panic!("Unsupported binary operator"),
        }
    }

    pub fn convert_unary_op(&self, op: &UnaryOp) -> Results<BoogieUnOp> {
        match op {
            UnaryOp::NotBool => Ok(BoogieUnOp::Not),
            UnaryOp::NegInt | UnaryOp::NegFloat => Ok(BoogieUnOp::Neg),
        }
    }
}
