use super::util::{go_type_string, go_var_name};
use crate::cfg;
use std::collections::{hash_map::Entry, HashMap, HashSet};
use std::fmt::Write;

pub(super) struct GoStateMachine<'p> {
    program: &'p cfg::Program,
    var_decls: HashMap<cfg::VariableId, String>,
    pre_statements: Vec<String>,
    cases: Vec<(usize, String)>,
    skip_decls: HashSet<cfg::VariableId>,
    default_handler: String,
}

impl<'p> GoStateMachine<'p> {
    pub(super) fn new(program: &'p cfg::Program) -> Self {
        Self {
            program,
            var_decls: HashMap::new(),
            pre_statements: Vec::new(),
            cases: Vec::new(),
            skip_decls: HashSet::new(),
            default_handler: "panic(\"invalid state\")".to_string(),
        }
    }

    pub(super) fn skip_declaration(&mut self, var_id: cfg::VariableId) {
        self.skip_decls.insert(var_id);
    }

    pub(super) fn ensure_var_decl(&mut self, var_id: cfg::VariableId, ty_override: Option<String>) {
        if self.skip_decls.contains(&var_id) {
            return;
        }

        let default_ty = || go_type_string(self.program, self.program.variables[var_id].ty);
        let (go_type, override_applied) = match ty_override {
            Some(ty) => (ty, true),
            None => (default_ty(), false),
        };

        match self.var_decls.entry(var_id) {
            Entry::Vacant(entry) => {
                entry.insert(go_type);
            }
            Entry::Occupied(mut entry) => {
                if override_applied && entry.get() != &go_type {
                    entry.insert(go_type);
                }
            }
        }
    }

    pub(super) fn add_pre_stmt(&mut self, stmt: impl Into<String>) {
        self.pre_statements.push(stmt.into());
    }

    pub(super) fn add_case(&mut self, state: usize, body: String) {
        self.cases.push((state, body));
    }

    #[allow(dead_code)]
    pub(super) fn set_default_handler(&mut self, handler: impl Into<String>) {
        self.default_handler = handler.into();
    }

    pub(super) fn render(
        &self,
        out: &mut String,
        entry_state: usize,
    ) -> Result<(), std::fmt::Error> {
        if !self.var_decls.is_empty() {
            let mut decls: Vec<_> = self.var_decls.iter().collect();
            decls.sort_by_key(|(var_id, _)| var_id.index());
            for (var_id, ty) in decls {
                let var_name = go_var_name(self.program, *var_id);
                writeln!(out, "\tvar {} {}", var_name, ty)?;
            }
            writeln!(out)?;
        }

        if !self.pre_statements.is_empty() {
            for stmt in &self.pre_statements {
                writeln!(out, "{}", stmt)?;
            }
            writeln!(out)?;
        }

        writeln!(out, "\tcurrentState := {}", entry_state)?;
        writeln!(out, "\tfor {{")?;
        writeln!(out, "\t\tswitch currentState {{")?;

        let mut cases: Vec<_> = self.cases.iter().collect();
        cases.sort_by_key(|(state, _)| *state);
        for (state, body) in cases {
            writeln!(out, "\t\tcase {}:", state)?;
            write!(out, "{}", body)?;
        }

        if !self.default_handler.is_empty() {
            writeln!(out, "\t\tdefault:")?;
            writeln!(out, "\t\t\t{}", self.default_handler)?;
        }

        writeln!(out, "\t\t}}")?;
        writeln!(out, "\t}}")?;
        writeln!(out)?;
        Ok(())
    }
}
