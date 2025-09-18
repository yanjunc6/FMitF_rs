use crate::ast::visit::Visitor;
use crate::ast::*;
use crate::pretty::PrettyPrint;
use std::io::{self, Write};

// Debug options - simple constants for development
const SHOW_IDS: bool = true;
const SHOW_NAME_RESOLUTION: bool = true;
const SHOW_TYPE_RESOLUTION: bool = true;

/// A pretty printer that uses the visitor pattern to traverse and format AST nodes.
pub struct AstPrinter<W: Write> {
    writer: W,
    indent_level: usize,
}

impl<W: Write> AstPrinter<W> {
    pub fn new(writer: W) -> Self {
        Self {
            writer,
            indent_level: 0,
        }
    }

    fn write_indent(&mut self) -> io::Result<()> {
        for _ in 0..self.indent_level {
            write!(self.writer, "  ")?;
        }
        Ok(())
    }

    fn indent(&mut self) {
        self.indent_level += 1;
    }

    fn dedent(&mut self) {
        if self.indent_level > 0 {
            self.indent_level -= 1;
        }
    }

    pub fn print_program(&mut self, program: &Program) -> io::Result<()> {
        self.visit_program(program).map_err(|e| e)
    }

    /// Print unified ID and type information for any declaration
    fn print_declaration_info(&mut self, id: usize, node_type: &str) -> io::Result<()> {
        if SHOW_IDS {
            write!(self.writer, "[{}{}]", node_type, id)?;
        }
        Ok(())
    }

    /// Print function type signature with bounded type variables
    fn print_type_scheme(&mut self, prog: &Program, scheme: &TypeScheme) -> io::Result<()> {
        // Print bounded type variables if any
        if !scheme.quantified_params.is_empty() {
            for (i, param_id) in scheme.quantified_params.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ", ")?;
                }
                write!(self.writer, "gp{}", param_id.index())?;
            }
            write!(self.writer, " => ")?;
        }

        self.print_type_compact(prog, &scheme.ty)?;

        Ok(())
    }

    /// Print the compact representation of a type
    fn print_resolved_type_info(
        &mut self,
        prog: &Program,
        resolved_type: &Option<ResolvedType>,
    ) -> io::Result<()> {
        if SHOW_TYPE_RESOLUTION {
            match resolved_type {
                Some(ty) => {
                    write!(self.writer, "[")?;
                    self.print_type_compact(prog, ty)?;
                    write!(self.writer, "]")?;
                }
                None => write!(self.writer, "[t?]")?, // t? = type unresolved
            }
        }
        Ok(())
    }

    /// Print the compact representation of a type
    fn print_type_compact(&mut self, prog: &Program, ty: &ResolvedType) -> io::Result<()> {
        match ty {
            ResolvedType::Declared { decl_id, args } => {
                write!(self.writer, "t{}", decl_id.index())?;
                if !args.is_empty() {
                    write!(self.writer, "<")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(self.writer, ", ")?;
                        }
                        self.print_type_compact(prog, arg)?;
                    }
                    write!(self.writer, ">")?;
                }
                Ok(())
            }
            ResolvedType::Table { table_id } => {
                let table_decl = &prog.table_decls[*table_id];
                write!(self.writer, "tab<{}>", table_decl.name.name)
            }
            ResolvedType::InferVar(id) => write!(self.writer, "infer{}", id),
            ResolvedType::GenericParam(id) => {
                write!(self.writer, "gp{}", id.index())
            }
            ResolvedType::Void => write!(self.writer, "void"),
            ResolvedType::Function {
                param_types,
                return_type,
            } => {
                write!(self.writer, "(")?;
                for (i, param_type) in param_types.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.print_type_compact(prog, param_type)?;
                }
                write!(self.writer, ") -> ")?;
                self.print_type_compact(prog, return_type)
            }
        }
    }

    /// Print resolution in compact format
    fn print_resolution_compact(&mut self, res: &IdentifierResolution) -> io::Result<()> {
        match res {
            IdentifierResolution::Var(id) => write!(self.writer, "v{}", id.index())?,
            IdentifierResolution::Param(id) => write!(self.writer, "p{}", id.index())?,
            IdentifierResolution::Const(id) => write!(self.writer, "c{}", id.index())?,
            IdentifierResolution::Function(id) => write!(self.writer, "f{}", id.index())?,
            IdentifierResolution::Type(id) => write!(self.writer, "it{}", id.index())?,
            IdentifierResolution::Table(id) => write!(self.writer, "itab{}", id.index())?,
            IdentifierResolution::GenericParam(id) => write!(self.writer, "igp{}", id.index())?,
        }
        Ok(())
    }

    /// Print AST type with systematic formatting
    fn print_ast_type_with_info(&mut self, prog: &Program, type_id: AstTypeId) -> io::Result<()> {
        let ast_type = &prog.types[type_id];
        match ast_type {
            AstType::Named {
                name,
                resolved_type,
            } => {
                write!(self.writer, "{}", name.name)?;
                if let Some(type_decl_id) = resolved_type {
                    self.print_declaration_info(type_decl_id.index(), "t")?;
                }
            }
            AstType::Generic {
                base,
                args,
                resolved_base_type,
                ..
            } => {
                write!(self.writer, "{}", base.name)?;
                if let Some(type_decl_id) = resolved_base_type {
                    self.print_declaration_info(type_decl_id.index(), "t")?;
                }
                write!(self.writer, "<")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.print_ast_type_with_info(prog, *arg)?;
                }
                write!(self.writer, ">")?;
            }
            AstType::Function {
                params,
                return_type,
                ..
            } => {
                write!(self.writer, "(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.print_ast_type_with_info(prog, *param)?;
                }
                write!(self.writer, ") -> ")?;
                self.print_ast_type_with_info(prog, *return_type)?;
            }
        }
        Ok(())
    }
}

impl<W: Write> Visitor<'_, (), io::Error> for AstPrinter<W> {
    fn visit_program(&mut self, prog: &Program) -> Result<(), io::Error> {
        for item in &prog.declarations {
            self.visit_item(prog, item)?;
            writeln!(self.writer)?;
        }
        Ok(())
    }

    fn visit_callable_decl(
        &mut self,
        prog: &Program,
        id: FunctionId,
        decl: &CallableDecl,
    ) -> Result<(), io::Error> {
        // Print decorators
        for decorator in &decl.decorators {
            self.write_indent()?;
            write!(self.writer, "@{}", decorator.name.name)?;
            writeln!(self.writer)?;
        }

        self.write_indent()?;
        match decl.kind {
            CallableKind::Function => write!(self.writer, "function")?,
            CallableKind::Operator => write!(self.writer, "operator")?,
            CallableKind::Partition => write!(self.writer, "partition")?,
            CallableKind::Transaction => write!(self.writer, "transaction")?,
        }

        write!(self.writer, " {}", decl.name.name)?;
        self.print_declaration_info(id.index(), "f")?;

        // Generic parameters
        if !decl.generic_params.is_empty() {
            write!(self.writer, "<")?;
            for (i, param_id) in decl.generic_params.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ", ")?;
                }
                let param = &prog.generic_params[*param_id];
                write!(self.writer, "{}", param.name.name)?;
                self.print_declaration_info(param_id.index(), "g")?;
            }
            write!(self.writer, ">")?;
        }

        // Parameters
        write!(self.writer, "(")?;
        for (i, param_id) in decl.params.iter().enumerate() {
            if i > 0 {
                write!(self.writer, ", ")?;
            }
            let param = &prog.params[*param_id];
            write!(self.writer, "{}", param.name.name)?;
            self.print_declaration_info(param_id.index(), "p")?;
            write!(self.writer, ": ")?;
            self.print_ast_type_with_info(prog, param.ty)?;
            self.print_resolved_type_info(prog, &param.resolved_type)?;
        }
        write!(self.writer, ")")?;

        // Return type
        if let Some(return_type_id) = decl.return_type {
            write!(self.writer, " -> ")?;
            self.print_ast_type_with_info(prog, return_type_id)?;
            self.print_resolved_type_info(prog, &decl.resolved_return_type)?;
        }

        // Function type (derived from parameters and return type)
        if let Some(scheme) = &decl.resolved_function_type {
            write!(self.writer, " : ")?;
            self.print_type_scheme(prog, scheme)?;
        }

        // Assumptions
        for assumption in &decl.assumptions {
            writeln!(self.writer)?;
            self.write_indent()?;
            write!(self.writer, "requires ")?;
            self.print_expression_inline(prog, *assumption)?;
        }

        // Body
        if let Some(body_id) = decl.body {
            writeln!(self.writer)?;
            self.visit_block(prog, body_id)?;
        } else {
            writeln!(self.writer, ";")?;
        }

        Ok(())
    }

    fn visit_type_decl(
        &mut self,
        prog: &Program,
        id: TypeDeclId,
        decl: &TypeDecl,
    ) -> Result<(), io::Error> {
        // Print decorators
        for decorator in &decl.decorators {
            self.write_indent()?;
            write!(self.writer, "@{}", decorator.name.name)?;
            writeln!(self.writer)?;
        }

        self.write_indent()?;
        write!(self.writer, "type {}", decl.name.name)?;
        self.print_declaration_info(id.index(), "t")?;

        // Generic parameters
        if !decl.generic_params.is_empty() {
            write!(self.writer, "<")?;
            for (i, param_id) in decl.generic_params.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ", ")?;
                }
                let param = &prog.generic_params[*param_id];
                write!(self.writer, "{}", param.name.name)?;
                self.print_declaration_info(param_id.index(), "g")?;
            }
            write!(self.writer, ">")?;
        }

        writeln!(self.writer, ";")?;
        Ok(())
    }

    fn visit_const_decl(
        &mut self,
        prog: &Program,
        id: ConstId,
        decl: &ConstDecl,
    ) -> Result<(), io::Error> {
        self.write_indent()?;
        write!(self.writer, "const {}", decl.name.name)?;
        self.print_declaration_info(id.index(), "c")?;

        write!(self.writer, ": ")?;
        self.print_ast_type_with_info(prog, decl.ty)?;
        self.print_resolved_type_info(prog, &decl.resolved_type)?;
        write!(self.writer, " = ")?;
        self.print_expression_inline(prog, decl.value)?;
        writeln!(self.writer, ";")?;
        Ok(())
    }

    fn visit_table_decl(
        &mut self,
        prog: &Program,
        id: TableId,
        decl: &TableDecl,
    ) -> Result<(), io::Error> {
        self.write_indent()?;
        write!(self.writer, "table {}", decl.name.name)?;
        self.print_declaration_info(id.index(), "tab")?;

        writeln!(self.writer, " {{")?;
        self.indent();

        for (field_index, element) in decl.elements.iter().enumerate() {
            match element {
                TableElement::Field(field) => {
                    self.write_indent()?;
                    if field.is_primary {
                        write!(self.writer, "primary ")?;
                    }
                    write!(self.writer, "{}", field.name.name)?;
                    self.print_declaration_info(field_index, "field")?;
                    write!(self.writer, ": ")?;
                    self.print_ast_type_with_info(prog, field.ty)?;
                    self.print_resolved_type_info(prog, &field.resolved_type)?;
                    writeln!(self.writer, ";")?;
                }
                TableElement::Node(node) => {
                    self.write_indent()?;
                    write!(self.writer, "node {}(", node.name.name)?;
                    for (i, arg) in node.args.iter().enumerate() {
                        if i > 0 {
                            write!(self.writer, ", ")?;
                        }
                        self.print_expression_inline(prog, *arg)?;
                    }
                    writeln!(self.writer, ");")?;
                }
                TableElement::Invariant(expr) => {
                    self.write_indent()?;
                    write!(self.writer, "invariant ")?;
                    self.print_expression_inline(prog, *expr)?;
                    writeln!(self.writer, ";")?;
                }
            }
        }

        self.dedent();
        self.write_indent()?;
        writeln!(self.writer, "}}")?;
        Ok(())
    }

    fn visit_block(&mut self, prog: &Program, id: BlockId) -> Result<(), io::Error> {
        let block = &prog.blocks[id];
        self.write_indent()?;
        writeln!(self.writer, "{{")?;
        self.indent();

        for stmt_id in &block.statements {
            self.visit_stmt(prog, *stmt_id)?;
        }

        self.dedent();
        self.write_indent()?;
        writeln!(self.writer, "}}")?;
        Ok(())
    }

    fn visit_stmt(&mut self, prog: &Program, id: StmtId) -> Result<(), io::Error> {
        match &prog.statements[id] {
            Statement::VarDecl(var_id) => {
                let var_decl = &prog.var_decls[*var_id];
                self.write_indent()?;
                write!(self.writer, "let {}", var_decl.name.name)?;
                self.print_declaration_info(var_id.index(), "v")?;
                if let Some(ty_id) = var_decl.ty {
                    write!(self.writer, ": ")?;
                    self.print_ast_type_with_info(prog, ty_id)?;
                }
                self.print_resolved_type_info(prog, &var_decl.resolved_type)?;
                if let Some(init_id) = var_decl.init {
                    write!(self.writer, " = ")?;
                    self.print_expression_inline(prog, init_id)?;
                }
                writeln!(self.writer, ";")?;
            }
            Statement::If {
                condition,
                then_block,
                else_block,
                ..
            } => {
                self.write_indent()?;
                write!(self.writer, "if ")?;
                self.print_expression_inline(prog, *condition)?;
                writeln!(self.writer)?;
                self.visit_block(prog, *then_block)?;
                if let Some(else_id) = else_block {
                    self.write_indent()?;
                    writeln!(self.writer, "else")?;
                    self.visit_block(prog, *else_id)?;
                }
            }
            Statement::For {
                init,
                condition,
                update,
                body,
                ..
            } => {
                self.write_indent()?;
                write!(self.writer, "for (")?;
                if let Some(for_init) = init {
                    match for_init {
                        ForInit::VarDecl(var_id) => {
                            let var_decl = &prog.var_decls[*var_id];
                            write!(self.writer, "let {}", var_decl.name.name)?;
                            self.print_declaration_info(var_id.index(), "v")?;
                            if let Some(ty_id) = var_decl.ty {
                                write!(self.writer, ": ")?;
                                self.print_ast_type_with_info(prog, ty_id)?;
                            }
                            self.print_resolved_type_info(prog, &var_decl.resolved_type)?;
                            if let Some(init_id) = var_decl.init {
                                write!(self.writer, " = ")?;
                                self.print_expression_inline(prog, init_id)?;
                            }
                        }
                        ForInit::Expression(expr_id) => {
                            self.print_expression_inline(prog, *expr_id)?;
                        }
                    }
                }
                write!(self.writer, "; ")?;
                if let Some(cond_id) = condition {
                    self.print_expression_inline(prog, *cond_id)?;
                }
                write!(self.writer, "; ")?;
                if let Some(update_id) = update {
                    self.print_expression_inline(prog, *update_id)?;
                }
                writeln!(self.writer, ")")?;
                self.visit_block(prog, *body)?;
            }
            Statement::Return { value, .. } => {
                self.write_indent()?;
                write!(self.writer, "return")?;
                if let Some(val_id) = value {
                    write!(self.writer, " ")?;
                    self.print_expression_inline(prog, *val_id)?;
                }
                writeln!(self.writer, ";")?;
            }
            Statement::Assert { expr, .. } => {
                self.write_indent()?;
                write!(self.writer, "assert ")?;
                self.print_expression_inline(prog, *expr)?;
                writeln!(self.writer, ";")?;
            }
            Statement::Hop {
                decorators, body, ..
            } => {
                for decorator in decorators {
                    self.write_indent()?;
                    write!(self.writer, "@{}", decorator.name.name)?;
                    writeln!(self.writer)?;
                }
                self.write_indent()?;
                writeln!(self.writer, "hop")?;
                self.visit_block(prog, *body)?;
            }
            Statement::HopsFor {
                decorators,
                var,
                start,
                end,
                body,
                ..
            } => {
                for decorator in decorators {
                    self.write_indent()?;
                    write!(self.writer, "@{}", decorator.name.name)?;
                    writeln!(self.writer)?;
                }
                self.write_indent()?;
                let var_decl = &prog.var_decls[*var];
                write!(self.writer, "hops for {}", var_decl.name.name)?;
                self.print_declaration_info(var.index(), "v")?;
                write!(self.writer, " in ")?;
                self.print_expression_inline(prog, *start)?;
                write!(self.writer, "..")?;
                self.print_expression_inline(prog, *end)?;
                writeln!(self.writer)?;
                self.visit_block(prog, *body)?;
            }
            Statement::Expression { expr, .. } => {
                self.write_indent()?;
                self.print_expression_inline(prog, *expr)?;
                writeln!(self.writer, ";")?;
            }
            Statement::Block(block_id) => {
                self.visit_block(prog, *block_id)?;
            }
        }
        Ok(())
    }
}

impl<W: Write> AstPrinter<W> {
    // Helper methods for inline printing with systematic ID and type information
    fn print_expression_inline(&mut self, prog: &Program, expr_id: ExprId) -> io::Result<()> {
        let expr = &prog.expressions[expr_id];

        match expr {
            Expression::Literal {
                value,
                resolved_type,
                ..
            } => {
                self.print_literal_inline(prog, value)?;
                self.print_expression_decorators(prog, expr_id, resolved_type, &[])?;
            }
            Expression::Identifier {
                name,
                resolved_type,
                resolved_declarations,
                ..
            } => {
                write!(self.writer, "{}", name.name)?;
                self.print_expression_decorators(
                    prog,
                    expr_id,
                    resolved_type,
                    resolved_declarations,
                )?;
            }
            Expression::Binary {
                left,
                op,
                right,
                resolved_type,
                resolved_callables,
                ..
            } => {
                self.print_expression_inline(prog, *left)?;
                write!(self.writer, " {} ", op.name)?;
                self.print_callable_resolution_info(resolved_callables)?;
                self.print_expression_inline(prog, *right)?;
                self.print_expression_decorators(prog, expr_id, resolved_type, &[])?;
            }
            Expression::Unary {
                op,
                expr: inner_expr,
                resolved_type,
                resolved_callables,
                ..
            } => {
                write!(self.writer, "{}", op.name)?;
                self.print_callable_resolution_info(resolved_callables)?;
                self.print_expression_inline(prog, *inner_expr)?;
                self.print_expression_decorators(prog, expr_id, resolved_type, &[])?;
            }
            Expression::Assignment {
                lhs,
                rhs,
                resolved_type,
                ..
            } => {
                self.print_expression_inline(prog, *lhs)?;
                write!(self.writer, " = ")?;
                self.print_expression_inline(prog, *rhs)?;
                self.print_expression_decorators(prog, expr_id, resolved_type, &[])?;
            }
            Expression::Call {
                callee,
                args,
                resolved_type,
                resolved_callables,
                ..
            } => {
                self.print_expression_inline(prog, *callee)?;
                self.print_callable_resolution_info(resolved_callables)?;
                write!(self.writer, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.print_expression_inline(prog, *arg)?;
                }
                write!(self.writer, ")")?;
                self.print_expression_decorators(prog, expr_id, resolved_type, &[])?;
            }
            Expression::MemberAccess {
                object,
                member,
                resolved_type,
                resolved_table,
                resolved_field,
                ..
            } => {
                self.print_expression_inline(prog, *object)?;
                write!(self.writer, ".{}", member.name)?;
                if let Some(table_id) = resolved_table {
                    self.print_declaration_info(table_id.index(), "tab")?;
                }
                if let Some(_field) = resolved_field {
                    // TableField is a struct, not an enum, so just print the field info
                    write!(self.writer, "[field]")?;
                }
                self.print_expression_decorators(prog, expr_id, resolved_type, &[])?;
            }
            Expression::TableRowAccess {
                table,
                key_values,
                resolved_type,
                resolved_table,
                ..
            } => {
                self.print_expression_inline(prog, *table)?;
                if let Some(table_id) = resolved_table {
                    self.print_declaration_info(table_id.index(), "tab")?;
                }
                write!(self.writer, "[")?;
                for (i, kv) in key_values.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    write!(self.writer, "{}: ", kv.key.name)?;
                    self.print_expression_inline(prog, kv.value)?;
                }
                write!(self.writer, "]")?;
                self.print_expression_decorators(prog, expr_id, resolved_type, &[])?;
            }
            Expression::Grouped {
                expr,
                resolved_type,
                ..
            } => {
                write!(self.writer, "(")?;
                self.print_expression_inline(prog, *expr)?;
                write!(self.writer, ")")?;
                self.print_expression_decorators(prog, expr_id, resolved_type, &[])?;
            }
            Expression::Lambda {
                params,
                return_type,
                body,
                resolved_type,
                ..
            } => {
                write!(self.writer, "(")?;
                for (i, param_id) in params.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    let param = &prog.params[*param_id];
                    write!(self.writer, "{}", param.name.name)?;
                    self.print_declaration_info(param_id.index(), "p")?;
                    write!(self.writer, ": ")?;
                    self.print_ast_type_with_info(prog, param.ty)?;
                    self.print_resolved_type_info(prog, &param.resolved_type)?;
                }
                write!(self.writer, ") -> ")?;
                self.print_ast_type_with_info(prog, *return_type)?;
                write!(self.writer, " ")?;
                self.visit_block(prog, *body)?;
                self.print_expression_decorators(prog, expr_id, resolved_type, &[])?;
            }
        }
        Ok(())
    }

    fn print_literal_inline(&mut self, prog: &Program, literal: &Literal) -> io::Result<()> {
        match literal {
            Literal::Integer(val) => write!(self.writer, "{}", val)?,
            Literal::Float(val) => write!(self.writer, "{}", val)?,
            Literal::String(val) => write!(self.writer, "\"{}\"", val)?,
            Literal::Bool(val) => write!(self.writer, "{}", val)?,
            Literal::List(elements) => {
                write!(self.writer, "[")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.print_expression_inline(prog, *elem)?;
                }
                write!(self.writer, "]")?;
            }
            Literal::RowLiteral(key_values) => {
                write!(self.writer, "{{")?;
                for (i, kv) in key_values.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    write!(self.writer, "{}: ", kv.key.name)?;
                    self.print_expression_inline(prog, kv.value)?;
                }
                write!(self.writer, "}}")?;
            }
        }
        Ok(())
    }

    /// Print expression decorators in format [expr_id][resolved_type][name_resolution]
    fn print_expression_decorators(
        &mut self,
        prog: &Program,
        expr_id: ExprId,
        resolved_type: &Option<ResolvedType>,
        resolutions: &[IdentifierResolution],
    ) -> io::Result<()> {
        // Resolved Type
        self.print_resolved_type_info(prog, resolved_type)?;

        // Name Resolution
        if SHOW_NAME_RESOLUTION && !resolutions.is_empty() {
            write!(self.writer, "[")?;
            for (i, res) in resolutions.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ",")?;
                }
                self.print_resolution_compact(res)?;
            }
            write!(self.writer, "]")?;
        }
        Ok(())
    }

    /// Print callable resolution for binary/unary operators and function calls
    fn print_callable_resolution_info(&mut self, callables: &[FunctionId]) -> io::Result<()> {
        if SHOW_NAME_RESOLUTION && !callables.is_empty() {
            write!(self.writer, "[")?;
            for (i, callable_id) in callables.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ", ")?;
                }
                write!(self.writer, "f{}", callable_id.index())?;
            }
            write!(self.writer, "]")?;
        }
        Ok(())
    }
}

impl PrettyPrint for Program {
    fn pretty_print(&self, writer: &mut impl Write) -> io::Result<()> {
        let mut printer = AstPrinter::new(writer);
        printer.print_program(self)
    }
}

impl Program {
    /// Pretty print with debug information
    pub fn pretty_print_with_debug(&self, writer: &mut impl Write) -> io::Result<()> {
        let mut printer = AstPrinter::new(writer);
        printer.print_program(self)
    }
}
