use crate::ast::visit::Visitor;
use crate::ast::*;
use crate::pretty::PrettyPrint;
use std::io::{self, Write};

// Debug options - simple constants for development
const SHOW_IDS: bool = true;
const SHOW_NAME_RESOLUTION: bool = false;
const SHOW_TYPE_RESOLUTION: bool = false;

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

    /// Helper to print debug info if enabled
    fn debug_info(
        &mut self,
        id: impl std::fmt::Display,
        node_type: &str,
        resolutions: &[IdentifierResolution],
        resolved_type: &Option<ResolvedType>,
    ) -> io::Result<()> {
        if SHOW_IDS {
            write!(self.writer, " /*{}:{}*/", node_type, id)?;
        }
        if SHOW_NAME_RESOLUTION && !resolutions.is_empty() {
            write!(self.writer, " /*name:")?;
            for (i, res) in resolutions.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ",")?;
                }
                match res {
                    IdentifierResolution::Var(id) => write!(self.writer, "var:{}", id.index())?,
                    IdentifierResolution::Param(id) => write!(self.writer, "param:{}", id.index())?,
                    IdentifierResolution::Const(id) => write!(self.writer, "const:{}", id.index())?,
                    IdentifierResolution::Function(id) => {
                        write!(self.writer, "func:{}", id.index())?
                    }
                    IdentifierResolution::Type(id) => write!(self.writer, "type:{}", id.index())?,
                    IdentifierResolution::Table(id) => write!(self.writer, "table:{}", id.index())?,
                    IdentifierResolution::GenericParam(id) => {
                        write!(self.writer, "generic:{}", id.index())?
                    }
                }
            }
            write!(self.writer, "*/")?;
        }
        if SHOW_TYPE_RESOLUTION {
            match resolved_type {
                Some(ty) => {
                    write!(self.writer, " /*type:")?;
                    self.write_type(ty)?;
                    write!(self.writer, "*/")?;
                }
                None => write!(self.writer, " /*type:unresolved*/")?,
            }
        }
        Ok(())
    }

    /// Helper to write type information
    fn write_type(&mut self, ty: &ResolvedType) -> io::Result<()> {
        match ty {
            ResolvedType::Primitive { type_id, .. } => {
                write!(self.writer, "prim:{}", type_id.index())
            }
            ResolvedType::Table { table_id } => write!(self.writer, "table:{}", table_id.index()),
            ResolvedType::TypeVariable { name, var_id } => {
                write!(self.writer, "{}#{}", name, var_id)
            }
            ResolvedType::Void => write!(self.writer, "void"),
            ResolvedType::Unknown => write!(self.writer, "unknown"),
            ResolvedType::Unresolved(name) => write!(self.writer, "unresolved:{:?}", name),
            _ => write!(self.writer, "complex"), // Simplified for brevity
        }
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

        // Add declaration ID if debug is enabled
        if SHOW_IDS {
            write!(self.writer, "[d{}]", id.index())?;
        }

        // Generic parameters
        if !decl.generic_params.is_empty() {
            write!(self.writer, "<")?;
            for (i, param_id) in decl.generic_params.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ", ")?;
                }
                let param = &prog.generic_params[*param_id];
                write!(self.writer, "{}", param.name.name)?;
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
            write!(self.writer, "{}: ", param.name.name)?;
            self.debug_info(param_id.index(), "param", &[], &param.resolved_type)?;
            self.print_ast_type_inline(prog, param.ty)?;
        }
        write!(self.writer, ")")?;

        // Return type
        if let Some(return_type_id) = decl.return_type {
            write!(self.writer, " -> ")?;
            self.print_ast_type_inline(prog, return_type_id)?;
            self.debug_info(0, "ret", &[], &decl.resolved_return_type)?;
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

        // Add declaration ID if debug is enabled
        if SHOW_IDS {
            write!(self.writer, "[d{}]", id.index())?;
        }

        // Generic parameters
        if !decl.generic_params.is_empty() {
            write!(self.writer, "<")?;
            for (i, param_id) in decl.generic_params.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ", ")?;
                }
                let param = &prog.generic_params[*param_id];
                write!(self.writer, "{}", param.name.name)?;
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

        // Add declaration ID if debug is enabled
        if SHOW_IDS {
            write!(self.writer, "[d{}]", id.index())?;
        }

        write!(self.writer, ": ")?;
        self.print_ast_type_inline(prog, decl.ty)?;
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

        // Add declaration ID if debug is enabled
        if SHOW_IDS {
            write!(self.writer, "[d{}]", id.index())?;
        }

        writeln!(self.writer, " {{")?;
        self.indent();

        for element in &decl.elements {
            match element {
                TableElement::Field(field) => {
                    self.write_indent()?;
                    if field.is_primary {
                        write!(self.writer, "primary ")?;
                    }
                    write!(self.writer, "{}: ", field.name.name)?;
                    self.print_ast_type_inline(prog, field.ty)?;
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
                if let Some(ty_id) = var_decl.ty {
                    write!(self.writer, ": ")?;
                    self.print_ast_type_inline(prog, ty_id)?;
                }
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
                            if let Some(ty_id) = var_decl.ty {
                                write!(self.writer, ": ")?;
                                self.print_ast_type_inline(prog, ty_id)?;
                            }
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
                write!(self.writer, "hops for {} in ", var_decl.name.name)?;
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
    // Helper methods for inline printing without visiting
    fn print_expression_inline(&mut self, prog: &Program, expr_id: ExprId) -> io::Result<()> {
        let expr = &prog.expressions[expr_id];

        match expr {
            Expression::Literal { value, .. } => {
                self.print_literal_with_decorators(prog, value, expr_id, expr)?;
            }
            Expression::Identifier { name, .. } => {
                self.print_identifier_with_decorators(name.name.clone(), expr_id, expr)?;
            }
            Expression::Binary {
                left, op, right, ..
            } => {
                self.print_expression_inline(prog, *left)?;
                write!(self.writer, " {} ", op.name)?;
                self.print_expression_inline(prog, *right)?;
            }
            Expression::Unary {
                op,
                expr: inner_expr,
                ..
            } => {
                write!(self.writer, "{}", op.name)?;
                self.print_expression_inline(prog, *inner_expr)?;
            }
            Expression::Assignment { lhs, rhs, .. } => {
                self.print_expression_inline(prog, *lhs)?;
                write!(self.writer, " = ")?;
                self.print_expression_inline(prog, *rhs)?;
            }
            Expression::Call { callee, args, .. } => {
                self.print_expression_inline(prog, *callee)?;
                write!(self.writer, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.print_expression_inline(prog, *arg)?;
                }
                write!(self.writer, ")")?;
            }
            Expression::MemberAccess { object, member, .. } => {
                self.print_expression_inline(prog, *object)?;
                write!(self.writer, ".{}", member.name)?;
            }
            Expression::TableRowAccess {
                table, key_values, ..
            } => {
                self.print_expression_inline(prog, *table)?;
                write!(self.writer, "[")?;
                for (i, kv) in key_values.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    write!(self.writer, "{}: ", kv.key.name)?;
                    self.print_expression_inline(prog, kv.value)?;
                }
                write!(self.writer, "]")?;
            }
            Expression::Grouped { expr, .. } => {
                write!(self.writer, "(")?;
                self.print_expression_inline(prog, *expr)?;
                write!(self.writer, ")")?;
            }
            Expression::Lambda {
                params,
                return_type,
                body,
                ..
            } => {
                write!(self.writer, "(")?;
                for (i, param_id) in params.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    let param = &prog.params[*param_id];
                    write!(self.writer, "{}: ", param.name.name)?;
                    self.print_ast_type_inline(prog, param.ty)?;
                }
                write!(self.writer, ") -> ")?;
                self.print_ast_type_inline(prog, *return_type)?;
                write!(self.writer, " ")?;

                // Print the lambda body using normal block printing
                self.visit_block(prog, *body)?;
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

    fn print_ast_type_inline(&mut self, prog: &Program, type_id: AstTypeId) -> io::Result<()> {
        let ast_type = &prog.types[type_id];
        match ast_type {
            AstType::Named { name, .. } => {
                write!(self.writer, "{}", name.name)?;
            }
            AstType::Generic { base, args, .. } => {
                write!(self.writer, "{}<", base.name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(self.writer, ", ")?;
                    }
                    self.print_ast_type_inline(prog, *arg)?;
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
                    self.print_ast_type_inline(prog, *param)?;
                }
                write!(self.writer, ") -> ")?;
                self.print_ast_type_inline(prog, *return_type)?;
            }
        }
        Ok(())
    }

    /// Print literal value with decorators like: 42[e17][int]
    fn print_literal_with_decorators(
        &mut self,
        prog: &Program,
        value: &Literal,
        expr_id: ExprId,
        expr: &Expression,
    ) -> io::Result<()> {
        // Print the literal value first
        self.print_literal_inline(prog, value)?;

        // Add decorators if enabled
        self.print_expression_decorators(expr_id, expr)?;
        Ok(())
    }

    /// Print identifier with decorators like: a[e17][int][var:5]
    fn print_identifier_with_decorators(
        &mut self,
        name: String,
        expr_id: ExprId,
        expr: &Expression,
    ) -> io::Result<()> {
        // Print the identifier name first
        write!(self.writer, "{}", name)?;

        // Add decorators if enabled
        self.print_expression_decorators(expr_id, expr)?;
        Ok(())
    }

    /// Print decorators for expressions like [e17][int][var:5]
    fn print_expression_decorators(
        &mut self,
        expr_id: ExprId,
        expr: &Expression,
    ) -> io::Result<()> {
        // Expression ID decorator
        if SHOW_IDS {
            write!(self.writer, "[e{}]", expr_id.index())?;
        }

        // Type decorator
        if SHOW_TYPE_RESOLUTION {
            let resolved_type = match expr {
                Expression::Literal { resolved_type, .. } => resolved_type,
                Expression::Identifier { resolved_type, .. } => resolved_type,
                Expression::Binary { resolved_type, .. } => resolved_type,
                Expression::Unary { resolved_type, .. } => resolved_type,
                Expression::Assignment { resolved_type, .. } => resolved_type,
                Expression::Call { resolved_type, .. } => resolved_type,
                Expression::MemberAccess { resolved_type, .. } => resolved_type,
                Expression::TableRowAccess { resolved_type, .. } => resolved_type,
                Expression::Grouped { resolved_type, .. } => resolved_type,
                Expression::Lambda { resolved_type, .. } => resolved_type,
            };

            if let Some(ty) = resolved_type {
                write!(self.writer, "[")?;
                self.write_type_simple(ty)?;
                write!(self.writer, "]")?;
            } else {
                write!(self.writer, "[?]")?;
            }
        }

        // Name resolution decorator
        if SHOW_NAME_RESOLUTION {
            if let Expression::Identifier {
                resolved_declarations,
                ..
            } = expr
            {
                if !resolved_declarations.is_empty() {
                    write!(self.writer, "[")?;
                    for (i, res) in resolved_declarations.iter().enumerate() {
                        if i > 0 {
                            write!(self.writer, ",")?;
                        }
                        self.write_resolution_simple(res)?;
                    }
                    write!(self.writer, "]")?;
                }
            }

            // For binary operations, show callable resolution
            if let Expression::Binary {
                resolved_callables, ..
            } = expr
            {
                if !resolved_callables.is_empty() {
                    write!(self.writer, "[calls:")?;
                    for (i, id) in resolved_callables.iter().enumerate() {
                        if i > 0 {
                            write!(self.writer, ",")?;
                        }
                        write!(self.writer, "{}", id.index())?;
                    }
                    write!(self.writer, "]")?;
                }
            }
        }

        Ok(())
    }

    /// Write type information in simple format
    fn write_type_simple(&mut self, ty: &ResolvedType) -> io::Result<()> {
        match ty {
            ResolvedType::Primitive { type_id, .. } => {
                // Get the actual type name if possible
                write!(self.writer, "t{}", type_id.index())
            }
            ResolvedType::Table { table_id } => write!(self.writer, "table{}", table_id.index()),
            ResolvedType::TypeVariable { name, .. } => {
                write!(self.writer, "{}", name)
            }
            ResolvedType::Void => write!(self.writer, "void"),
            ResolvedType::Unknown => write!(self.writer, "?"),
            ResolvedType::Unresolved(ast_type_id) => {
                write!(self.writer, "unresolved#{}", ast_type_id.index())
            }
            _ => write!(self.writer, "complex"), // For more complex types
        }
    }

    /// Write resolution information in simple format
    fn write_resolution_simple(&mut self, res: &IdentifierResolution) -> io::Result<()> {
        match res {
            IdentifierResolution::Var(id) => write!(self.writer, "v{}", id.index()),
            IdentifierResolution::Param(id) => write!(self.writer, "p{}", id.index()),
            IdentifierResolution::Const(id) => write!(self.writer, "c{}", id.index()),
            IdentifierResolution::Function(id) => write!(self.writer, "f{}", id.index()),
            IdentifierResolution::Type(id) => write!(self.writer, "t{}", id.index()),
            IdentifierResolution::Table(id) => write!(self.writer, "tab{}", id.index()),
            IdentifierResolution::GenericParam(id) => write!(self.writer, "g{}", id.index()),
        }
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
