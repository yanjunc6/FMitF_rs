use crate::ast::visit::Visitor;
use crate::ast::*;
use crate::pretty::PrettyPrint;
use std::io::{self, Write};

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
        _id: FunctionId,
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
            self.print_ast_type_inline(prog, param.ty)?;
        }
        write!(self.writer, ")")?;

        // Return type
        if let Some(return_type_id) = decl.return_type {
            write!(self.writer, " -> ")?;
            self.print_ast_type_inline(prog, return_type_id)?;
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
        _id: TypeDeclId,
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
        _id: ConstId,
        decl: &ConstDecl,
    ) -> Result<(), io::Error> {
        self.write_indent()?;
        write!(self.writer, "const {}: ", decl.name.name)?;
        self.print_ast_type_inline(prog, decl.ty)?;
        write!(self.writer, " = ")?;
        self.print_expression_inline(prog, decl.value)?;
        writeln!(self.writer, ";")?;
        Ok(())
    }

    fn visit_table_decl(
        &mut self,
        prog: &Program,
        _id: TableId,
        decl: &TableDecl,
    ) -> Result<(), io::Error> {
        self.write_indent()?;
        writeln!(self.writer, "table {} {{", decl.name.name)?;
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
                self.print_literal_inline(prog, value)?;
            }
            Expression::Identifier { name, .. } => {
                write!(self.writer, "{}", name.name)?;
            }
            Expression::Binary {
                left, op, right, ..
            } => {
                self.print_expression_inline(prog, *left)?;
                write!(self.writer, " {} ", op.name)?;
                self.print_expression_inline(prog, *right)?;
            }
            Expression::Unary { op, expr, .. } => {
                write!(self.writer, "{}", op.name)?;
                self.print_expression_inline(prog, *expr)?;
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
                // For lambda body, we need to print inline but that's complex
                // For now, just indicate it's a lambda body
                write!(self.writer, "{{ ... }}")?;
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
}

impl PrettyPrint for Program {
    fn pretty_print(&self, writer: &mut impl Write) -> io::Result<()> {
        let mut printer = AstPrinter::new(writer);
        printer.print_program(self)
    }
}
