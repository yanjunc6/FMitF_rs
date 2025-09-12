# AST (Abstract Syntax Tree) Module Documentation

This document provides a high-level overview of the `ast` module, its design principles, and the patterns used to interact with it.

## 1. Core Design Philosophy

The AST is the central data structure in the compiler, representing the parsed source code in a hierarchical format. Its design is guided by three core principles:

### 1.1. Arena-Based Storage

Instead of using standard Rust ownership with `Box<T>` and references (`&T`), the AST uses an **arena allocation** model, powered by the `id_arena` crate.

-   **How it Works:** All AST nodes (expressions, statements, etc.) are stored in `Arena<T>` collections within the single `Program` struct. A node is never referred to by a direct pointer but by a lightweight, copyable `Id<T>` (e.g., `ExprId`, `StmtId`).
-   **Why:** This completely eliminates Rust lifetime and borrowing complexities. You can pass an `Id` around freely without worrying about ownership. It also improves performance by reducing heap allocations and improving data locality.

### 1.2. Compositional and Granular Structure

The AST is not a single, monolithic enum. Instead, it is broken down into several distinct, logical categories that mirror the language's grammar.

-   `item`: Top-level declarations (`function`, `const`, `table`, `type`).
-   `stmt`: Statements that perform actions but do not return values (`let`, `if`, `return`).
-   `expr`: Expressions that evaluate to a value (`1 + 2`, `my_func()`, `a`).
-   `ty`: Type annotations (`int`, `List<string>`).
-   `common`: Shared, simple structures like `Identifier` and `Span`.

This compositional design makes the code more organized, readable, and easier to extend.

### 1.3. Designed for Multi-Pass Analysis

The AST nodes contain `resolved_*` fields (e.g., `resolved_type`, `resolved_declaration`) which are initially `None`. These fields are designed to be filled in by subsequent compiler passes.

-   **Pass 1 (Parsing):** The `AstBuilder` populates the arenas with the basic structure from the source code.
-   **Pass 2 (Name Resolution):** A visitor pass fills in `resolved_declaration` fields, linking identifiers to the `Id` of what they refer to.
-   **Pass 3 (Type Checking):** Another visitor pass fills in `resolved_type` fields with the computed `ResolvedType` for each expression and variable.

---

## 2. AST Node Hierarchy

The entire AST is owned by the `Program` struct, which contains the arenas.

-   **`Program`**: The root of the AST. It holds `Arena`s for every node type and a `Vec<Item>` representing the top-level declarations in the source file.

-   **`item::Item`**: Represents a top-level declaration.
    -   `Callable(FunctionId)`: A function, operator, or transaction.
    -   `Const(ConstId)`: A constant global declaration.
    -   `Type(TypeDeclId)`: A user-defined type declaration.
    -   `Table(TableId)`: A table schema declaration.

-   **`stmt::Statement`**: Represents a statement.
    -   `VarDecl(VarId)`: A `let` binding.
    -   `If { ... }`: An if-else conditional block.
    -   `Return { ... }`: A return statement.
    -   `Expression { ... }`: An expression used as a statement (e.g., a function call).

-   **`expr::Expression`**: Represents an expression that yields a value.
    -   `Literal { ... }`: A primitive value like an integer, string, or boolean.
    -   `Identifier { ... }`: A reference to a variable, function, etc.
    -   `Binary { ... }`: An infix operation (`+`, `-`, `*`, `/`).
    -   `Unary { ... }`: A prefix operation (`-`, `!`).
    -   `Call { ... }`: A function call.
    -   `MemberAccess { ... }`: Accessing a field of an object (`my_table.field`).

-   **`ty::AstType`**: Represents a type annotation written in the source code.
    -   `Named { ... }`: A simple named type like `int` or `MyType`.
    -   `Generic { ... }`: A generic type like `List<int>`.
    -   `Function { ... }`: A function type like `(int, bool) -> string`.

---

## 3. AST Traversal Patterns
To analyze or transform the AST you can use one of three traversal/transform patterns implemented in the codebase. Each pattern separates the traversal (walking the structure) from the action logic.

High-level mapping:
- Read-only traversal: `src/ast/visit.rs` -> trait `Visitor<'ast, R: Default = (), E = ()>`
- Mutable, in-place traversal: `src/ast/visit_mut.rs` -> trait `VisitorMut<'ast, R: Default = (), E = ()>`
- Transform/fold traversal: `src/ast/fold.rs` -> trait `Fold`

### Visitor (read-only)

- Trait signature: `pub trait Visitor<'ast, R: Default = (), E = ()>: Sized`.
- Methods take `&'ast Program` (borrowed) and node identifiers or references and return `Result<R, E>` where `R` is a return/accumulator type (defaults to `()`), and `E` is an error type.
- Every `visit_*` method has a default implementation that calls a corresponding walker function (e.g. `visit_expr` -> `walk_expr`). These helper walkers perform the recursive traversal and call back into the visitor for children.
- Walkers are named `walk_*` (for example: `walk_program`, `walk_item`, `walk_callable_decl`, `walk_stmt`, `walk_expr`, `walk_ast_type`). They traverse the arenas and call visitor methods for child nodes.
- Use this when you only need to inspect the AST without mutating it (linters, collectors, analyses).

Example important signatures:
- `fn visit_callable_decl(&mut self, prog: &'ast Program, id: FunctionId, decl: &'ast CallableDecl) -> Result<R, E>`
- `fn visit_expr(&mut self, prog: &'ast Program, id: ExprId) -> Result<R, E>`

### VisitorMut (in-place mutation)

- Trait signature: `pub trait VisitorMut<'ast, R: Default = (), E = ()>: Sized`.
- Methods take `&mut Program` and node identifiers and return `Result<R, E>`.
- Default implementations call `walk_*_mut` helper functions (e.g. `walk_expr_mut`) that perform recursive traversal while allowing mutations.
- The mutable walkers frequently clone IDs or clone node data (e.g. `let decl = prog.functions[id].clone()`) to avoid multiple simultaneous mutable borrows of the `Program` arenas. This is intentional to satisfy the borrow checker while still allowing in-place updates via the visitor methods.
- Use this when you want to modify nodes in the existing arenas (name resolution, filling `resolved_*` fields, type inference that writes into nodes).

Example important signatures:
- `fn visit_program(&mut self, prog: &mut Program) -> Result<R, E>`
- `fn visit_expr(&mut self, prog: &mut Program, id: ExprId) -> Result<R, E>`

### Fold (transform / reconstruction)

- Trait signature: `pub trait Fold: Sized`.
- Fold methods take ownership of nodes (or the whole `Program`) and return transformed nodes. For example: `fn fold_program(&mut self, prog: Program) -> Program` and `fn fold_expr(&mut self, prog: &Program, id: ExprId, expr: Expression) -> Expression`.
- Default fold implementations call `foldwalk_*` helper functions (note the helper name prefix is `foldwalk_`), e.g. `foldwalk_program`, `foldwalk_expr`, which traverse the program and invoke the folder on child nodes.
- The `Fold` implementation in the repository is a simplified pattern that clones from the read-only `Program` (since arenas are owned elsewhere) and returns new or rebuilt nodes. In a full arena-aware implementation you would rebuild arenas; the current helpers clone children from `prog` and call `folder.fold_*` on them.
- Use `Fold` when you want to produce a new AST (constant folding, desugaring, lowering).

Example important signatures:
- `fn fold_expr(&mut self, prog: &Program, id: ExprId, expr: Expression) -> Expression`
- `fn fold_program(&mut self, prog: Program) -> Program`

Notes and gotchas
- Visitor/VisitorMut use a generic return type `R` (with `Default`) and an error type `E`, so visitors can accumulate results or return early with errors.
- `VisitorMut` and `Fold` helpers clone node data to avoid borrowing issues; be mindful of the extra copies when writing performance-critical code.
- The default walker names are: `walk_*` for read-only visitors, `walk_*_mut` for mutable visitors, and `foldwalk_*` for the folder helpers.

That covers the API shape and recommended usage for the three traversal patterns in the codebase.