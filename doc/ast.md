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

To analyze or transform the AST, you should use one of the three provided traversal traits. These patterns separate the traversal logic (walking the tree) from the action logic (what to do at each node).

### 3.1. `visit::Visitor` (Read-only Inspection)

-   **Purpose:** To traverse the AST and gather information without modifying it.
-   **How it Works:**
    -   You implement the `Visitor` trait.
    -   Methods have the signature `fn visit_expr(&mut self, prog: &'ast Program, id: ExprId)`.
    -   Methods return `()`.
    -   You store any collected data within your visitor struct's state (e.g., a `Vec<Warning>`).
    -   You only need to override the `visit_*` methods for the nodes you care about. The default `walk_*` functions handle recursive traversal automatically.
-   **Example Use Case:**
    -   A linter that checks for style violations.
    -   A symbol collector that gathers all declared variables in a scope.

```rust
struct FunctionLinter {
    errors: Vec<String>,
}

impl<'ast> Visitor<'ast> for FunctionLinter {
    // Only override the function visitor
    fn visit_callable_decl(&mut self, _prog: &'ast Program, _id: FunctionId, decl: &'ast CallableDecl) {
        if decl.name.to_string().chars().next().unwrap().is_uppercase() {
            self.errors.push("Function names should start with a lowercase letter.".to_string());
        }
    }
}
```

### 3.2. `visit_mut::VisitorMut` (In-place Mutation)

-   **Purpose:** To traverse the AST and modify nodes in-place.
-   **How it Works:**
    -   You implement the `VisitorMut` trait.
    -   Methods have the signature `fn visit_expr(&mut self, prog: &mut Program, id: ExprId)`. Notice `&mut Program`.
    -   Methods return `()`. Modifications are done directly by mutating the node fetched from the arena.
    -   Because you are modifying the arenas while iterating, the `walk_*_mut` functions often need to `clone()` node data to satisfy the borrow checker.
-   **Example Use Case:**
    -   **Name Resolution:** Finding an `Identifier` expression and filling its `resolved_declaration` field.
    -   **Type Checking:** Filling the `resolved_type` field on expressions and variable declarations.

```rust
struct TypeResolver { /* ... */ }

impl VisitorMut for TypeResolver {
    fn visit_expr(&mut self, prog: &mut Program, id: ExprId) {
        // Compute the type of the expression...
        let computed_type = /* ... */;

        // Mutate the node in the arena
        let expr_node = &mut prog.expressions[id];
        expr_node.resolved_type = Some(computed_type);

        // Continue traversal
        walk_expr_mut(self, prog, id);
    }
}
```

### 3.3. `fold::Fold` (Transformation & Reconstruction)

-   **Purpose:** To traverse the AST and **transform** it by replacing nodes with new ones, effectively creating a new (or modified) tree. This is similar to the `syn::Fold` pattern.
-   **How it Works:**
    -   You implement the `Fold` trait.
    -   Methods have the signature `fn fold_expr(&mut self, expr: Expression) -> Expression`.
    -   Each `fold_*` method takes ownership of a node and **returns a new, potentially different, node**.
    -   The default `walk_and_fold_*` functions handle the recursion: they call `fold_*` on all children, then rebuild the parent node using the *new* children that were returned.
-   **Example Use Case:**
    -   **Constant Folding:** Replacing an expression like `2 + 3` with the single literal `5`.
    -   **Desugaring:** Replacing a `for` loop node with its equivalent `while` loop representation.
    -   Lowering the AST to an Intermediate Representation (IR).

```rust
struct ConstantFolder { /* ... */ }

impl Fold for ConstantFolder {
    fn fold_expr(&mut self, expr: Expression) -> Expression {
        // First, fold the children of the expression
        let folded_expr = walk_and_fold_expr(self, expr);

        // Now, apply the transformation to the current node
        if let Expression::Binary { op, left, right, .. } = folded_expr {
            if op.value == "+" {
                // Try to evaluate `left + right`
                if let (Expression::Literal(Literal::Integer(l)), Expression::Literal(Literal::Integer(r))) = (&left, &right) {
                    // If successful, return a new Literal node instead of the Binary node
                    let result = l + r;
                    return Expression::Literal(Literal::Integer(result));
                }
            }
        }
        // Otherwise, return the (already folded) expression as is
        folded_expr
    }
}
```