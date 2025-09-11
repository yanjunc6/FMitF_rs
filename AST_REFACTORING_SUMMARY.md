# AST Refactoring Summary

## Problem
The original AST had a terrible design with generic `resolved` and `resolved_type` fields in the `Identifier` struct, plus global `DeclRef` and `ResolvedType` enums that were overly generic and inconsistent.

## Solution
Completely refactored the AST to use **specific resolved fields at each AST node** where they're actually needed, following the good examples of `TableNode::resolved_partition` and `resolved_callable` fields.

## Changes Made

### 1. Cleaned up Identifier
**Before:**
```rust
pub struct Identifier {
    pub name: String,
    pub span: Option<Span>,
    pub resolved: Option<DeclRef>,           // ❌ Generic, terrible
    pub resolved_type: Option<ResolvedType>, // ❌ Generic, terrible
}
```

**After:**
```rust
pub struct Identifier {
    pub name: String,
    pub span: Option<Span>,
    // ✅ Clean - resolution stored at specific nodes where needed
}
```

### 2. Added Specific Resolution Fields

#### Type Resolution
```rust
pub enum Type {
    Named {
        name: Identifier,
        resolved_type: Option<TypeDeclId>, // ✅ Specific
    },
    Generic {
        base: Identifier,
        args: Vec<TypeId>,
        resolved_base_type: Option<TypeDeclId>, // ✅ Specific
        span: Option<Span>,
    },
    // ...
}
```

#### Expression Resolution
```rust
pub enum Expression {
    Identifier {
        name: Identifier,
        resolved_declaration: Option<IdentifierResolution>, // ✅ Specific
        span: Option<Span>,
    },
    
    MemberAccess {
        object: ExprId,
        member: Identifier,
        resolved_table: Option<TableId>,     // ✅ Specific - which table
        resolved_field: Option<TableField>,  // ✅ Specific - field details
        span: Option<Span>,
    },
    
    TableRowAccess {
        table: ExprId,
        key_values: Vec<KeyValue>,
        resolved_table: Option<TableId>,     // ✅ Specific
        span: Option<Span>,
    },
    
    // Binary and Unary already had good resolved_callable fields
    Binary {
        // ...
        resolved_callable: Option<FunctionId>, // ✅ Already good
    },
    Unary {
        // ...
        resolved_callable: Option<FunctionId>, // ✅ Already good
    },
}
```

#### Variable and Parameter Resolution
```rust
pub struct VarDecl {
    pub name: Identifier,
    pub ty: Option<TypeId>,
    pub init: Option<ExprId>,
    pub resolved_type: Option<TypeDeclId>, // ✅ Specific
    pub span: Option<Span>,
}

pub struct Parameter {
    pub name: Identifier,
    pub ty: TypeId,
    pub resolved_type: Option<TypeDeclId>, // ✅ Specific
    pub span: Option<Span>,
}
```

#### Key-Value Resolution
```rust
pub struct KeyValue {
    pub key: Identifier,
    pub value: ExprId,
    pub resolved_table: Option<TableId>,     // ✅ Specific - which table
    pub resolved_field: Option<TableField>,  // ✅ Specific - field details
    pub span: Option<Span>,
}
```

### 3. Removed Bad Global Types
- ❌ Deleted `DeclRef` enum (too generic)
- ❌ Deleted `ResolvedType` enum (too generic)
- ✅ Added specific `IdentifierResolution` enum for identifier expressions only

### 4. Updated All Systems
- ✅ Updated name resolver to use specific resolution fields
- ✅ Updated AST builder to initialize all new fields as `None`
- ✅ Updated pattern matching in prelude and printer
- ✅ All compilation errors fixed
- ✅ Tested with custom operators - still works perfectly

## Benefits

1. **Consistency**: All resolution follows the same pattern as the good examples (`resolved_callable`, `resolved_partition`)
2. **Type Safety**: Each resolved field has the exact type needed for that context
3. **Clarity**: No more guessing what a generic `DeclRef` means in different contexts
4. **Maintainability**: Easy to add new resolution fields where needed
5. **Performance**: No unnecessary indirection through generic enums

## Validation
- ✅ All compilation errors fixed
- ✅ Custom operator test still passes
- ✅ Resolution test works correctly
- ✅ No TODOs left in name_resolver.rs or ast_builder.rs
- ✅ Complete job finished

The AST now follows a clean, consistent pattern where **resolution information is stored exactly where it's needed** with **specific types for specific purposes**.
