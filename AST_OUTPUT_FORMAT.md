# AST Pretty Printer Output Format Documentation

This document explains how to read the systematic AST output format used by the FMitF_rs compiler.

## Overview

The AST printer uses a systematic format where every declaration and expression is annotated with ID and type information using square brackets `[...]`. This provides complete traceability of the compiler's internal state during compilation.

## ID Format Categories

### Declaration IDs

#### Function IDs: `[f###]`
- **Format**: `[f0]`, `[f1]`, `[f42]`, etc.
- **Usage**: Identifies function, operator, transaction, and partition declarations
- **Example**: `transaction beFriend[f41](...)`

#### Type IDs: `[t###]`
- **Format**: `[t0]`, `[t1]`, `[t5]`, etc.
- **Usage**: Identifies type declarations (int, bool, List, etc.)
- **Example**: `type int[t0];` or parameter `x[p0]: int[t0]`

#### Variable IDs: `[v###]`
- **Format**: `[v0]`, `[v1]`, `[v42]`, etc.
- **Usage**: Identifies local variable declarations
- **Example**: `let aid1[v0]: UUID[T20#20]`

#### Parameter IDs: `[p###]`
- **Format**: `[p0]`, `[p73]`, `[p82]`, etc.
- **Usage**: Identifies function/operator parameter declarations
- **Example**: `function map[f31](list[p59]: List<T>[t5], ...)`

#### Constant IDs: `[c###]`
- **Format**: `[c0]`, `[c1]`, etc.
- **Usage**: Identifies constant declarations
- **Example**: `const MAX_SIZE[c0]: int[t0] = 100[e42][t0];`

#### Table IDs: `[tab###]`
- **Format**: `[tab0]`, `[tab3]`, etc.
- **Usage**: Identifies table declarations
- **Example**: `table Graph[tab0] { ... }`

#### Generic Parameter IDs: `[g###]`
- **Format**: `[g0]`, `[g3]`, etc.
- **Usage**: Identifies generic type parameters
- **Example**: `type List[t5]<T[g0]>;`

#### Field IDs: `[field###]`
- **Format**: `[field0]`, `[field3]`, etc.
- **Usage**: Identifies table field declarations
- **Example**: `primary UID1[field0]: int[?];`

### Expression IDs: `[e###]`
- **Format**: `[e0]`, `[e42]`, `[e132]`, etc.
- **Usage**: Identifies every expression in the AST
- **Example**: `uid1[e10][t0][p73]` - expression e10 with type t0 resolving to parameter p73

## Type Resolution Format

### Primitive Types
- **Format**: `[t###]` where ### is the type ID
- **Examples**: 
  - `[t0]` = int
  - `[t1]` = bool
  - `[t2]` = string
  - `[t3]` = float

### Generic Types
- **Format**: `[t###<...>]` for parameterized types
- **Examples**: 
  - `[t5<t0>]` = List<int>
  - `[t6<T4#4>]` = Row<T> where T is type variable T4#4

### Table Types  
- **Format**: `[tab###]` for resolved table references
- **Example**: `Graph[e9][?][tab0][tab0]` - table expression with resolved table tab0

### Type Variables
- **Format**: `[T###]` for unresolved type variables
- **Example**: `[T20#20]` - type variable T with ID 20

### Unresolved Types
- **Format**: `[?]` for types that couldn't be resolved
- **Example**: Table fields like `Counter[field3]: int[?];`

## Name Resolution Format

Name resolution shows what identifiers refer to:

- **`[p###]`**: Resolves to parameter ### 
- **`[v###]`**: Resolves to variable ###
- **`[f###]`**: Resolves to function ###
- **`[tab###]`**: Resolves to table ###

## Complete Expression Format

Every expression follows the pattern: `expression[expr_id][resolved_type][name_resolution...]`

### Examples

#### Simple Identifier
```
uid1[e10][t0][p73]
```
- `e10`: Expression ID 10
- `t0`: Resolved to type int (t0)  
- `p73`: Resolves to parameter 73

#### Function Call
```
genUUID[e5][T21#21][f38][f38]()
```
- `e5`: Expression ID 5
- `T21#21`: Returns type variable T21  
- `[f38][f38]`: Calls function f38

#### Binary Operation
```
uid1[e29][?][p73] + [f0, f5, f30]" befriend with "[e31][?]
```
- `+` operator resolves to multiple overloads: `[f0, f5, f30]`
- Left operand `uid1[e29][?][p73]`: expression e29, unknown type, parameter p73
- Right operand `" befriend with "[e31][?]`: expression e31, unknown type

#### Table Access
```
Graph[e9][?][tab0][tab0][UID1: uid1[e10][t0][p73], UID2: uid2[e11][t0][p74]][e12][tab0]
```
- `Graph[e9][?][tab0][tab0]`: Table access, expression e9, resolves to table tab0
- Key-value pairs with their own expression IDs
- Final `[e12][tab0]`: The row access expression e12 returning table tab0 type

## Function Declaration Format

```
transaction beFriend[f41](uid1[p73]: int[t0], uid2[p74]: int[t0], time[p75]: int[t0])
```

- `beFriend[f41]`: Function name with ID f41
- `uid1[p73]: int[t0]`: Parameter uid1 with ID p73, type int (t0)

## Variable Declaration Format

```
let aid1[v0]: UUID[T20#20] = genUUID[e5][T21#21][f38][f38]()[e6][T22#22];
```

- `aid1[v0]`: Variable aid1 with ID v0  
- `: UUID[T20#20]`: Declared type UUID, resolved to type variable T20#20
- `= expression`: Initialization expression with full expression annotation

## Table Declaration Format

```
table Graph[tab0] {
  primary UID1[field0]: int[?];
  node f(UID1[e0][?][tab0]);
}
```

- `Graph[tab0]`: Table name with ID tab0
- `UID1[field0]: int[?]`: Field UID1 with ID field0, type int (unresolved)
- `UID1[e0][?][tab0]`: Node function parameter, expression e0, resolves to table tab0

## Reading Tips

1. **Follow the IDs**: Each `[###]` tells you exactly which AST node something refers to
2. **Type tracing**: Track how types flow through expressions via `[t###]` and type variables
3. **Resolution chains**: See how identifiers resolve by following `[p###]`, `[v###]`, etc.
4. **Expression hierarchy**: Every sub-expression has its own ID, building up complex expressions
5. **Operator overloading**: Binary operators show all possible function resolutions: `[f0, f5, f30]`

## Debugging Use Cases

- **Type inference**: Follow `[T###]` type variables through unification
- **Name resolution**: Trace identifier bindings via resolution decorators  
- **Expression structure**: Understand AST shape through expression IDs
- **Function dispatch**: See which functions/operators are candidates for calls
- **Table schema**: Verify field types and table structure resolution

This systematic format provides complete visibility into the compiler's internal state, making debugging and development much more tractable.