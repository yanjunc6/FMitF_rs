# TPCC Benchmark Bug Report

This document tracks all bugs found in the TPCC benchmark compilation and their fixes.

## Bug Categories Identified

Based on analysis of `tmp/compilation.log`, the following error categories were found:

1. **Type Mixing Errors** - `invalid argument types (real and int) to binary operator` (4077 occurrences)
2. **Missing Modifies Clauses** - `command assigns to a global variable that is not in the enclosing procedure's modifies clause` (1596 occurrences)  
3. **Duplicate Block Names** - `more than one declaration of block name` (2 occurrences)

## Individual Bug Fixes

---

### **BUG #1: Type Mixing Between Real and Int in Arithmetic Operations**

**Error Pattern**: 
```
Error: invalid argument types (real and int) to binary operator +
Error: invalid argument types (real and int) to binary operator *
```

**Occurrences**: 4077 total errors in TPCC benchmark

**Root Cause**: 
The Boogie code generator was creating binary operations between `real` (float) and `int` types without explicit type conversion. In Boogie, unlike some languages, integers are not automatically promoted to reals in arithmetic operations.

**Example**:
- Source: `TestTable[id: table_id].amount = item_price * qty;` where `item_price` is `float` and `qty` is `int`
- Generated (wrong): `_t4 := param_item_price * param_qty;` // real * int - ERROR!
- Generated (fixed): `_t4 := param_item_price * (int2real(param_qty));` // real * real - OK!

**Minimal Test Case**: [`bug1_type_mixing.transact`](./bug1_type_mixing.transact)

**Fix Applied**: 
- **File**: `src/verification/Boogie/gen_Boogie.rs`
- **Change**: Added automatic type conversion for arithmetic operations (Add, Sub, Mul)
- **Method**: Added `int2real()` function declaration and modified binary operation conversion to detect mixed types
- **Logic**: When `int` and `float` types are mixed in arithmetic, automatically convert the `int` operand using `int2real()`

**Status**: ✅ **FIXED** - Tested with minimal case, no more type errors

---

### **BUG #2: Missing Modifies Clauses in Commutativity Verification**

**Error Pattern**:
```
Error: command assigns to a global variable that is not in the enclosing procedure's modifies clause: District_D_TAX
Error: command assigns to a global variable that is not in the enclosing procedure's modifies clause: Item_I_PRICE
```

**Occurrences**: 1596 total errors in TPCC benchmark

**Root Cause**:
The commutativity verification procedures had incomplete modifies clauses. The modifies clause only included variables that were *written* to by the transaction logic, but the Boogie code also performs `havoc` operations on variables that are only *read*. Since `havoc` is an assignment in Boogie, all havoc'd variables must be in the modifies clause.

**Analysis**:
- Modifies clause: `modifies A, B;` (only written variables)
- Havoc operations: `havoc A; havoc B; havoc C; havoc D;` (read + written variables)
- Error: C and D are assigned (havoc'd) but not in modifies clause

**Fix Applied**:
- **File**: `src/verification/commutative/mod.rs`
- **Change**: Updated modifies clause generation to include ALL havoc'd variables
- **Before**: `modifies.extend(tables_written_a + tables_written_b)`
- **After**: `modifies.extend(tables_read_a + tables_written_a + tables_read_b + tables_written_b)`

**Status**: ✅ **FIXED** - All 1,596 modifies clause errors eliminated

---

### **BUG #1.2: Map Store Type Conversion**

**Error Pattern**:
```
Error: right-hand side in map store with wrong type: real (expected: int)
```

**Occurrences**: 60 total errors remaining after Bug #1 fix

**Root Cause**:
Bug #1 fix introduced int2real() conversions for arithmetic operations, but the results are real values that get stored into int-typed maps. The conversion works for arithmetic but creates type mismatches for storage.

**Example**:
```boogie
A__t764 := A__t761 * (int2real(A__t755));  // Result is real
Order_Line_OL_AMOUNT[...] := A__t764;      // Storing real into int map - ERROR!
```

### **BUG #1.2: Map Store Type Conversion**

**Error Pattern**:
```
Error: right-hand side in map store with wrong type: real (expected: int)
```

**Occurrences**: 60 total errors remaining after Bug #1 fix

**Root Cause**:
Bug #1 fix introduced int2real() conversions for arithmetic operations, but the results are real values that get stored into int-typed maps. The conversion works for arithmetic but creates type mismatches for storage.

**Example**:
```boogie
A__t764 := A__t761 * (int2real(A__t755));  // Result is real
Order_Line_OL_AMOUNT[...] := A__t764;      // Storing real into int map - ERROR!
```

**Fix Applied**:
- **File**: `src/verification/Boogie/gen_Boogie.rs`
- **Change**: Added conditional real2int conversion for int-typed table field assignments
- **Logic**: Check target field type in `convert_assignment()` and wrap values with `real2int()` when storing into int fields
- **Method**: Added `rvalue_might_be_real()` helper to detect when conversion is needed

**Status**: ✅ **FIXED** - All 60 map store type errors eliminated

---

### **BUG #3: Duplicate Block Labels**

**Error Pattern**:
```
Error: more than one declaration of block name: block99
Error: more than one declaration of block name: block6
```

**Occurrences**: 121 total errors

**Root Cause**: 
Basic block label generation creates duplicate names across different procedure contexts in partition verification.

**Analysis**:
The issue occurs in partition verification procedures where the same basic block numbers are reused across different scopes, causing name collisions in the generated Boogie code.

**Status**: � **IN PROGRESS** - Need to add unique prefixes to block labels

---

## **COMPREHENSIVE TYPE SYSTEM IMPROVEMENT NEEDED**

**Fundamental Issue**: 
The current CFG lacks proper type information, requiring complex type inference at Boogie generation time. This leads to fragile solutions for type conversion.

**Proposed Solution**:
1. **Enhanced CFG Type System**: Add type information to all CFG nodes (constants, variables, binary operations)
2. **CFG-Specific Operators**: Create type-aware binary/unary operators instead of reusing AST operators
3. **Type-Driven Code Generation**: Use CFG type information to generate correct Boogie operators and conversions

**Implementation Plan**:
1. Modify `cfg/mod.rs` to add type fields to CFG structures
2. Update `cfg_builder.rs` to propagate type information from AST
3. Create CFG-specific operator enums with type constraints
4. Simplify Boogie generation using CFG type information

**Files to Modify**:
- `src/cfg/mod.rs` - Add type fields to CFG structures
- `src/cfg/cfg_builder.rs` - Type propagation logic
- `src/verification/Boogie/gen_Boogie.rs` - Simplified type-driven code generation
