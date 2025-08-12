# Formal Verification Plan for **FMitF**

_August 2025_

---

## 1  Scope and Objectives

This document specifies the **verification layer** that will be added to
the FMitF compilation pipeline.  
Two independent properties shall be proved:

1. **P-1  Correct Node Placement** – every `hop { … }` is guaranteed to
   run on _exactly one_ physical node, determined solely by the declared
   `partition` functions.
2. **P-2  Slice Commutativity** – for every pair of hop slices  
   `Aᵢ → … → Aⱼ` and `Bₖ → … → Bₗ` belonging to (possibly different)
   transactions, **all** run-time interleavings permitted by the **IC3**
   protocol yield **identical final database state** and indistinguish­
   able observable outputs.

Together the two properties entail *conflict-serialisability* for the
chopped transactions produced by the FMitF compiler.



## 2  Property P-1 – Correct Use of Partition Functions

### 2.1  Problem Statement  

Inside every hop the compiler ultimately emits a remote-execution
primitive of the form

```
EXECUTE_AT node_selector(e₁, e₂, …) { /* hop body */ }
```

The **safety obligation** is:

> If two calls to `node_selector` (or any other partition function) are
> evaluated with the same tuple of argument values at run time, they must
> select the same physical node.

### 2.2  Static Proof Strategy  

1. **CFG Simulation**  
   • Traverse each hop’s SSA-form CFG.  
   • Translate every basic block into _pure Boogie_ code that **exactly
     mirrors** the semantics of the Rust CFG (see § 5).  
   • Record every occurrence of a *table access* whose placement is
     decided by a partition function `pf`.

2. **Partition-Call Catalogue**  
   • Each recorded access yields a triple  
     `(hop-id, pf, 〈arg₁, …, argₖ〉)`.  
   • Look up the *earliest* previously recorded triple with the _same_
     `(hop-id, pf)`; call its argument vector `〈arg₁′, …, argₖ′〉`.

3. **Verification Condition**  
   Insert, at the later program point, the Boogie assertion

   ```boogie
   assert !(arg₁ == arg₁' && … && argₖ == argₖ'); 
   ```

   which is equivalent to the implication  
   `(args are equal) ⇒ (no second, different node chosen)` because the
   earlier access has already fixed the node id.  
   If no earlier triple exists, nothing is asserted.

4. **Soundness Argument**  
   The assertion may fail **only** if the same arguments could evaluate
   to different nodes – exactly the bug we intend to exclude.



## 3  Property P-2 – Commutativity of Hop Slices

### 3.1  Execution Model Recap (IC3)

At run time FMitF adopts the **IC3** execution protocol:

* A transaction **T** is split into **pieces** – in FMitF one piece
  corresponds to one `hop`.
* The **SC-graph** summarises static dependencies  
  – **S-edges** respect program order inside a transaction;  
  – **C-edges** connect two hops that access at least one common record
    where at least one access is a write.

IC3 maintains two dynamic structures:

| Name | Storage | Purpose |
|------|---------|---------|
| `T.depQueue` | per transaction | list of transactions that **T** must not overtake |
| `DB[r].accList` | per record `r` | all _uncommitted_ transactions that have read/written `r` |

`RunPiece(hop, T)` proceeds in three phases:

1. **Wait** – decide how long **T** must wait before executing `hop`:  
   • _Case 1_: the hop has **no C-edges** → execute immediately.  
   • _Case 2_: all its C-edges lead to **exactly one hop** of every
     transaction in `T.depQueue` → wait only for those hops.  
   • _Case 3_: otherwise → wait until **all** transactions in
     `T.depQueue` commit.
2. **Execute** – reads obtain the *stash* value (last value written by
   any committed hop), writes are buffered locally.
3. **Piece-Commit** – validate reads, extend `depQueue`, update each
   record’s `accList` and `stash`.

`RunTransaction(T)` finally waits for every transaction in `T.depQueue`,
materialises the buffered writes and removes **T** from each `accList`.
No locks are held across the entire write-set; safety is obtained solely
through explicit waiting.


### 3.2  What “Commutativity” Means Here

Let  

* **S₁ = Aᵢ · … · Aⱼ**  
* **S₂ = Bₖ · … · Bₗ**

be two _contiguous_ hop sequences (“slices”).  **Commutativity** requires

```
DB₀, σ₀ ──[ any IC3-allowed interleaving of S₁ ▷ S₂ ]──▶ DB₁, σ₁
DB₀, σ₀ ──[ same interleaving but with S₂ ▷ S₁ ]──────▶ DB₂, σ₂
                     ⇒   DB₁ = DB₂  ∧  σ₁ = σ₂
```

where `σ` contains all user-observable variables that are **live-out**
after the slices.

### 3.3  Generating the Interleavings

We must enumerate precisely those schedules **IC3 may realise**:

```
Algorithm 1 – EnumerateIC3Interleavings(S₁, S₂)

input : two slices S₁, S₂   (length m, n)
output: list of schedules π₁ … πₖ

1  let Shuffles := all order-preserving shuffles of (m + n) elements
2  foreach π in Shuffles do
3      if  WaitPhaseConstraintsSatisfied(π)    // Fig. 2
4          add π to result
```

*Line 3* consults the SC-graph plus the dynamic rules in § 3.1: a hop may
appear in `π` **only** after every hop whose transaction is currently in
its `depQueue` and that matches *Case 2* or *Case 3* has already been
scheduled (no store-forwarding is modelled).  
If the check fails, `π` is unreachable at run time and is discarded.  
The remaining schedules are **complete** – we neither miss a legal
schedule nor include an illegal one.

In the *initial* verifier implementation we take the simpler but sound
shortcut from the IC3 paper:

> Generate **all** shuffles and trust the **Boogie harness** (which
> faithfully re-implements `RunPiece`) to rule out the impossible ones.

This trades performance for simplicity while still avoiding
false-negatives.

### 3.4  Static Facts Required

| Fact | Origin | Present? |
|------|--------|---------|
| read-/write-set of every hop | CFG | **Yes** |
| live-in / live-out vars of any slice | new data-flow pass | **Add** |
| hops sharing a C-edge | SC-graph | **Yes** |
| S-edge order | CFG | **Yes** |

### 3.5  Boogie Harness Template

```boogie
procedure Check_SliceCommut_pi()
{
  // --- 1. Snapshot initial world -----------------------------
  havoc DB0;                // abstract heap of all tables
  havoc LiveIns0;           // values of all live-in vars
  DB        := DB0;
  LiveIns   := LiveIns0;

  // --- 2. Execute interleaving  π  ---------------------------
  call Hop_A_i(); … call Hop_B_l();   // generated, inlined, IC3 semantics

  var DB1  := DB;
  var OUT1 := LiveOuts;     // collector for all live-out vars

  // --- 3. Reset and execute mirror  πᴿ -----------------------
  DB      := DB0;
  LiveIns := LiveIns0;

  …                           // same hops but with S₂ ▷ S₁ order

  assert DB1  == DB;          // identical heap
  assert OUT1 == LiveOuts;    // identical observable vars
}
```

A failing assertion indicates a *real* serialisation anomaly that IC3
would allow to occur at run time.



## 4  Compiler Integration

```
AST  →  CFG  →  (optimise)  →  SC-graph
                    │
                    ▼
            Data-flow Analysis
                    │
                    ▼
          Verification Artifacts
            (Boogie + SMT-LIB)
```

* **Backend location** `src/verify/`  
* **Harness generator** attached to `Compiler::finish()`  
* **CLI flag** `--verify {p1|p2|all}`  
  – verification starts *after* SC-graph construction and before any
  optional optimisation pass.


## 5  Hop-Level CFG → Boogie Translation
_Building a text file that Boogie can parse_

The goal is to turn every **HopCfg** into _one_ self-contained Boogie
procedure whose body is a **straight-line inlining** of all basic blocks
(BB), without any further procedure calls.  The whole algorithm fits in
≈300 LOC and can be implemented by walking the CFG with the **visitor
API** given in `cfg_api.rs`.

### 5.1  Five-Step Generation Pipeline

| Step | Visitor | Output | Note |
|------|---------|--------|------|
| 1 | `DeclareVisitor` (outer) | global `type` & `var` decls | run _once_ over `program` |
| 2 | `HopVisitor` (outer) | one Boogie `procedure` header per hop | collects BB list |
| 3 | `BlockVisitor` (outer) | labelled Boogie blocks | emits `LABEL_bbX:` |
| 4 | `StmtVisitor` (inner) | individual Boogie stmts | pure string emit |
| 5 | Post-pass | fall-through `goto` cleanup | dead-code pruning optional |

All visitors merely _append_ strings to an output buffer; there is **no
mutation** of the CFG.

### 5.2  Naming & Declarations

1. **Variables**  
   A fresh Boogie variable is emitted for _every_ `VarId`:

   ```
   // Boogie fragment
   var h12_balance: int;    // hop-id 12  —  original name “balance”
   ```

   Rule: `h<HopId>_<OrigName>`.  
   Thus two different hops never clash even if they use the same source
  -level name.

2. **Tables**  
   A table with _N_ primary keys is encoded as a **nested map** of depth
   `N`:

   ```
   // For  (k1, k2, k3)  →  row of type RowT
   type T_lvl3  = [int] RowT;
   type T_lvl2  = [int] T_lvl3;
   type T_lvl1  = [int] T_lvl2;
   var  T_stash: T_lvl1;     // IC3 “stash” value (last committed)
   var  T_buf  : T_lvl1;     // per-hop write buffer
   ```

   Updating `Account[id, branch]` therefore translates to

   ```boogie
   T_buf[id][branch] := RowT(balance := newBal, owner := owner);
   ```

3. **Program Counter**  
   Each basic block gets a label `BB_<HopId>_<Idx>` so we can emit
   structured `goto`s directly from the CFG edges.

### 5.3  Basic Block Translation Rules (cheat-sheet)

| CFG node | Boogie snippet |
|----------|----------------|
| `Assign { x = y }` | `h12_x := h12_y;` |
| `Assign { x = a + b }` | `h12_x := h12_a + h12_b;` |
| `Array[i] = v` | `arr := MapUpdate(arr, i, v);` |
| `TableField = v` | `T_buf[k1]…[kN].f := v;` |
| `Rvalue::TableAccess` | `tmp := T_stash[k1]…[kN].f;` |
| `EdgeType::Unconditional` | `goto BB_next;` |
| `EdgeType::ConditionalTrue{cond}` | `if (cond) { goto BB_t; } else { goto BB_f; }` |
| `EdgeType::Return` | `goto BB_RET;` |
| `EdgeType::Abort` | `assert false;` |
| `EdgeType::HopExit{next}` | `call PieceCommit(); goto BB_entry_nextHop;` |

### 5.4  Piece-Commit (inline macro)

Emit _once_ per hop, **not** a real procedure:

```boogie
PieceCommit:
  // validate & lock part – ghost, skip for now
  // flush buffered writes
  havoc k1, …, kN;                          // fresh loop indices
  assume true;                              // (models ∀)
  T_stash := MapMerge(T_stash, T_buf);      // overwrite touched rows
  T_buf   := T_buf_empty;
  return;
```

Because Boogie disallows higher-order updates, `MapMerge` is declared as
an **uninterpreted function**, making the translation simple yet sound.

### 5.5  Putting It All Together (pseudo-code)

```rust
fn generate_boogie(program: &CfgProgram) -> String {
    let mut out = String::new();

    // STEP 1 – global decls
    DeclareVisitor(&mut out).visit_program(program);

    for hop_id in each_hop(program) {
        // STEP 2 – procedure header
        write!(out, "procedure Hop_{}() {{\n", hop_id);

        // STEP 3/4 – body
        BlockVisitor(&mut out, hop_id).visit_hop(program, hop_id);

        write!(out, "}\n\n");
    }
    out
}
```

Implement `BlockVisitor` by calling `visit_basic_block()` on each BB in
`FunctionCfg::hop_order` and inside that, iterate over
`BasicBlock::statements`, sending every statement to `StmtVisitor`.

That’s all: run the generator, dump the resulting `.bpl` file next to
`scgraph.dot`, and feed it to Boogie from the CLI flag `--verify`.
