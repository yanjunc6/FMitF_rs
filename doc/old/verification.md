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
   • Traverse each hop’s form CFG.  
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
*(concise, self-contained recipe)*

The same printer emits a `.bpl` file for both proofs:

• P-1: single hop, **plain** variable names  
• P-2: two hop sequences, variables prefixed **a_ / b_**

A flag in `PrinterConfig { var_mode: VarMode }` selects the style; all
other logic is identical.

--------------------------------------------------------------------
### 5.1  Front-end information needed

1. `HopCfg` – basic blocks + successors (execution order already given).  
2. `VarInfo` – original name & type for every `VarId`.  
3. `TableInfo` – ordered primary-key list, data fields.  
4. *Read/write set* per hop, *live-in / live-out* per slice.

No optimiser or SC-graph internals are used while printing.

--------------------------------------------------------------------
### 5.2  Variable naming

```
Plain      →  balance
PrefixedA  →  a_balance
PrefixedB  →  b_balance
```

The renaming happens during **emission**; the CFG is kept unchanged.

--------------------------------------------------------------------
### 5.3  Table representation

Each **data field** becomes one nested map.  Depth = number of
primary keys (`k`).

| k | Boogie type & declaration | Read | Write |
|---|---------------------------|------|-------|
| 1 | `type T_f = [int]int;`<br>`var  T_f : T_f;` | `v := T_f[pk1];` | `T_f := T_f[pk1 := v];` |
| 2 | `type T_f = [int][int]int;` | `v := T_f[pk1][pk2];` | `T_f[pk1] := T_f[pk1][pk2 := v];` |
| 3 | `type T_f = [int][int][int]int;` | `v := T_f[p1][p2][p3];` | `T_f[p1][p2] := T_f[p1][p2][p3 := v];` |
| ≥4 | nest further in the obvious pattern |

_No_ stash/buffer arrays: the map itself is the current DB state.

--------------------------------------------------------------------
### 5.4  CFG → Boogie cheat-sheet

| CFG element | Boogie emitted |
|-------------|----------------|
| `x = y` | `x := y;` |
| `x = a + b` | `x := a + b;` |
| `array[i] = v` | `array := array[i := v];` |
| `T[pk].f = v` | use rule in §5.3 |
| `tmp = T[pk].f` | use rule in §5.3 |
| `Unconditional` | `goto BB_next;` |
| `ConditionalTrue{c}` | `if (c) { goto BB_t; } else { goto BB_f; }` |
| `Return` | `goto BB_RET;` |
| `Abort` | `goto BB_RET;`  // later asserts skipped |
| `HopExit{next}` | `goto ENTRY_next;` |

--------------------------------------------------------------------
### 5.5  Printer skeleton

```rust
fn emit_bpl(cfg:&CfgProgram,
            hops_a:&[HopId],
            hops_b:Option<&[HopId]>) -> String {
    let mut out = BoogieBuilder::new();

    DeclareVisitor { out:&mut out }.visit_program(cfg);      // global decls

    for h in cfg.hops.indices() {                            // hop bodies
        HopVisitor { out:&mut out, mode:VarMode::Plain }
            .visit_hop(cfg, h);
    }

    HarnessWriter { out:&mut out, hops_a, hops_b }.write(cfg); // P-1 or P-2
    out.finish()
}
```

`HopVisitor` prints labels `BB_<hop>_<idx>:` and translates every
statement via the cheat-sheet above.

### 5.6  Generating Every “Legal” Merge of Two Hop Lists  
(no database vocabulary – just list shuffling with one simple rule)

We have

```
sliceA = [a1, a2, …, am]     // all hops of transaction A
sliceB = [b1, b2, …, bn]     // all hops of transaction B
```

and one extra input set

```
C = { (ai , bj) }            // “conflict” pairs taken from the C-edges
                             // ‑ always between a hop of A and a hop of B
```

The compiler guarantees S-edges **inside** each slice, so the only
constraints we must enforce are:

 • keep the original order inside each slice,  
 • when two conflicting hops appear, the first one that is executed
   fixes a **global direction** (A-before-B _or_ B-before-A) that every
   later conflicting pair must respect.

Example  
```
sliceA = [a , b]      sliceB = [c , d]
C = { (a , c) , (b , d) }
```
Legal orders: `a b c d`, `a c b d`, `c d a b`, `c a d b` …  
Illegal: `a b d c` (first conflict gives A→B, later d c would flip).

------------------------------------------------------------------
High-level idea
------------------------------------------------------------------
While we merge the two lists, we remember **one flag**

```
dir = Unknown | ABeforeB | BBeforeA
```

• `Unknown` – we have not placed any conflicting pair yet.  
• `ABeforeB` – at least one (ai … bj) was seen, so **all** future
  conflicts must keep A before B.  
• `BBeforeA` – the opposite.

If placing the next hop would contradict the flag, we drop that branch
of the search immediately.

------------------------------------------------------------------
Detailed pseudocode
------------------------------------------------------------------

```pseudocode
enum Direction { Unknown, ABeforeB, BBeforeA }

function buildConflictSet(C_edges):
    // returns a fast   isConflict[x][y]   lookup
    table := empty  // e.g., HashSet of 64-bit keys
    for (u,v) in C_edges:
        table.add( (u,v) )
        table.add( (v,u) )   // store both directions for O(1) test
    return table


procedure generateAllMerges(sliceA, sliceB, C_edges):
    conflicts := buildConflictSet(C_edges)
    m := length(sliceA)
    n := length(sliceB)
    result := []

    // depth-first helper
    procedure DFS(i, j, placed, dir):
        // i = next index in A,  j = next index in B
        if i == m and j == n:
            result.append(copy(placed))
            return

        // ---------------------------------------------------
        // local function: try to append hop h (from A? = isA)
        // ---------------------------------------------------
        procedure tryAppend(hop h, bool isA, int i2, int j2):
            newDir := dir
            // scan placed from right to left until we meet a hop
            // of the *other* transaction that conflicts with h
            for k from length(placed)-1 down to 0:
                p := placed[k]
                if isA != isFromA(p):              // opposite Tx
                    if (h,p) in conflicts:         // they conflict
                        if dir == Unknown:
                            newDir := if isA then ABeforeB else BBeforeA
                        else if (dir == ABeforeB and not isA) or
                                (dir == BBeforeA and isA):
                            return            // violates direction
                        break                 // first conflicting pred found
            placed.append(h)
            DFS(i2, j2, placed, newDir)
            placed.pop()

        // Option 1 – take next hop from A
        if i < m:
            tryAppend(sliceA[i], true, i+1, j)

        // Option 2 – take next hop from B
        if j < n:
            tryAppend(sliceB[j], false, i, j+1)

    // kick off the recursion
    DFS(0, 0, [], Unknown)
    return result
```

Helper `isFromA(h)` is `true` if `h` belongs to `sliceA`.

------------------------------------------------------------------
Why this works
------------------------------------------------------------------
• The first time we see a conflicting pair the flag switches from
  `Unknown` to the chosen global order.  
• From that point on the inner test
  ```
  (dir == ABeforeB and not isA)
  ```
  blocks any hop that would flip the direction, so the search never
  explores illegal schedules.  
• All other branches (no conflicts or same direction) proceed normally
  and eventually reach the base-case `i==m && j==n`, adding exactly the
  legal merges to `result`.

------------------------------------------------------------------
Complexity
------------------------------------------------------------------
Worst case (no conflicts) enumerates the full binomial number of merges
`C(m+n, m)`.  Every conflict that appears early halves the live branch
count, so practical slices stay small.

------------------------------------------------------------------
Output
------------------------------------------------------------------
`generateAllMerges` returns a list of vectors; each vector is one legal
interleaving.  The verification harness will loop over that list and
feed every vector to Boogie.


--------------------------------------------------------------------
### 5.7  Abort handling

A ghost flag `aborted` is raised when control reaches an `Abort` edge.
All equality assertions in the harness are guarded with  
`if (!aborted)`; thus any run that aborts vacuously satisfies the proof.

--------------------------------------------------------------------
With these rules a developer only needs to ❶ walk the CFG by the visitor
API, ❷ apply the table and cheat-sheet templates, and ❸ let
`HarnessWriter` insert the correct assertions.  The produced `.bpl`
passes Boogie unmodified.
