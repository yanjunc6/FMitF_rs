# IC3 Execution Protocol & What We Must Know for Commutativity (P-2)

This note distils the key points from the IC3 paper section pasted in the
conversation and explains **exactly** which static facts (data-flow
results) the verifier needs when it proves deep-dive commutativity
between two hop slices.

---

## 1  IC3 in a Nutshell

### 1.1  Pieces, Transactions, Serialisation Graph
* A *transaction* **T** is split into *pieces* `p₁, p₂, …`  
  – in FMitF a piece is precisely a `hop { … }`.
* The global serial-order is represented by an **SC-graph** whose  
  – **S-edges** follow the program order inside one transaction,  
  – **C-edges** connect two pieces that access at least one common
    record where at least one access is a write.

### 1.2  Two Runtime Data Structures
| Name | Where stored | Purpose |
|------|--------------|---------|
| `T.depQueue` | per transaction | dynamic list of transactions that **T** must not overtake. |
| `DB[r].accList` | per record `r` | all *uncommitted* transactions that have read or written `r` (“accessor list”). |

### 1.3  Algorithm 1  – `RunPiece(p, T)`
Three **phases**:

1. **Wait**  
   Decide how long **T** must wait before executing piece `p`.  
   IC3 distinguishes three static cases using the SC-graph:  
   1. `p` has **no C-edges** at all → execute immediately.  
   2. `p` has C-edges, but each targets **exactly one** piece `p′`
      in every transaction `T′` found in `T.depQueue`  
      → wait only for those `p′` to finish.  
   3. otherwise → wait until each `T′` in `T.depQueue` commits.
2. **Execute**  
   *Reads* return the **stashed** value (the value written by the *last
   committed piece*, not necessarily the last committed transaction).
   *Writes* are buffered locally.
3. **Commit (of the piece, not the transaction)**  
   Classic optimistic-CC validation: lock read/write sets, re-validate
   reads, then  
   * update `T.depQueue` with the **last** writer found in each
     accessed record’s `accList`,
   * insert `T` (with “reader” / “writer” tag) into every `accList`,
   * update the record’s `stash` with the piece’s buffered write,
   * release the locks.

### 1.4  Algorithm 2 – `RunTransaction(T)`
After all pieces finished successfully:

1. Wait for **every transaction** in `T.depQueue` to commit.
2. Materialise all buffered writes into the real DB values.
3. Remove `T` from each touched `accList`.

Locks are **not** held across the whole write-set because the explicit
waiting above prevents overtaking.

### 1.5  Why Waiting Only for `p′` Can Be Safe
If the only SC-cycle involving `p` and a transaction `T′` is the edge
`p ↔ p′`, ensuring `p′` precedes `p` is sufficient; transitive cycles
that also involve other transactions are automatically respected (see
paper § 3.5).

---

## 2  Implications for P-2 (Commutativity of Hop Slices)

When we check that two hop slices commute, we must emulate the *allowed*
runtime schedules that IC3 would permit:

* **Slices**  
  Each slice is a *contiguous* sequence of hops from one transaction
  (e.g. `A₂ → A₃`).
* **Interleavings**  
  We interleave the two slices such that
  1. **intra-slice order** is preserved, and
  2. no hop executes earlier than IC3’s **Wait Phase** would allow
     w.r.t. the dynamic `depQueue` rules.

Observation:  
In practice, when both slices are from different transactions, the only
forbidden interleavings are those that would schedule two
**piece-level C-edges** in the wrong order **and** the later hop has the
earlier hop’s transaction in its `depQueue`.  If both slices belong to
the same transaction, *all* interleavings are already prohibited by
S-edges, hence we do not generate them.

For the initial version of the verifier we adopt a **simpler rule**:  
• generate **all** order-preserving shuffles and rely on IC3’s wait
logic encoded in the Boogie harness to discard the impossible ones.
Deadlocks do not appear because we do **not** merge SC-cycles in
analysis (the paper’s “deadlock-prone cycle merging” can be added later
as an optimisation).

---

## 3  Static Facts Required from Data-Flow Analysis

| Fact | Used for | How to compute |
|------|----------|----------------|
| **Read-set / Write-set** of every hop | piece-validation; accessor-list emulation; Boogie state difference. | Already collected during CFG construction. |
| **Live-in variables** and **live-out variables** of any *hop sequence* (slice) | To know which variables must be `havoc`ed and compared. | Classic backwards liveness analysis extended over hop boundaries. |
| **Which hop pairs share a C-edge** | Decide if two hops belong to Case-1, 2, 3 in Wait Phase. | Available from SC-graph construction. |
| **Transaction → Hop order (S-edges)** | Enforce intra-slice order; prevent illegal interleavings. | Given by program order. |
| *(Optional)* “Deadlock-prone cycle” detection | Could prune interleavings that IC3 would merge; not needed initially. | Tarjan-style SCC over the SC-graph. |

All other runtime bookkeeping (`depQueue`, `accList`, stash values, read
validation) is **modelled** inside the Boogie harness; no extra static
information is required.

---

## 4  Boogie Harness Template for One Interleaving π

```
procedure CheckSliceComm_pi()
{
  // 1. Havoc everything the slices read
  havoc Tables, LiveIns;

  // 2. Execute interleaving π under IC3-piece semantics
  call Hop( … );   // generated inline code honours Wait-Phase rules
  ……

  var DB1  := Tables;
  var Out1 := ⟨live-outs⟩;

  havoc Tables, LiveIns;

  // 3. Execute mirror interleaving πʳ (swap whole slices)
  ……

  assert DB1  == Tables;
  assert Out1 == ⟨live-outs⟩;
}
```

A failed assertion reflects a genuine schedule that violates
commutativity under IC3.

---

### End of file
