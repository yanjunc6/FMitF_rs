# Formal Verification Plan for **FMitF**

_August 2025_

---

## 1  Scope and Objectives

This note defines the **verification layer** that will be added on top of the
current FMitF compilation pipeline.  
Two independent properties shall be proved:

1. **P-1   Correct Node Placement** – Every `hop { … }` is guaranteed to run
   on _exactly one_ physical node, determined solely by the declared
   `partition` functions.
2. **P-2   Slice Commutativity** – For every pair of hop slices  
   `Aᵢ → … → Aⱼ` and `Bₖ → … → Bₗ` belonging to (possibly different)
   transactions, all run-time interleavings permitted by the **IC3**
   protocol yield **identical final database state** and indistinguishable
   observable outputs.

Together these properties imply _conflict-serialisability_ for the chopped
transactions produced by the FMitF compiler.



## 2  Property P-1 – Correct Use of Partition Functions

### 2.1  Problem Statement  

Inside every hop the compiler inserts remote-execution commands:

```
EXECUTE_AT node_selector(e₁, e₂, …) {
    // hop body
}
```

The programmer must guarantee that calls to the same `partition`
expression with identical arguments denote the same physical node
throughout the entire program.

_Informally_: if two expressions evaluate to the same tuple of argument
values at run time, they must designate the same node.

### 2.2  Static Proof Strategy  

1. **CFG Simulation**  
   • Traverse the SSA-form CFG of each hop.  
   • Simulate the cfg in Boogie and insert the partition argument comparison.

2. **Partition Call Catalogue**  
   • Whenever the traversal reaches *table access* record the
     tuple `(hop-id, pf, 〈a, b, …〉)` where `pf` is the partition function.
   • Find the first tuple recorded as `(hop-id, pf, 〈a', b', …〉)`, feed the following _verification condition_ into Boogie/Z3:
```
assert (a == a' and b == b' and ...)
```
      If such tuple is not found, skip.


## 3  Property P-2 – Commutativity of Hop Slices

### 3.1  Execution Model Recap (IC3)

A transaction _T_ consists of **pieces** (`hop`s).  At runtime IC3
enforces: TODO: describe the IC3 quickly.

* **S-edges**: program order inside one transaction instance.
* **C-edges**: conflicts between pieces of different transactions.
* **depQueue** (per T): list of transactions that must commit before T.
* **accList** (per record r): all live transactions that touched r.

### 3.2  What “Commutativity” Means Here

Let **S₁ = Aᵢ · … · Aⱼ** and **S₂ = Bₖ · … · Bₗ** be two _contiguous_
sequences of hops (“slices”).
We must show:

```
DB₀, σ₀  ──[ any IC3-allowed interleaving of S₁ ▷ S₂ ]──▶  DB₁, σ₁
DB₀, σ₀  ──[ same interleaving but with S₂ ▷ S₁ ]──────▶  DB₂, σ₂

           ⇒   (DB₁ = DB₂) ∧ (σ₁ = σ₂)
```

where `σ` collects all user-observable local variables that are live at that point.

### 3.3  Generating the Interleavings

IC3 sometimes _waits_ for particular pieces. We need to have an algorithm to describe this process. TODO

### 3.4  Static Facts Required

| Fact | Origin | Already available |
|------|--------|-------------------|
| Read- & write-set of each hop | CFG construction | **Yes** |
| Live-in / live-out vars of any slice | Data-flow pass | **Add** |
| List of hop pairs with C-edge | SC-graph | **Yes** |
| S-edge ordering inside a transaction | CFG | **Yes** |

### 3.5  Boogie Harness Template

```boogie
procedure Check_SliceCommut_pi()
{
  // 1. Havoc all external state
  havoc DB0;
  havoc LiveIns0;
  DB = DB0;
  LiveIns = LiveIns0;

  // 2. Execute interleaving  π  under IC3 semantics
  // Hop_A_i generated inlined code
  ...
  // Hop_B_l generated inlined code

  var DB1  := DB;
  var Out1 := LiveOuts;

  // 3. Reset and run mirror interleaving  πᴿ
  DB = DB0;
  LiveIns = LiveIns0;

  ...
  
  assert DB1  == DB;
  assert Out1 == LiveOuts;
}
```

A failing assertion signals a genuine serialisation anomaly.



## 4  Compiler Integration

```
AST  →  CFG  →  (optimise)  →  SC-graph
                    │
                    ▼
            Data-flow Analysis
                    │
                    ▼
           Verification Artifact
           (Boogie + SMT-LIB)
```

* **Verification Backend** – lives in `src/verify/`.
* **Harness Generator** – attaches to `Compiler::finish()`.
* **CLI Flag** – `--verify [p1|p2|all]` activates proof emission.  
  Verification runs _after_ SC-graph construction.

## 5 Hop level simulation

TODO: we need a pratical part to describe simulation of hops. It should be a one to one translation between the cfg to boogie IVL.
