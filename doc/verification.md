# FMitF Verification – Requirements & Methodology
*(consolidated reference – August 2025)*

---

## 1.  Objectives

FMitF compiles a transactional language in which every **transaction**
is split into a sequence of `hop { … }` blocks executed on distinct
cluster nodes chosen by user-defined `partition` functions.

The compiler must *formally* guarantee one safety properties:

| ID | Property | Informal statement |
|----|-----------|-------------------|
| P-1 | Partition-soundness | All table accesses that occur inside a single `hop { … }` are routed to **the same cluster partition**. |

Another property is the one we want to prove for hops:
| ID | Property | Informal statement |
|----|-----------|-------------------|
| P-2 | Commutativity | Given two contiguous hop slices selected by the user, *every* order-preserving interleaving of the two slices is observationally equivalent to executing the entire first slice followed by the entire second slice, and vice-versa. |

All proofs are discharged automatically by translating the program plus
a verification harness to the Boogie IVL and feeding it to the Boogie
solver.

---

## 2.  Common Verification Technique

1. **Symbolic execution of hops**  
   • Reads, writes, return values and concrete table cells touched by a
     sequence of hops are collected in a *symbolic I/O summary*  
   • Tables are modelled as Boogie `map` variables.

2. **Havoc initial state**  
   Before every proof attempt, all live-in variables and all table maps
   are *havoced* (assigned arbitrary values) to ensure results hold in
   every reachable concrete state.

3. **Run, snapshot, compare**  
   • Execute one candidate execution order inlined into a Boogie
     procedure.  
   • Capture the resulting database maps and output variables in
     snapshot #1.  
   • Re-havoc the initial state, execute the *other* order, capture
     snapshot #2.  
   • `assert snapshot1 == snapshot2`.

---

## 3.  Property-specific Harnesses

### 3.1  Partition-soundness  (P-1)

For each individual hop **H**:

1. Havoc tables and hop parameters.  
2. Inline-execute **H**; simultaneously mark every key `k` that is used
   to access a table guarded by partition function `p`.  
3. Emit Boogie assertion  
   `forall k1, k2 :: Accessed(k1) && Accessed(k2) ==> p(k1) == p(k2)`

A counter-example from Boogie immediately identifies the hop, the first
two offending keys and their source locations.

---

### 3.2  Commutativity (P-2)

User supplies two contiguous hop slices:

Slice-A = A₂ → A₃ Slice-B = B₂


Allowed **interleavings** preserve intra-slice order, e.g.

1. `A₂ A₃ B₂`  
2. `A₂ B₂ A₃`  
3. `B₂ A₂ A₃`

For every interleaving `π` and its *mirror* `πʳ` (swap the two slices’
global order) we generate the same “run, snapshot, compare” harness as
in P-2.  All interleavings must satisfy the assertions.

---

## 4.  User Interaction & Reporting

Results:

* Colour-coded console summary (✔ / ✗ per pass).  
* For every Boogie obligation:  
  – `.bpl` file in `verify_out/`  
  – `.log` with Boogie’s stdout / counter-example model
* The executor could be part of verification module but all the file IO should be put in cli module.
* When a proof fails, the console shows the hop(s) and
  the first offending location, plus a readable interpretation of the
  model when available.

---
