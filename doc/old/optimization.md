# Summary of Classic Data-Flow Analyses and the Optimizations that Use Them  

Please note that the cfg/mod.rs doesn't produce SSA but normal IR. If any place says it is SSA, delete this, stop working on the task, start check whether anything is wrong.

## 1. Data-Flow Analyses

| Analysis | Direction | Typical Transfer Function / Lattice Element | Example (in **bold** = value inside the lattice element) |
|----------|-----------|---------------------------------------------|----------------------------------------------------------|
| **Live-Variable Analysis (Liveness)** | **Backward** (information flows from successors to predecessors) | • Set of variables that may be used before they are re-defined.<br>•  operator = **union**. | After executing `y = x + 1` the LV set is **{x, …}** because `x` is used afterwards; `y` is *not* live until it is read. |
| **Reaching Definitions (RD)** | **Forward** | • Set of (variable, definition-site) pairs that reach a program point without being killed.<br>• operator = **union**. | At entry of block B: **{(a,1), (b,3)}** says the defs at stmt 1 and 3 may reach B. |
| **Available Expressions (AE)** | **Forward** | • Set of side-effect-free expressions whose current value is guaranteed to be available (i.e., previously computed and none of their operands have since been modified).<br>• operator = **intersection**. | Before `z = x+y`, set might contain **{x+y, p*q}** meaning `x+y` can be reused. |
| **Constant-Value Analysis** | **Forward** | • Map `Var → {const c, ⊤ (unknown), ⊥ (undefined)}` (aka *constant propagation lattice*).<br>• operator = **greatest lower bound** (e.g., two different constants → ⊤). | After `x = 3; if (…) … else … ;` we might have **x ↦ 3**, `y ↦ ⊤`. |
| **Copy Lattice (Copy Propagation Analysis)** | **Forward** | • Map `Var → { Var', ⊤, ⊥ }` where `v ↦ w` means “`v` definitely holds the same value as `w`”.<br>• operator = **glb** (disagreeing copies → ⊤). | After `b = a; c = b;`, map is **{b ↦ a, c ↦ a, …}**. |

---

## 2. Optimizations

| Optimization | Dependent Analysis | When to Apply | How / What Gets Changed |
|--------------|-------------------|---------------|-------------------------|
| **Dead-Code Elimination (DCE)** | Live-Variable Analysis | After code generation of SSA or three-address code, but before register allocation. | • Scan statements **backwards**; remove assignment `x = …` if `x` is *not live* afterwards and the RHS has no side effects.<br>• May be iterated until no more code is removed. |
| **Copy Propagation** | Copy Lattice (or simple RD restricted to copies) | Typically right after Copy analysis and before constant propagation / CSE. | • For each use of `v` where lattice says `v ↦ w`, replace `v` with `w`.<br>• Delete redundant assignments if they become dead (triggering another DCE pass). |
| **Constant Propagation** | Constant-Value Analysis | Early and frequently; combine with folding in iterative optimization passes. | • Replace each variable use whose value is a definite constant c with literal `c`.<br>• May enable dead-branch elimination and folding. |
| **Constant Folding** | Constant Propagation (or direct constant evaluation) | Directly after constants are exposed, during IR construction or in a cleanup phase. | • Evaluate operations whose operands are all constants (e.g., `5*0 → 0`).<br>• Replace compare/jump on known outcome to simplify CFG. |
| **Common Sub-expression Elimination (CSE)** | Available Expressions | Usually before register allocation and after propagation passes (so more expressions match). | • If expression `e` is **available** at point P and will be recomputed, reuse the earlier computed value (introduce a temporary if needed).<br>• Must ensure operands are side-effect-free and unmodified. |

---

### Recommended Pass Order (Typical)

1. Constant & Copy propagation + folding (iterative)  
2. CSE  
3. Dead-Code Elimination (may run between each phase) 

This order is not mandatory but maximizes the synergy among the listed optimizations.
