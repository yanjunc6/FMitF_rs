# Go Codegen Guidelines for CFG → BoltDB Chains

This guide explains how to turn our optimized CFG (`cfg_opt_pretty.txt`) into idiomatic Go chain code that integrates with BoltDB. The snippets in `go_examples` are useful references, but the goal is to generate cleaner, safer code that follows the constraints of our CFG IR, leverages our static analyses, and respects Go best practices.

---

## Design Overview

- **Source of truth**: `src/cfg/mod.rs` defines the IR concepts (tables, types, instructions, hops). Use it as the contract for what must appear in Go.
- **Goal**: Emit Go that mirrors CFG control-flow and data dependencies, while guaranteeing type safety, predictable key encodings, and correct state transitions inside BoltDB transactions.
- **Key tools**: The CFG already provides table schemas, function signatures, hop order, and live-variable analysis. Use these facts to automate decisions instead of duplicating logic in Go.

---

## Table Representation

### Primary Key Structs

Every table must expose its primary key as a dedicated struct—even if the key is a single column. This ensures consistent serialization and keeps future composite keys easy to extend.

```go
// Warehouse table
// CFG: primary key (W_ID)
type WarehouseKey struct {
    W_ID uint64
}

type Warehouse struct {
    Key  WarehouseKey
    Tax  float32
    Ytd  float32
}
```

Map CFG table metadata to Go structs:

1. **Primary key fields** → `*Key` struct with exported fields (CamelCase names).
2. **Non-key fields** → table struct fields (also exported) that match CFG field names.
3. **Partition function** metadata → helper to compute the shard. Represent as pure function using the same arithmetic as the CFG `partition` function.

### Serialization

BoltDB stores byte slices, so always serialize keys and rows with `encoding/json`. Key structs avoid mistakes when composing composite keys.

```go
func marshalKey(key any) []byte {
    b, err := json.Marshal(key)
    if err != nil { panic(err) }
    return b
}
```

---

## Safe Access Helpers

Access functions must take typed keys, never raw parameter maps. This removes stringly-typed bugs and centralizes serialization.

```go
# Go Codegen Guidelines for CFG → BoltDB Chains

This doc explains how to lower our CFG (from `cfg_opt_pretty.txt`, informed by `src/cfg/mod.rs`) into idiomatic, safe Go that uses BoltDB. It extends the earlier sketch with stricter rules you requested: primary key structs, typed helpers, bidirectional converters, explicit per-instruction lowering, conversion placement at basic-block boundaries, `NextReq`/hop return semantics (matching `tpccNewOrderChain.go`), and deterministic temporary naming.

---

## High-level approach

- Use the CFG as the canonical spec. Emit one Go function per CFG hop and map each basic block to a label and `goto` structure so control flow remains identical to the IR.
- All table access must be typed: keys are structs, rows are structs. Do not accept a `params map[string]string` inside table helpers.
- Keep conversions between strings and typed values at block boundaries only (begin / end of basic block), guided by live-in / live-out sets.

---

## Table & Key representation (strict)

Rules:

1. Every table gets two generated types: `<Table>Key` (always, even for single-column keys) and `<Table>` (the row struct).
2. Primary key fields go into the `<Table>Key` struct. Non-key fields go into `<Table>`.
3. Table helper signatures:

   - Getter: func get<Table>(tx *bolt.Tx, key <Table>Key) ([]byte, <Table>)
   - Optional: func put<Table>(tx *bolt.Tx, key <Table>Key, row <Table>) error

Example (Warehouse):

```go
type WarehouseKey struct { W_ID uint64 }

type Warehouse struct {
    Key  WarehouseKey
    W_NAME string
    W_TAX  float32
    W_YTD  float32
}

func getWarehouse(tx *bolt.Tx, key WarehouseKey) ([]byte, Warehouse) {
    b := tx.Bucket([]byte("tWarehouse"))
    kb := mustJSON(key)
    var w Warehouse
    _ = json.Unmarshal(b.Get(kb), &w)
    return kb, w
}

func putWarehouse(tx *bolt.Tx, key WarehouseKey, w Warehouse) {
    b := tx.Bucket([]byte("tWarehouse"))
    putData(b, mustJSON(key), mustJSON(w))
}
```

Use `mustJSON` (small helper) for concise emitted code; it should panic on unexpected marshal errors. `putData` writes to the provided bucket.

---

## Converter library (bidirectional)

Because inter-hop params and hop results are strings, a converters package must provide both directions for each used type. Emit these helpers once and import in generated code.

Minimal API (examples):

```go
// string -> typed
func toUint64(s string) uint64 { v, err := strconv.ParseUint(s, 10, 64); if err != nil { panic(err) }; return v }
func toInt64(s string) int64   { v, err := strconv.ParseInt(s, 10, 64); if err != nil { panic(err) }; return v }
func toFloat32(s string) float32 { f, err := strconv.ParseFloat(s, 32); if err != nil { panic(err) }; return float32(f) }
func toBool(s string) bool { b, err := strconv.ParseBool(s); if err != nil { panic(err) }; return b }

// typed -> string
func fromUint64(v uint64) string { return strconv.FormatUint(v, 10) }
func fromInt64(v int64) string { return strconv.FormatInt(v, 10) }
func fromFloat32(f float32) string { return strconv.FormatFloat(float64(f), 'f', -1, 32) }
func fromBool(b bool) string { return strconv.FormatBool(b) }
```

Put a `TODO` comment in the generated `converters.go` reminding implementers to add composite/list converters and to handle locale or precision policies if required.

---

## CFG type → Go type decision rules

- CFG `Int` → use `uint64` for identity fields (IDs) and `int64` for signed counters; the generator should consult table field metadata to pick. Default to `uint64` for primary key fields.
- CFG `Float` → `float32` (unless a higher precision is required).
- CFG `Bool` → `bool`.
- CFG `String` → `string`.
- `List(T)` → `[]T`.

Emit per-field comments documenting the chosen Go type and the reason (ID vs counter) to aid future audits.

---

## Instruction lowering: exact lists and example statements

Lower each `InstructionKind` into a small ordered list of Go statements. When the instruction uses string params, the generator emits conversions at block entry; when producing values that are live-out, emit conversions at block exit.

1) Assign

    - If `src` is an operand already typed: `dst := src` or `dst = src`.
    - If `src` is a constant: `dst := <literal>`.
    - If `src` is a string param: `dst := toType(params["name"])` (but that should be generated only at block entry; avoid repeating inside the block).

2) BinaryOp (e.g., AddInt, MulFloat)

    - Ensure both operands are typed locals: `lhs := <op1>; rhs := <op2>` (op1/op2 will be variables or constants).
    - Insert casts if needed: `dst := lhs + rhs` or `dst := int64(lhs) + int64(rhs)`.

3) UnaryOp

    - `dst := -operand` or `dst := !operand`.

4) Call

    - If intrinsic conversion: `dst := toType(arg)` or `dst := fromType(arg)`.
    - Otherwise: `res := Fn(arg1, arg2)`; if `dest` exists: `dst := res`.

5) TableGet

    - Build typed key (using live-in values): `k := TableKey{ ... }`.
    - `kBytes, row := getTable(tx, k)`.
    - If only a field is required: `dst := row.Field`.

6) TableSet

    - Mutate `row` locals as required by previous instructions.
    - `putData(tx.Bucket([]byte("t<Table>")), kBytes, mustJSON(row))`.

7) Assert

    - `if !cond { panic("assert failed: ...") }`.

For each emitted instruction, consult live-variable analysis. If the destination is dead at block exit, do not emit it. If a temporary will be used only inside one instruction, declare it inline with `:=`.

---

## Where and when to convert string ↔ typed values

Goal: minimize conversions and keep them deterministic.

Rule:

1. At the beginning of each basic block, the generator emits conversions for the block's live-in variables that are stored as strings in `params` (use CFG live-in set). Example:

```go
// Block entry (generated)
wid := toUint64(params["wid"])   // only if wid ∈ live-in(bb)
did := toUint64(params["did"])   // only if did ∈ live-in(bb)
```

2. Inside the block operate only on typed locals. Do not call converters mid-block.

3. At the end of each basic block, emit conversions for variables that are live-out (and that must be visible to the next hop):

```go
// Block exit (generated)
params["new_oid"] = fromUint64(newOid) // if newOid ∈ live-out(bb)
```

4. When the hop returns `Results` (i.e., values the caller/NextReq expects), convert those typed values into strings immediately before returning.

This strategy keeps the conversions to the block boundaries and uses live-in/live-out sets to minimize work.

---

## Terminators and `NextReq` behavior (match `tpccNewOrderChain.go` semantics)

Terminators map directly to small control patterns in Go:

- `Jump(bbX)` → `goto bbX`.
- `Branch(cond, if_true, if_false)` → `if cond { goto bbTrue } else { goto bbFalse }`.
- `Return(operand?)` → convert `operand` to string(s) as `Results` (if any), and `return &proto.TrxRes{ Status: proto.Status_Success, Info: in.Info, Results: results }, nil`.
- `HopExit(nextHop)` → when the CFG indicates a hop exit, the current hop should return the values that `NextReq` expects to use to compute the next request. Practically our generated hop functions return `(*proto.TrxRes, error)` with any `Results` set; the control loop that manages hops will call `NextReq` to create the next request.

`NextReq` responsibilities (used by `tpccNewOrderChain.go`):

1. Inspect `in.Info.Hopid` and `in.Results`.
2. Store or propagate `Results` into `params` using `from<Type>` converters as needed.
3. Construct typed values for partition computation using `to<Type>` on `params` when needed.
4. Compute `nextShard` from the CFG `node_partition` function (emit the same arithmetic expression).
5. Append `nextShard` to `history`, set `req.Params = params`, `req.History = history`, `req.Info.Hopid = in.Info.Hopid + 1`, `req.Dependency = in.Dependency`, then `return req, params, history, nextShard`.

Keep `params` as the string boundary representation; conversions to typed values should happen in the entry of the next hop's first basic block per the conversion rule above.

---

## Variable naming and `#tmp` notation

CFG prints temporaries like `#tmp56[v219]`. The generator must not invent ad-hoc names. Use this deterministic mapping:

- If a variable has a human name in the CFG, preserve it (e.g., `w_id`, `d_id`).
- For compiler temporaries, use the bracketed id as canonical: `#tmp56[v219]` → `tmp_v219` (or `tmp219`). The generator should prefer `tmp_v219` to avoid accidental collisions with user names.

Concretely:

```
#tmp56[v219] -> tmp_v219
#tmp0[v38]   -> tmp_v38
```

Do not try to recompute the preface number (`56`) from the pretty printer; always use the `vNNN` index as the unique id.

Declare temporaries in the smallest scope possible; if live-var analysis shows the temporary lives only inside a single block, declare it there with `:=`.

---

## Example: district hop (polished and following the rules)

This example shows the generated structure for a district hop, with typed keys/helpers, block labels, boundary conversions, and writes.

```go
func TPCCNewOrderDistrictHop(tx *bolt.Tx, in *proto.TrxReq) (*proto.TrxRes, error) {
    // params is the string boundary map
    params := in.Params

    // --- block bb0 (live-in conversion)
bb0:
    wid := toUint64(params["wid"])   // emitted because wid ∈ live-in(bb0)
    did := toUint64(params["did"])   // emitted because did ∈ live-in(bb0)
    cid := toUint64(params["cid"])   // etc.

    // Build typed key and fetch
    dkey := DistrictKey{ D_W_ID: wid, D_ID: did }
    dKeyBytes, d := getDistrict(tx, dkey)

    oid := d.D_NEXT_O_ID
    d.D_NEXT_O_ID = oid + 1

    // write-back district (mutate + TableSet)
    putData(tx.Bucket([]byte("tDistrict")), dKeyBytes, mustJSON(d))

    // create NewOrder and Order rows and persist them
    no := NewOrder{ NO_O_ID: oid, NO_D_ID: did, NO_W_ID: wid }
    putData(tx.Bucket([]byte("tNewOrder")), mustJSON(no), mustJSON(no))

    order := Order{ O_KEY: no, O_C_ID: cid, O_OL_CNT: O_OL_CNT /*constant*/, O_ALL_LOCAL: false }
    putData(tx.Bucket([]byte("tOrder")), mustJSON(no), mustJSON(order))

    // insert order lines (loop omitted here for brevity)

    // block exit: prepare results and params for next hop
    // If we need to return a result to NextReq, convert it now
    res := []string{} // e.g., no results for this hop
    return &proto.TrxRes{ Status: proto.Status_Success, Info: in.Info, Results: res }, nil
}
```

Notes:

- Conversions from `params` (strings) happened at block entry.
- Writes used `putData` and the `dKeyBytes` produced by `getDistrict`.
- No direct access to `in.Params` inside helpers; keys and rows are typed.

---

## Generator checklist (actionable)

1. Emit `<Table>Key` and `<Table>` types for every table in CFG.
2. Emit typed helpers: `get<Table>(tx,key)`, `put<Table>(tx,key,row)`.
3. Emit `converters.go` with bidirectional converters for all primitives and TODOs for lists/structs.
4. For each function/transaction and each of its hops:
   - Emit a Go function with signature `func(tx *bolt.Tx, in *proto.TrxReq) (*proto.TrxRes, error)`.
   - For each basic block: emit the block label, emit live-in conversions, emit lowered instructions (per-instruction statement lists), emit live-out conversions for params/results, emit terminator lowering.
   - Use `tmp_vNNN` naming for temporaries.
5. Emit `NextReq` logic matching the CFG partition functions and `tpccNewOrderChain.go` semantics; use `from<Type>` to serialize results into `params`.
6. Run `gofmt` on all generated files.

---

If you want I can implement one of these next: (a) regenerate `go_examples/tpccGlobal.go` and `tpccNewOrderChain.go` to follow these rules, or (b) generate a small prototype that reads the `cfg_opt_pretty.txt` table list and emits the Go structs + converters + typed helpers. Tell me which and I will implement it.
