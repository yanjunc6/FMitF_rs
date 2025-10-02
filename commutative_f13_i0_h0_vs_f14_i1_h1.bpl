type String;
const empty: String;
function concat(x: String, y: String): String;
function str<a>(x: a): String;
axiom (forall s: String :: {concat(empty, s)} concat(empty, s) == s);
axiom (forall s: String :: {concat(s, empty)} concat(s, empty) == s);
axiom (forall<a> x: a, y: a :: {str(x), str(y)} str(x) == str(y) ==> x == y);
axiom (forall<a> x: a :: {str(x)} str(x) != empty);
type List a;
function length<a>(List a) returns (int);
function get<a>(List a, int) returns (a);
function append<a>(List a, a) returns (List a);
function emptyList<a>(a) returns (List a);
axiom (forall<a> w1:a, w2:a :: emptyList(w1) == emptyList(w2));
axiom (forall<a> w:a :: length(emptyList(w)) == 0);
axiom (forall<a> l:List a, e:a :: length(append(l, e)) == length(l) + 1);
axiom (forall<a> l:List a, e:a :: get(append(l, e), length(l)) == e);
axiom (forall<a> l:List a, e:a, i:int :: 0 <= i && i < length(l) ==> get(append(l, e), i) == get(l, i));
axiom (forall<a> l:List a :: length(l) >= 0);
axiom (forall<a> l1:List a, l2:List a :: (length(l1) != length(l2) ==> l1 != l2));
axiom (forall<a> l1:List a, l2:List a, e:a, f:a :: (e == f && l1 == l2) ==> append(l1, e) == append(l2, f));
type UUID;
function genUUID(n: int, m: int): UUID;
axiom (forall n1: int, m1: int, n2: int, m2: int :: {genUUID(n1, m1), genUUID(n2, m2)} genUUID(n1, m1) == genUUID(n2, m2) ==> (n1 == n2 && m1 == m2));
axiom (forall n1: int, m1: int, n2: int, m2: int :: {genUUID(n1, m1), genUUID(n2, m2)} (n1 != n2 || m1 != m2) ==> genUUID(n1, m1) != genUUID(n2, m2));
type Iterator a;
function iter_position<T>(iter: Iterator T) returns (int);
function iter_length<T>(iter: Iterator T) returns (int);
function iter_n<T>(iter: Iterator T) returns (int);
function iter_m<T>(iter: Iterator T) returns (int);
function model_iter_length(n: int, m: int) returns (int);
function scan<T>(t: Table T, n: int, m: int) returns (Iterator (Table T));
function next<T>(iter: Iterator T) returns (Iterator T);
function hasNext<T>(iter: Iterator T) returns (bool);
axiom (forall n: int, m: int :: model_iter_length(n, m) >= 0);
axiom (forall n: int, m: int :: model_iter_length(n, m) == 1);
axiom (forall<T> t: Table T, n: int, m: int :: iter_position(scan(t, n, m)) == 0);
axiom (forall<T> t: Table T, n: int, m: int :: iter_n(scan(t, n, m)) == n);
axiom (forall<T> t: Table T, n: int, m: int :: iter_m(scan(t, n, m)) == m);
axiom (forall<T> t: Table T, n: int, m: int :: iter_length(scan(t, n, m)) == model_iter_length(n, m));
axiom (forall<T> iter: Iterator T :: iter_position(next(iter)) == iter_position(iter) + 1);
axiom (forall<T> iter: Iterator T :: iter_length(next(iter)) == iter_length(iter));
axiom (forall<T> iter: Iterator T :: iter_n(next(iter)) == iter_n(iter));
axiom (forall<T> iter: Iterator T :: iter_m(next(iter)) == iter_m(iter));
axiom (forall<T> iter: Iterator T :: hasNext(iter) <==> iter_position(iter) < iter_length(iter));
function get_UID1#15(arg0: Iterator (Table (Graph))) returns (int);
function model_get_UID1(n: int, m: int, index: int) returns (int);
axiom (forall iter: Iterator (Table (Graph)) :: hasNext(iter) ==> get_UID1#15(iter) == model_get_UID1(iter_n(iter), iter_m(iter), iter_position(iter)));
function get_UID2#16(arg0: Iterator (Table (Graph))) returns (int);
function model_get_UID2(n: int, m: int, index: int) returns (int);
axiom (forall iter: Iterator (Table (Graph)) :: hasNext(iter) ==> get_UID2#16(iter) == model_get_UID2(iter_n(iter), iter_m(iter), iter_position(iter)));
type Table a;
type Graph;
var Graph_UID2 : [int][int]int;
var Graph_UID1 : [int][int]int;
var Graph_Counter : [int][int]int;
const __slice__ : int;
const TBL_Graph : Table (Graph);
procedure Check_SliceCommut_Hop0_vs_Hop1()
modifies Graph_Counter;
{
  var s1_active : bool;
  var s0_active : bool;
  var Graph_Counter_init : [int][int]int;
  var s1_uid_init : int;
  var s0_uid1_init : int;
  var s0_uid2_init : int;
  var s0_#tmp0 : UUID;
  var s0_#tmp1 : UUID;
  var s0_uid1 : int;
  var s0_uid2 : int;
  var __dummy_witness_1 : int;
  var s1_#tmp2 : List (int);
  var s1_uids : List (int);
  var s1_#tmp3 : Iterator (Table (Graph));
  var s1_graph_iter : Iterator (Table (Graph));
  var s1_#tmp4 : bool;
  var s1_#tmp5 : int;
  var s1_#tmp6 : int;
  var s1_graph_uid2 : int;
  var s1_#tmp7 : int;
  var s1_#tmp8 : bool;
  var s1_uid : int;
  var s1_#tmp9 : bool;
  var s1_#tmp10 : bool;
  var s1_#tmp11 : List (int);
  var Graph_Counter_a_then_b : [int][int]int;
  var s1_uids_a_then_b : List (int);
  var Graph_Counter_b_then_a : [int][int]int;
  var s1_uids_b_then_a : List (int);

  // Slice commutativity verification: hop 0 vs hop 1
  // --- Step 1: Havoc initial state ---
    havoc Graph_Counter;
    havoc s1_uid;
    havoc s0_uid1;
    havoc s0_uid2;
    s1_active := true;
    s0_active := true;
  // --- Step 2: Save initial state ---
    Graph_Counter_init := Graph_Counter;
    s1_uid_init := s1_uid;
    s0_uid1_init := s0_uid1;
    s0_uid2_init := s0_uid2;
  // --- Step 3: Execute special interleavings ---
  // Executing A then B:
    if (s0_active) {
    s0_block0__ab:
      s0_#tmp0 := genUUID(3, 0);
      s0_#tmp1 := genUUID(4, 0);
      Graph_Counter := Graph_Counter[s0_uid1 := Graph_Counter[s0_uid1][s0_uid2 := 0]];
      s0_active := false;
    }
    if (s1_active) {
    s1_block1__ab:
      s1_#tmp2 := emptyList(__dummy_witness_1);
      s1_uids := s1_#tmp2;
      s1_#tmp3 := scan(TBL_Graph, 0, 1);
      s1_graph_iter := s1_#tmp3;
      goto s1_block2__ab;
    s1_block2__ab:
      s1_#tmp4 := hasNext(s1_graph_iter);
      if (s1_#tmp4) {
      goto s1_block3__ab;
    } else {
      goto s1_block5__ab;
    }
    s1_block3__ab:
      s1_#tmp5 := get_UID1#15(s1_graph_iter);
      s1_#tmp6 := get_UID2#16(s1_graph_iter);
      s1_graph_uid2 := s1_#tmp6;
      s1_#tmp7 := Graph_Counter[s1_#tmp5][s1_#tmp6];
      s1_#tmp8 := s1_#tmp5 == s1_uid;
      s1_#tmp9 := s1_#tmp7 > 0;
      s1_#tmp10 := s1_#tmp8 && s1_#tmp9;
      if (s1_#tmp10) {
      goto s1_block6__ab;
    } else {
      goto s1_block7__ab;
    }
    s1_block4__ab:
      s1_graph_iter := next(s1_graph_iter);
      goto s1_block2__ab;
    s1_block5__ab:
      s1_active := false;
      goto out1;
    s1_block6__ab:
      s1_#tmp11 := append(s1_uids, s1_graph_uid2);
      s1_uids := s1_#tmp11;
      goto s1_block7__ab;
    s1_block7__ab:
      goto s1_block4__ab;
    }
    out1:
    assert false;
  // Snapshotting final state for a_then_b
    Graph_Counter_a_then_b := Graph_Counter;
    s1_uids_a_then_b := s1_uids;
  // Restoring initial state:
    Graph_Counter := Graph_Counter_init;
    s1_uid := s1_uid_init;
    s0_uid1 := s0_uid1_init;
    s0_uid2 := s0_uid2_init;
    s1_active := true;
    s0_active := true;
  // Executing B then A:
    if (s1_active) {
    s1_block1__ba:
      s1_#tmp2 := emptyList(__dummy_witness_1);
      s1_uids := s1_#tmp2;
      s1_#tmp3 := scan(TBL_Graph, 0, 1);
      s1_graph_iter := s1_#tmp3;
      goto s1_block2__ba;
    s1_block2__ba:
      s1_#tmp4 := hasNext(s1_graph_iter);
      if (s1_#tmp4) {
      goto s1_block3__ba;
    } else {
      goto s1_block5__ba;
    }
    s1_block3__ba:
      s1_#tmp5 := get_UID1#15(s1_graph_iter);
      s1_#tmp6 := get_UID2#16(s1_graph_iter);
      s1_graph_uid2 := s1_#tmp6;
      s1_#tmp7 := Graph_Counter[s1_#tmp5][s1_#tmp6];
      s1_#tmp8 := s1_#tmp5 == s1_uid;
      s1_#tmp9 := s1_#tmp7 > 0;
      s1_#tmp10 := s1_#tmp8 && s1_#tmp9;
      if (s1_#tmp10) {
      goto s1_block6__ba;
    } else {
      goto s1_block7__ba;
    }
    s1_block4__ba:
      //assert {:msg "Loop bound reached"} false;
      s1_graph_iter := next(s1_graph_iter);
      goto s1_block2__ba;
    s1_block5__ba:
      s1_active := false;
    s1_block6__ba:
      s1_#tmp11 := append(s1_uids, s1_graph_uid2);
      s1_uids := s1_#tmp11;
      goto s1_block7__ba;
    s1_block7__ba:
      goto s1_block4__ba;
    }
    if (s0_active) {
    s0_block0__ba:
      s0_#tmp0 := genUUID(3, 0);
      s0_#tmp1 := genUUID(4, 0);
      Graph_Counter := Graph_Counter[s0_uid1 := Graph_Counter[s0_uid1][s0_uid2 := 0]];
      s0_active := false;
    }
  // Snapshotting final state for b_then_a
    Graph_Counter_b_then_a := Graph_Counter;
    s1_uids_b_then_a := s1_uids;
  // --- Step 4: Verify A→B ≡ B→A (Special interleavings equivalence) ---
  // Verifying A->B === B->A equivalence:
    assert {:msg "(SpecialInterleavingNonEquivalence (node_1 (function_id . 13) (instance . 0) (hop_id . 0)) (node_2 (function_id . 14) (instance . 1) (hop_id . 1)))"} (Graph_Counter_a_then_b == Graph_Counter_b_then_a) && (s1_uids_a_then_b == s1_uids_b_then_a);
}

