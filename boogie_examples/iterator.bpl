// #############################################################################
// #  TYPE DEFINITIONS
// #############################################################################

// --- Core Types ---
type List a;
type Iterator a;
type Table a;

// --- Opaque/Uninterpreted Types ---
// These types are treated as black boxes, as requested.
type String;
type UUID;
type Activity;
const empty_string: String; // A constant for string initialization if needed.
const __dummy_witness_1: int; // A witness value for creating an emptyList.

// #############################################################################
// #  AXIOMS FOR LIST
// #############################################################################

// --- List Functions ---
function length<a>(List a) returns (int);
function get<a>(List a, int) returns (a);
function append<a>(List a, a) returns (List a);
function emptyList<a>(a) returns (List a);

// --- List Axioms ---
axiom (forall<a> w1:a, w2:a :: emptyList(w1) == emptyList(w2));
axiom (forall<a> w:a :: length(emptyList(w)) == 0);
axiom (forall<a> l:List a, e:a :: length(append(l, e)) == length(l) + 1);
axiom (forall<a> l:List a, e:a :: get(append(l, e), length(l)) == e);
axiom (forall<a> l:List a, e:a, i:int :: 0 <= i && i < length(l) ==> get(append(l, e), i) == get(l, i));
axiom (forall<a> l:List a :: length(l) >= 0);


// #############################################################################
// #  AXIOMS FOR DETERMINISTIC ITERATOR (MODIFIED)
// #############################################################################

// --- Internal Model Functions ---
function iter_position<T>(iter: Iterator T) returns (int);
function iter_length<T>(iter: Iterator T) returns (int);
function iter_n<T>(iter: Iterator T) returns (int);
function iter_m<T>(iter: Iterator T) returns (int);

// Model functions for length and each 'get' call.
// These are now pure functions of the (n, m) key and index.
function model_iter_length(n: int, m: int) returns (int);
function model_get_AID(n: int, m: int, index: int) returns (UUID);
function model_get_UID(n: int, m: int, index: int) returns (int);

// --- Public Intrinsic Functions ---
function scan<T>(t: Table T, n: int, m: int) returns (Iterator T);
function next<T>(iter: Iterator T) returns (Iterator T);
function hasNext<T>(iter: Iterator T) returns (bool);
function get_AID(iter: Iterator Activity) returns (UUID);
function get_UID(iter: Iterator Activity) returns (int);

// --- Iterator Axioms ---

// Axiom for deterministic length
axiom (forall n: int, m: int :: model_iter_length(n, m) >= 0);

// scan()
axiom (forall<T> t: Table T, n: int, m: int :: iter_position(scan(t, n, m)) == 0);
axiom (forall<T> t: Table T, n: int, m: int :: iter_n(scan(t, n, m)) == n);
axiom (forall<T> t: Table T, n: int, m: int :: iter_m(scan(t, n, m)) == m);
// Length is now determined by the (n, m) key
axiom (forall<T> t: Table T, n: int, m: int :: iter_length(scan(t, n, m)) == model_iter_length(n, m));

// next()
axiom (forall<T> iter: Iterator T :: iter_position(next(iter)) == iter_position(iter) + 1);
axiom (forall<T> iter: Iterator T :: iter_length(next(iter)) == iter_length(iter));
// The (n, m) key must be preserved when calling next()
axiom (forall<T> iter: Iterator T :: iter_n(next(iter)) == iter_n(iter));
axiom (forall<T> iter: Iterator T :: iter_m(next(iter)) == iter_m(iter));

// hasNext()
axiom (forall<T> iter: Iterator T :: hasNext(iter) <==> iter_position(iter) < iter_length(iter));

// get_...()
// The values returned by 'get' now map to the pure model functions using the iterator's key.
axiom (forall iter: Iterator Activity :: hasNext(iter) ==>
    get_AID(iter) == model_get_AID(iter_n(iter), iter_m(iter), iter_position(iter))
);

axiom (forall iter: Iterator Activity :: hasNext(iter) ==>
    get_UID(iter) == model_get_UID(iter_n(iter), iter_m(iter), iter_position(iter))
);


// #############################################################################
// #  GLOBAL STATE
// #############################################################################

const TBL_Activity: Table Activity;
var Activity_Log: [UUID][int]String;


// #############################################################################
// #  MAIN VERIFICATION PROCEDURE
// #############################################################################

procedure DeterminismCheck()
  // This procedure modifies global state, though in this example it's read-only.
  modifies Activity_Log;
{
  // --- Initial Shared State ---
  var s1_uid: int;
  var logs_run1: List String;
  var logs_run2: List String;

  // Local variables for the first run
  var s1_logs_run1: List String;
  var s1_activity_iter_run1: Iterator Activity;
  var s1_active_run1: bool;
  var s1_activity_aid_run1: UUID;
  var s1_activity_uid_run1: int;
  var s1_#tmp30_run1: List String;
  var s1_#tmp32_run1: bool;
  var s1_#tmp33_run1: UUID;
  var s1_#tmp34_run1: int;
  var s1_#tmp35_run1: bool;
  var s1_#tmp36_run1: String;
  var s1_#tmp37_run1: List String;

    // Local variables for the second run
  var s1_logs_run2: List String;
  var s1_activity_iter_run2: Iterator Activity;
  var s1_active_run2: bool;
  var s1_activity_aid_run2: UUID;
  var s1_activity_uid_run2: int;
  var s1_#tmp30_run2: List String;
  var s1_#tmp32_run2: bool;
  var s1_#tmp33_run2: UUID;
  var s1_#tmp34_run2: int;
  var s1_#tmp35_run2: bool;
  var s1_#tmp36_run2: String;
  var s1_#tmp37_run2: List String;

  // Havoc the transaction input
  havoc s1_uid;

  // =========================================================================
  //                           FIRST EXECUTION
  // =========================================================================
  

  s1_block14_run1:
    s1_#tmp30_run1 := emptyList(empty_string);
    s1_logs_run1 := s1_#tmp30_run1;
    // Use the base iterator to start this run
    s1_activity_iter_run1 := scan(TBL_Activity, 0, 1);
    goto s1_block15_run1;
  s1_block15_run1:
    //assume iter_position(s1_activity_iter_run1) >= 0;
    //assume iter_position(s1_activity_iter_run1) <= iter_length(s1_activity_iter_run1);
    s1_#tmp32_run1 := hasNext(s1_activity_iter_run1);
    if (s1_#tmp32_run1) {
      goto s1_block16_run1;
    } else {
      goto s1_block17_run1;
    }
  s1_block16_run1:
    s1_#tmp33_run1 := get_AID(s1_activity_iter_run1);
    s1_activity_aid_run1 := s1_#tmp33_run1;
    s1_#tmp34_run1 := get_UID(s1_activity_iter_run1);
    s1_activity_uid_run1 := s1_#tmp34_run1;
    s1_#tmp35_run1 := s1_activity_uid_run1 == s1_uid;
    if (s1_#tmp35_run1) {
      goto s1_block18_run1;
    } else {
      goto s1_block19_run1;
    }
  s1_block17_run1:
    s1_active_run1 := false;
    // End of first run, save the result
    logs_run1 := s1_logs_run1;
    goto second_execution_start;
  s1_block18_run1:
    s1_#tmp36_run1 := Activity_Log[s1_activity_aid_run1][s1_activity_uid_run1];
    s1_#tmp37_run1 := append(s1_logs_run1, s1_#tmp36_run1);
    s1_logs_run1 := s1_#tmp37_run1;
    goto s1_block19_run1;
  s1_block19_run1:
    // Advance the iterator for this run
    s1_activity_iter_run1 := next(s1_activity_iter_run1);
    goto s1_block15_run1;

second_execution_start:
  // =========================================================================
  //                           SECOND EXECUTION
  // =========================================================================

  s1_block14_run2:
    s1_#tmp30_run2 := emptyList(empty_string);
    s1_logs_run2 := s1_#tmp30_run2;
    // Use the SAME base iterator to start this run
    s1_activity_iter_run2 := scan(TBL_Activity, 0, 1);
    goto s1_block15_run2;
  s1_block15_run2:
    //assume iter_position(s1_activity_iter_run2) >= 0;
    //assume iter_position(s1_activity_iter_run2) <= iter_length(s1_activity_iter_run2);
    s1_#tmp32_run2 := hasNext(s1_activity_iter_run2);
    if (s1_#tmp32_run2) {
      goto s1_block16_run2;
    } else {
      goto s1_block17_run2;
    }
  s1_block16_run2:
    // assert s1_activity_iter_run1 == s1_activity_iter_run2;
    s1_#tmp33_run2 := get_AID(s1_activity_iter_run2);
    s1_activity_aid_run2 := s1_#tmp33_run2;
    s1_#tmp34_run2 := get_UID(s1_activity_iter_run2);
    s1_activity_uid_run2 := s1_#tmp34_run2;
    s1_#tmp35_run2 := s1_activity_uid_run2 == s1_uid;
    if (s1_#tmp35_run2) {
      goto s1_block18_run2;
    } else {
      goto s1_block19_run2;
    }
  s1_block17_run2:
    s1_active_run2 := false;
    // End of second run, save the result
    logs_run2 := s1_logs_run2;
    goto final_assertion;
  s1_block18_run2:
    //assert s1_activity_aid_run1 == s1_activity_aid_run2;
    //assert s1_activity_uid_run1 == s1_activity_uid_run2;
    s1_#tmp36_run2 := Activity_Log[s1_activity_aid_run2][s1_activity_uid_run2];
    s1_#tmp37_run2 := append(s1_logs_run2, s1_#tmp36_run2);
    s1_logs_run2 := s1_#tmp37_run2;
    goto s1_block19_run2;
  s1_block19_run2:
    // Advance the iterator for this run
    s1_activity_iter_run2 := next(s1_activity_iter_run2);
    goto s1_block15_run2;

final_assertion:
  // =========================================================================
  //                           FINAL ASSERTION
  // =========================================================================
  assert{:msg "logs are not equal"} logs_run1 == logs_run2;
}