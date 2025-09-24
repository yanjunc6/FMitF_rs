// ----------------------------------------
// List<T> model with map/filter/reduce
// ----------------------------------------

type List<T>;

const Nil<T>: List<T>;
function Cons<T>(head: T, tail: List<T>) returns (List<T>);

// Basic operations
function length<T>(list: List<T>) returns (int);
function get<T>(list: List<T>, idx: int) returns (T);

// Higher-order style operations (functions as maps)
function map<T, U>(list: List<T>, f: [T]U) returns (List<U>);
function filter<T>(list: List<T>, p: [T]bool) returns (List<T>);
function reduce<T, U>(list: List<T>, rf: [U, T]U, init: U) returns (U);

// ----------------------------------------
// Axioms: structure and semantics
// ----------------------------------------

// Non-negativity of length
axiom (forall T: Type, l: List<T> :: length(l) >= 0);

// Length of Nil and Cons
axiom (forall T: Type :: length(Nil<T>) == 0);
axiom (forall T: Type, h: T, t: List<T> :: length(Cons(h, t)) == 1 + length(t));

// Indexing for Cons: head at 0; tail shifted for i>0 but only within bounds
axiom (forall T: Type, h: T, t: List<T> :: get(Cons(h, t), 0) == h);
axiom (forall T: Type, h: T, t: List<T>, i: int ::
  0 < i && i < length(Cons(h, t)) ==> get(Cons(h, t), i) == get(t, i - 1)
);

// Map semantics: structural (map preserves shape) and pointwise behavior
axiom (forall T: Type, U: Type, f: [T]U :: map(Nil<T>, f) == Nil<U>);
axiom (forall T: Type, U: Type, f: [T]U, h: T, t: List<T> ::
  map(Cons(h, t), f) == Cons(f[h], map(t, f))
);

// Map preserves length; for indices in range, map applies f to elements
axiom (forall T: Type, U: Type, l: List<T>, f: [T]U :: length(map(l, f)) == length(l));
axiom (forall T: Type, U: Type, l: List<T>, f: [T]U, i: int ::
  0 <= i && i < length(l) ==> get(map(l, f), i) == f[get(l, i)]
);

// Filter semantics: structural (keep/drop head) and length monotonicity
axiom (forall T: Type, p: [T]bool :: filter(Nil<T>, p) == Nil<T>);
axiom (forall T: Type, p: [T]bool, h: T, t: List<T> ::
  p[h] ==> filter(Cons(h, t), p) == Cons(h, filter(t, p))
);
axiom (forall T: Type, p: [T]bool, h: T, t: List<T> ::
  !p[h] ==> filter(Cons(h, t), p) == filter(t, p)
);
// Filter does not increase length
axiom (forall T: Type, l: List<T>, p: [T]bool :: length(filter(l, p)) <= length(l));

// Reduce (fold-left) semantics over the list
axiom (forall T: Type, U: Type, rf: [U, T]U, init: U :: reduce(Nil<T>, rf, init) == init);
axiom (forall T: Type, U: Type, rf: [U, T]U, init: U, h: T, t: List<T> ::
  reduce(Cons(h, t), rf, init) == reduce(t, rf, rf[init, h])
);

// ----------------------------------------
// Test harness over int lists
// ----------------------------------------

procedure TestIntLists();
implementation TestIntLists() {
  var l: List<int>;
  var lm: List<int>;
  var lf: List<int>;
  var inc: [int]int;
  var even: [int]bool;
  var add: [int, int]int;
  var r: int;

  // Build a concrete list [1, 2, 3]
  l := Cons(1, Cons(2, Cons(3, Nil<int>)));

  // Define inc(x) = x + 1, even(x) = x mod 2 == 0, and add(acc, x) = acc + x
  inc := (lambda x: int :: x + 1);
  even := (lambda x: int :: (x mod 2) == 0);
  add := (lambda acc: int, x: int :: acc + x);

  // Basic list checks
  assert length(l) == 3;
  assert get(l, 0) == 1;
  assert get(l, 1) == 2;
  assert get(l, 2) == 3;

  // Map: inc over [1,2,3] gives [2,3,4]
  lm := map(l, inc);
  assert length(lm) == 3;
  assert get(lm, 0) == 2;
  assert get(lm, 1) == 3;
  assert get(lm, 2) == 4;

  // Filter: keep even numbers from [1,2,3] gives [2]
  lf := filter(l, even);
  assert length(lf) == 1;
  assert get(lf, 0) == 2;

  // Reduce: sum with initial 0 over [1,2,3] gives 6
  r := reduce(l, add, 0);
  assert r == 6;

  // Composition examples
  var lf_map: List<int>;
  lf_map := map(lf, inc);           // [2] mapped by inc gives [3]
  assert length(lf_map) == length(lf);
  assert get(lf_map, 0) == 3;

  // Sum of map(l, inc) = 2 + 3 + 4 = 9
  assert reduce(map(l, inc), add, 0) == 9;

  // Filter the mapped list for evens: [2,3,4] -> [2,4], sum is 6
  assert reduce(filter(map(l, inc), even), add, 0) == 6;
}