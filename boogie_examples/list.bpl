// ------------------------------------------------------------
// Polymorphic List with Nil/Cons, length, and get
// ------------------------------------------------------------

type List a;

// Constructors
function Cons<a>(x:a, t:List a) returns (List a);
// Nil needs a witness of type a (standard polytype trick)
function Nil<a>(w:a) returns (List a);

// Operations
function length<a>(l:List a) returns (int);
function get<a>(l:List a, i:int) returns (a);

// ------------------------------------------------------------
// Axioms
// ------------------------------------------------------------

// Nil is independent of its witness (so Nil(0) == Nil(42) == Nil(x))
axiom (forall<a> u:a, v:a :: Nil(u) == Nil(v));

// Length of Nil and Cons
axiom (forall<a> w:a :: length(Nil(w)) == 0);
axiom (forall<a> x:a, t:List a :: length(Cons(x, t)) == 1 + length(t));

// Indexing semantics (only defined within bounds)
// Head at index 0
axiom (forall<a> x:a, t:List a :: get(Cons(x, t), 0) == x);
// Tail shifted for i>0, guarded by bounds
axiom (forall<a> x:a, t:List a, i:int ::
  0 < i && i < length(Cons(x, t)) ==> get(Cons(x, t), i) == get(t, i - 1)
);

// Non-negativity (helps discharge bounds like 0 <= i < length(l))
axiom (forall<a> l:List a :: length(l) >= 0);


// ------------------------------------------------------------
// Tests: build list literals with Cons/.../Nil
// ------------------------------------------------------------

procedure Test_Int() {
  var NIL:List int;
  var l:List int;
  var l2:List int;

  // Empty list (witness can be any int; uniqueness axiom makes it canonical)
  NIL := Nil(0);

  // Build [1, 2, 3]
  l := Cons(1, Cons(2, Cons(3, NIL)));

  assert length(l) == 3;
  assert get(l, 0) == 1;
  assert get(l, 1) == 2;
  assert get(l, 2) == 3;

  // Witness independence: Nil(123) is the same empty list
  assert Nil(0) == Nil(123);

  // Another literal: [5, 6]
  l2 := Cons(5, Cons(6, NIL));
  assert length(l2) == 2 && get(l2, 0) == 5 && get(l2, 1) == 6;

  // Sanity: this should not be provable
  assert length(Cons(1, Cons(2, Cons(3, NIL)))) == 2; // not provable
}

procedure Test_Generic<a>(x:a, y:a, z:a) {
  var NIL:List a;
  var l:List a;
  var tail:List a;

  NIL := Nil(x);
  l := Cons(x, Cons(y, Cons(z, NIL)));

  assert length(l) == 3;
  assert get(l, 0) == x;
  assert get(l, 1) == y;
  assert get(l, 2) == z;

  // Removing the head yields a tail whose length is one less
  tail := Cons(y, Cons(z, NIL));
  assert length(tail) == length(l) - 1;
}