// A generic list type based on append/emptyList
type List a;

// ------------------------------------------------------------
// Core Operations
// ------------------------------------------------------------

function length<a>(List a) returns (int);
function get<a>(List a, int) returns (a);
function append<a>(List a, a) returns (List a);

// The emptyList function takes a "witness" argument of type 'a'
// to make the type concrete, following the pattern from the official example.
function emptyList<a>(a) returns (List a);


// ------------------------------------------------------------
// Axioms
// ------------------------------------------------------------

// Axiom 1: All empty lists of the same type 'a' are equal,
// regardless of the witness value used to create them.
axiom (forall<a> w1:a, w2:a :: emptyList(w1) == emptyList(w2));

// Axiom 2: The length of an empty list is 0.
// This is now unambiguous as it uses the witness 'w'.
axiom (forall<a> w:a :: length(emptyList(w)) == 0);

// Axiom 3: Appending an element increases the length by 1.
axiom (forall<a> l:List a, e:a :: length(append(l, e)) == length(l) + 1);

// Axiom 4: Getting the last element after an append returns the appended element.
axiom (forall<a> l:List a, e:a :: get(append(l, e), length(l)) == e);

// Axiom 5: Accessing other elements remains the same after an append.
axiom (forall<a> l:List a, e:a, i:int :: 0 <= i && i < length(l) ==> get(append(l, e), i) == get(l, i));

// Axiom 6: Length is never negative (good practice for the verifier).
axiom (forall<a> l:List a :: length(l) >= 0);


// ------------------------------------------------------------
// Test Procedure
// ------------------------------------------------------------
procedure TestMyListAxioms()
{
    var l0, l1, l2, l3: List int;

    // Create the empty list by providing a witness value (0) of the desired type (int).
    l0 := emptyList(0);
    l1 := append(l0, 10);
    l2 := append(l1, 20);
    l3 := append(l2, 30);

    // --- Test 1: Lengths ---
    // The verifier can now instantiate Axiom 2 with witness '0' to prove this.
    assert length(l0) == 0;
    assert length(l1) == 1;
    assert length(l2) == 2;
    assert length(l3) == 3;

    // --- Test 2: Getting elements ---
    // These proofs, which depend on a correct understanding of length, will now succeed.
    assert get(l3, 2) == 30;
    assert get(l3, 1) == 20;
    assert get(l3, 0) == 10;
}