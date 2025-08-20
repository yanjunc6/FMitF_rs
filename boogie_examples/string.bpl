// StringModel.bpl
// Uninterpreted String model with concat, empty, int/real conversions.
// No assumption that IntToString(i) and RealToString(r) are always distinct.

// --------------------------
// Type and Operators
// --------------------------

type String;

const empty: String;

function Concat(x: String, y: String): String;
function IntToString(i: int): String;
function RealToString(r: real): String;


// --------------------------
// Axioms (with correct triggers)
// --------------------------

// Identity of empty for concat
axiom (forall s: String :: {Concat(empty, s)} Concat(empty, s) == s);
axiom (forall s: String :: {Concat(s, empty)} Concat(s, empty) == s);

// Injectivity of IntToString
axiom (forall i: int, j: int ::
  { IntToString(i), IntToString(j) }
  IntToString(i) == IntToString(j) ==> i == j);

// Injectivity of RealToString
axiom (forall x: real, y: real ::
  { RealToString(x), RealToString(y) }
  RealToString(x) == RealToString(y) ==> x == y);

// Conversions never yield empty
axiom (forall i: int :: {IntToString(i)} IntToString(i) != empty);
axiom (forall r: real :: {RealToString(r)} RealToString(r) != empty);


// --------------------------
// String literals as constants
// --------------------------
// For each literal lexeme in your source, declare one constant and reuse it.
// Do NOT mark them 'unique' unless you want to force all literals to be distinct.
// Reusing the same const name across the program ensures multiple occurrences
// of the same literal refer to the same String value.

const L_abc: String; // models the literal "abc"
const L_def: String; // models the literal "def"
// Add more as needed, e.g., const L_hello: String;

// The empty literal "" is represented by 'empty' (no extra constant needed).

// --------------------------
// Tests / Verification Harness
// --------------------------

procedure TestConcatIdentityLeft()
{
  var s: String;
  havoc s;
  assert Concat(empty, s) == s;
}

procedure TestConcatIdentityRight()
{
  var s: String;
  havoc s;
  assert Concat(s, empty) == s;
}

procedure TestIntToStringInjective()
{
  var i, j: int;
  havoc i, j;
  assume IntToString(i) == IntToString(j);
  assert i == j;
}

procedure TestRealToStringInjective()
{
  var x, y: real;
  havoc x, y;
  assume RealToString(x) == RealToString(y);
  assert x == y;
}

// We removed the "distinct codomains" test because we no longer assume it.

// Conversions not empty
procedure TestConversionsNotEmpty()
{
  assert IntToString(0) != empty;
  assert RealToString(0.0) != empty;
}

procedure TestConcatWithConversion()
{
  assert Concat(RealToString(1.5), empty) == RealToString(1.5);
  assert Concat(empty, IntToString(42)) == IntToString(42);
}


// --------------------------
// General Equality Tests
// --------------------------

// Equality from assignment (use := for assignment and == for equality)
procedure TestEqualityByAssignment()
{
  var s, s1, s2: String;
  havoc s;
  s1 := s;
  s2 := s;
  assert s1 == s2;
}

// Transitivity: given s1 == s2 and s2 == s3, then s1 == s3
procedure TestEqualityTransitivity()
{
  var s1, s2, s3: String;
  havoc s1, s2, s3;
  assume s1 == s2;
  assume s2 == s3;
  assert s1 == s3;
}

// Reflexivity: s == s
procedure TestEqualityReflexivity()
{
  var s: String;
  havoc s;
  assert s == s;
}

// Symmetry: from s1 == s2 infer s2 == s1
procedure TestEqualitySymmetry()
{
  var s1, s2: String;
  havoc s1, s2;
  assume s1 == s2;
  assert s2 == s1;
}

// Congruence (substitutivity) for functions
procedure TestEqualityCongruence()
{
  var s1, s2: String;
  havoc s1, s2;
  assume s1 == s2;
  assert Concat(s1, empty) == Concat(s2, empty);
  assert Concat(empty, s1) == Concat(empty, s2);
}

procedure Test_SameLiteralIsSameSymbol()
{
  var a, b: String;
  a := L_abc;
  b := L_abc;
  assert a == b; // same constant reused for the same literal
}

procedure Negative_Test_DoNotProveUnknownFacts()
{
  // Neither of these should be provable under the minimal model:
  assert L_abc == L_def;     // should fail
  assert L_abc != L_def;     // should also fail
}

// --------------------------
// Main: Run all tests
// --------------------------

procedure Main()
{
  call TestConcatIdentityLeft();
  call TestConcatIdentityRight();
  call TestIntToStringInjective();
  call TestRealToStringInjective();
  call TestConversionsNotEmpty();
  call TestConcatWithConversion();

  call TestEqualityByAssignment();
  call TestEqualityTransitivity();
  call TestEqualityReflexivity();
  call TestEqualitySymmetry();
  call TestEqualityCongruence();
  call Test_SameLiteralIsSameSymbol();
  call Negative_Test_DoNotProveUnknownFacts();
}