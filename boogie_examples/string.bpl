// StringModel.bpl
// Uninterpreted String model with concat, empty, and a generic str<a>.
// No IntToString/RealToString—only str<a>.

// --------------------------
// Type and Operators
// --------------------------

type String;

const empty: String;

function concat(x: String, y: String): String;

// Generic to-string for any type T
function str<a>(x: a): String;


// --------------------------
// Axioms (with triggers)
// --------------------------

// Identity of empty for concat
axiom (forall s: String :: {concat(empty, s)} concat(empty, s) == s);
axiom (forall s: String :: {concat(s, empty)} concat(s, empty) == s);

// Injectivity of str<T> (for each instantiation of T)
axiom (forall<a> x: a, y: a :: {str(x), str(y)} str(x) == str(y) ==> x == y);

// Optional: str<T>(x) is never the empty string (uniform for all T)
axiom (forall<a> x: a :: {str(x)} str(x) != empty);


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

// The empty literal "" is represented by 'empty'.


// --------------------------
// Tests / Verification Harness
// --------------------------

procedure TestConcatIdentityLeft()
{
  var s: String;
  havoc s;
  assert concat(empty, s) == s;
}

procedure TestConcatIdentityRight()
{
  var s: String;
  havoc s;
  assert concat(s, empty) == s;
}

procedure Test_str_int_Injective()
{
  var i, j: int;
  havoc i, j;
  assume str(i) == str(j);
  assert i == j;
}

procedure Test_str_real_Injective()
{
  var x, y: real;
  havoc x, y;
  assume str(x) == str(y);
  assert x == y;
}

// str-values are not empty (axiomatized above)
procedure Test_str_NotEmpty()
{
  assert str(0) != empty;
  assert str(0.0) != empty;
}

procedure TestConcatWith_str()
{
  assert concat(str(1.5), empty) == str(1.5);
  assert concat(empty, str(42)) == str(42);
}


// --------------------------
// General Equality Tests
// --------------------------

procedure TestEqualityByAssignment()
{
  var s, s1, s2: String;
  havoc s;
  s1 := s;
  s2 := s;
  assert s1 == s2;
}

procedure TestEqualityTransitivity()
{
  var s1, s2, s3: String;
  havoc s1, s2, s3;
  assume s1 == s2;
  assume s2 == s3;
  assert s1 == s3;
}

procedure TestEqualityReflexivity()
{
  var s: String;
  havoc s;
  assert s == s;
}

procedure TestEqualitySymmetry()
{
  var s1, s2: String;
  havoc s1, s2;
  assume s1 == s2;
  assert s2 == s1;
}

procedure TestEqualityCongruence()
{
  var s1, s2: String;
  havoc s1, s2;
  assume s1 == s2;
  assert concat(s1, empty) == concat(s2, empty);
  assert concat(empty, s1) == concat(empty, s2);
}

procedure Test_SameLiteralIsSameSymbol()
{
  var a, b: String;
  a := L_abc;
  b := L_abc;
  assert a == b; // same constant reused for the same literal
}

type AnyT;

procedure Test_str_GenericInjective()
{
  var x, y: AnyT;
  havoc x, y;
  assume str(x) == str(y);
  assert x == y;
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

  call Test_str_int_Injective();
  call Test_str_real_Injective();
  call Test_str_NotEmpty();
  call TestConcatWith_str();

  call TestEqualityByAssignment();
  call TestEqualityTransitivity();
  call TestEqualityReflexivity();
  call TestEqualitySymmetry();
  call TestEqualityCongruence();
  call Test_SameLiteralIsSameSymbol();

  call Test_str_GenericInjective();

  // Uncomment to see failing proof:
  // call Negative_Test_DoNotProveUnknownFacts();
}