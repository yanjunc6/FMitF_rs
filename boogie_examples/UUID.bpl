// UUIDModel_Pair.bpl
// Uninterpreted UUID type and a pure generator genUUID(n, m)
// with pairwise injectivity over its integer arguments.

type UUID;

// Pure generator keyed by a pair of integers
function genUUID(n: int, m: int): UUID;

// Injectivity axioms:
// 1) Equality of outputs implies equality of both indices
axiom (forall n1: int, m1: int, n2: int, m2: int ::
  {genUUID(n1, m1), genUUID(n2, m2)}
  genUUID(n1, m1) == genUUID(n2, m2) ==> (n1 == n2 && m1 == m2));

// 2) Distinctness of indices implies distinct outputs
//    (helps prove inequality directly from (n1 != n2) || (m1 != m2))
axiom (forall n1: int, m1: int, n2: int, m2: int ::
  {genUUID(n1, m1), genUUID(n2, m2)}
  (n1 != n2 || m1 != m2) ==> genUUID(n1, m1) != genUUID(n2, m2));


// --------------------------
// Sanity tests / harness
// --------------------------

procedure Test_CopyPreservesEquality()
{
  var a, b: UUID;
  havoc a;       // some unknown UUID
  b := a;        // copy
  assert a == b; // holds by assignment/equality reflexivity
}

procedure Test_Distinctness_DifferentFirstIndex()
{
  var n1, n2, m: int;
  assume n1 != n2;
  assert genUUID(n1, m) != genUUID(n2, m); // by axiom (2)
}

procedure Test_Distinctness_DifferentSecondIndex()
{
  var n, m1, m2: int;
  assume m1 != m2;
  assert genUUID(n, m1) != genUUID(n, m2); // by axiom (2)
}

procedure Test_Distinctness_BothDifferent()
{
  var n1, n2, m1, m2: int;
  assume n1 != n2 && m1 != m2;
  assert genUUID(n1, m1) != genUUID(n2, m2); // by axiom (2)
}

procedure Test_EqualityImpliesIndexEquality()
{
  var n1, n2, m1, m2: int;
  assume genUUID(n1, m1) == genUUID(n2, m2);
  assert n1 == n2;
  assert m1 == m2; // by axiom (1)
}

// Negative test: you cannot make two unequal UUIDs equal just by different witnesses
procedure Negative_SameWitnessIsSameUUID()
{
  var n, m: int;
  assert genUUID(n, m) == genUUID(n, m); // reflexivity of equality
  // The following should NOT be provable:
  // assert genUUID(n, m) != genUUID(n, m);
}