// This declares a map 'm' that models a store from integers to integers.
// In the SMT theory of arrays, this is an array.
var m: [int]int;

// This procedure takes three integers: an index 'x', a value 'y' to store at 'x',
// and another distinct index 'z'.
procedure MapUpdateExample(x: int, y: int, z: int)
    // We specify that this procedure modifies the map 'm'.
    modifies m;
    // We add a precondition that the two indices, x and z, are not equal.
    // This is crucial for the assertion at the end.
    requires x != z;
{
  // 1. First, we model storing the value 'y' at index 'x' in the map.
  // This creates a new version of the map 'm'.
  m[x] := y;

  // 2. Next, we store the value 100 at a *different* index 'z'.
  // This updates the map again.
  m[z] := 100;

  // 3. Finally, we assert that the value at the original index 'x' is still 'y'.
  // This demonstrates the "read-over-write" axiom: an update at one address
  // does not affect the value at a different address.
  assert m[x] == y;
}