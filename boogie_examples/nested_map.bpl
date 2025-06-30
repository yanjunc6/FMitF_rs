// In Boogie, a is a nested map: a[i] is itself a [int]int map.
// In C++ terms, think of it like: std::unordered_map<int, std::unordered_map<int, int>> a;

var a: [int][int]int;


// Main usage demo.
procedure main()
  modifies a;
{
  var oldVal: int;
  
  // In C++: a[0][0] = 5;
  a[0] := a[0][0 := 5];

  // In C++: oldVal = a[0][0];
  oldVal := a[0][0];

  // In C++: a[i][j] += 3;
  a[0] := a[0][0 := a[0][0] + 3];

  // Now we expect a[0][0] == oldVal + 3.
  assert a[0][0] == oldVal + 3;
}