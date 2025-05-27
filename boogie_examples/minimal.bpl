var T_val: int;

procedure F_Hop1() returns (x: int)
{
  // Only reads T_val, so no modifies needed
  x := T_val;
}

procedure F_Hop2(x: int)
  modifies T_val;
{
  T_val := x + 1;
}

procedure G_Hop1()
  modifies T_val;
{
  T_val := T_val * 2;
}

procedure main()
  modifies T_val;
{
  var x: int;
  var init: int;
  var final1: int;
  var final2: int;
  var final3: int;

  // Arbitrary initial value for T_val
  havoc T_val;
  init := T_val;

  // Interleaving 1: F; then G
  T_val := init;
  call x := F_Hop1();
  call F_Hop2(x);
  call G_Hop1();
  final1 := T_val;

  // Interleaving 2: F_Hop1, then G, then F_Hop2
  T_val := init;
  call x := F_Hop1();
  call G_Hop1();
  call F_Hop2(x);
  final2 := T_val;

  // Interleaving 3: G; then F
  T_val := init;
  call G_Hop1();
  call x := F_Hop1();
  call F_Hop2(x);
  final3 := T_val;

  // Assert serializability: Each interleaving is equivalent to a serial order
  assert final1 == final2 || final2 == final3;
}