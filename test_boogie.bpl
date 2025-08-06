var x: int;

procedure test()
  modifies x;
{
entry:
  havoc x;
  assert x >= 0 || x < 0;
  return;
}
