// Minimal kernel that captures the TPCC split-timeout pathology.
//
// Structure:
// - tiny prefix blocks (entry/preface)
// - long repeated unrolled-tail style suffix
//
// If a splitter repeatedly cuts near the front (e.g., block_index=2),
// one child remains close to the original hard suffix at every depth.

const N: int;
var A: [int]int;
var B: [int]int;

procedure item_step(i: int)
  modifies A, B;
{
  var q: int;
  var p: int;
  var t: int;

  q := A[i];
  p := B[i];

  // Nonlinear arithmetic is enough to make suffix obligations expensive.
  t := q * p;
  assert t >= 0;

  A[i] := q + 1;
  B[i] := p + t;
  return;
}

procedure commutative_tail_like()
  modifies A, B;
{
entry:
  // Prefix block 1 (cheap)
  A[0] := 1;
  goto pre2;

pre2:
  // Prefix block 2 (cheap)
  B[0] := 1;
  goto u0;

// Unrolled suffix starts here (structurally similar to TPCC unrolled tail).
u0:
  call item_step(0);
  goto u1;
u1:
  call item_step(1);
  goto u2;
u2:
  call item_step(2);
  goto u3;
u3:
  call item_step(3);
  goto u4;
u4:
  call item_step(4);
  goto u5;
u5:
  call item_step(5);
  goto u6;
u6:
  call item_step(6);
  goto u7;
u7:
  call item_step(7);
  goto u8;
u8:
  call item_step(8);
  goto u9;
u9:
  call item_step(9);
  goto u10;
u10:
  call item_step(10);
  goto u11;
u11:
  call item_step(11);
  goto done;

done:
  return;
}
