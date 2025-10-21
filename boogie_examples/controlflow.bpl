// A procedure with branching control flow to demonstrate the VCGen.
procedure ControlFlowTest(x: int)
{
    // This is our first basic block (let's call it B0).
    var y: int;

    // This 'if' creates a branch, sending control to one of two paths.
    if (x > 10) {
        // This is the "then" block (B1).
        // It's a separate location in the program's map.
        y := 1;
        assert y == 1;

    } else {
        // This is the "else" block (B2).
        // A third location in the program's map.
        y := -1;
        assert y == -1;
    }

    // After the if/else, the paths join back together.
    // This is our final block (B3).
    // The assertion here must hold true regardless of which path was taken.
    assert y == 1 || y == -1;
}