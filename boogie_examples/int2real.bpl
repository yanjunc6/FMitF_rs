procedure GotoExample()
{
    // Variable declarations should be before the first label.
    var a: int; // Declare variable a
    var b: int; // Declare variable b
    var res: int; // Declare result variable res
    
    a := b;
    assert a == b;
    assert real(a) == real(b);
}