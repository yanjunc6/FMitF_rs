procedure CommutativityOfStores(m0: [int]int, k1: int, k2: int, v1: int, v2: int)
// Assume the keys are distinct to avoid one write overwriting the other.
    requires k1 != k2;
{
    var m1, m2: [int]int;

    // Build map m1 by storing v1 then v2.
    m1 := m0[k1 := v1];
    m1 := m1[k2 := v2];

    // Build map m2 by storing v2 then v1.
    m2 := m0[k2 := v2];
    m2 := m2[k1 := v1];

    // This assertion can only be proven with extensionality.
    assert m1 == m2;
}


procedure StoreLoadExample(m: [int]int, i: int)
{
    assert m[i := m[i]] == m;
}   