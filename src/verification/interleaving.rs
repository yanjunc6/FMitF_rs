// src/verify/interleaving.rs

use crate::cfg::HopId;

pub fn enumerate_interleavings(a: &[HopId], b: &[HopId]) -> Vec<Vec<HopId>> {
    let mut results = vec![];
    let mut current = vec![];
    interleave_rec(a, b, &mut current, &mut results);
    results
}

fn interleave_rec(
    a: &[HopId],
    b: &[HopId],
    current: &mut Vec<HopId>,
    results: &mut Vec<Vec<HopId>>,
) {
    if a.is_empty() && b.is_empty() {
        results.push(current.clone());
        return;
    }
    if !a.is_empty() {
        current.push(a[0]);
        interleave_rec(&a[1..], b, current, results);
        current.pop();
    }
    if !b.is_empty() {
        current.push(b[0]);
        interleave_rec(a, &b[1..], current, results);
        current.pop();
    }
}