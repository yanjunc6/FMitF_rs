use super::{Flat, Lattice, MapLattice, SetLattice};
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;

impl<T: Eq + Hash + Clone + Debug> PartialEq for SetLattice<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self.is_top, other.is_top) {
            (true, true) => true,                    // ⊤ == ⊤
            (false, false) => self.set == other.set, // compare sets
            _ => false,                              // ⊤ ̸= anything else
        }
    }
}
impl<T: Eq + Hash + Clone + Debug> Eq for SetLattice<T> {}

impl<T: Eq + Hash + Clone + Debug> SetLattice<T> {
    /// Convenience constructor for an ordinary set element (not ⊤).
    pub fn new(elements: impl IntoIterator<Item = T>) -> Self {
        Self {
            set: elements.into_iter().collect(),
            is_top: false,
        }
    }

    /// Construct ⊤ explicitly when you need it.
    pub fn top_element() -> Self {
        Self {
            set: HashSet::new(),
            is_top: true,
        }
    }

    /// Get a reference to the underlying set.
    /// Returns None if this is the top element.
    pub fn as_set(&self) -> Option<&HashSet<T>> {
        if self.is_top {
            None
        } else {
            Some(&self.set)
        }
    }

    /// Check if this is the top element (if this is universal set)
    pub fn is_top(&self) -> bool {
        self.is_top
    }
}

impl<T: Eq + Hash + Clone + Debug> Lattice for SetLattice<T> {
    /* ---------- lattice bounds ---------- */

    /// Bottom element (empty set)
    fn bottom() -> Option<Self> {
        Some(Self {
            set: HashSet::new(), // ∅
            is_top: false,
        })
    }

    /// Top element (universal set)
    fn top() -> Option<Self> {
        Some(Self::top_element()) // ⊤
    }

    /// meet (set intersection)
    fn meet(&self, other: &Self) -> Self {
        match (self.is_top, other.is_top) {
            (true, true) => self.clone(),   // ⊤ ∧ ⊤ = ⊤
            (true, false) => other.clone(), // ⊤ ∧ X  = X
            (false, true) => self.clone(),  // X ∧ ⊤  = X
            (false, false) => {
                let inter: HashSet<_> = self.set.intersection(&other.set).cloned().collect();
                Self {
                    set: inter,
                    is_top: false,
                }
            }
        }
    }

    // join (set union)
    fn join(&self, other: &Self) -> Self {
        match (self.is_top, other.is_top) {
            (true, _) | (_, true) => Self::top_element(), // ⊤ ∨ X = ⊤
            (false, false) => {
                let uni: HashSet<_> = self.set.union(&other.set).cloned().collect();
                Self {
                    set: uni,
                    is_top: false,
                }
            }
        }
    }

    // A ⩽ B  ≜  A ⊆ B (with ⊤ on top)
    fn less_equal(&self, other: &Self) -> bool {
        match (self.is_top, other.is_top) {
            (true, true) => true,   // ⊤ ⩽ ⊤
            (true, false) => false, // ⊤ ⩽ X  is false
            (false, true) => true,  // X  ⩽ ⊤  is true
            (false, false) => self.set.is_subset(&other.set),
        }
    }
}

impl<V: Clone + Eq + Debug> Flat<V> {
    /// Greatest lower bound  (meet, ∧)
    pub fn meet(&self, other: &Self) -> Self {
        use Flat::*;
        match (self, other) {
            // Anything met with ⊤ stays/ becomes the other thing
            (Top, x) | (x, Top) => x.clone(),

            // Same concrete value ⇒ that value, different ⇒ ⊥
            (Value(a), Value(b)) => {
                if a == b {
                    Value(a.clone())
                } else {
                    Bottom
                }
            }

            // Bottom with anything ⇒ Bottom
            (Bottom, _) | (_, Bottom) => Bottom,
        }
    }

    /// Least upper bound (join, ∨)
    pub fn join(&self, other: &Self) -> Self {
        use Flat::*;
        match (self, other) {
            // Anything joined with ⊥ gives the other thing
            (Bottom, x) | (x, Bottom) => x.clone(),

            // Same concrete value ⇒ that value, different ⇒ ⊤
            (Value(a), Value(b)) if a == b => Value(a.clone()),
            (Value(_), Value(_)) => Top,

            // ⊤ with anything ⇒ ⊤
            (Top, _) | (_, Top) => Top,
        }
    }
}

impl<K, V> MapLattice<K, V>
where
    K: Eq + Hash + Clone + Debug,
    V: Clone + Eq + Debug,
{
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            is_top: false,
        }
    }

    /// Access helper (Bottom if absent)
    fn get_flat<'a>(&'a self, k: &K) -> Flat<V> {
        if self.is_top {
            Flat::Top
        } else {
            self.map.get(k).cloned().unwrap_or(Flat::Bottom)
        }
    }

    /// Public accessor for getting the value for a key
    pub fn get(&self, k: &K) -> Flat<V> {
        self.get_flat(k)
    }

    /// Check if this is the top element
    pub fn is_top(&self) -> bool {
        self.is_top
    }

    /// Insert/overwrite a concrete `Flat` value for a key
    pub fn insert(&mut self, key: K, val: Flat<V>) {
        if !self.is_top {
            if val == Flat::Bottom {
                self.map.remove(&key);
            } else {
                self.map.insert(key, val);
            }
        }
    }

    /// Iterate over the key-value pairs in the map
    pub fn iter(&self) -> impl Iterator<Item = (&K, &Flat<V>)> {
        self.map.iter()
    }
}

/// Implementation of the generic `Lattice` trait
impl<K, V> Lattice for MapLattice<K, V>
where
    K: Eq + Hash + Clone + Debug,
    V: Clone + Eq + Debug,
{
    /// ⊥ = no information for any key
    fn bottom() -> Option<Self> {
        Some(Self::new())
    }

    /// ⊤ = every key (present or future) is Top
    fn top() -> Option<Self> {
        Some(Self {
            map: HashMap::new(),
            is_top: true,
        })
    }

    /// Meet (∧)  – greatest lower bound
    fn meet(&self, other: &Self) -> Self {
        // If either side is global ⊤, the result is the *other* operand
        if self.is_top {
            return other.clone();
        }
        if other.is_top {
            return self.clone();
        }

        let mut out = Self::new();
        for k in self.map.keys().chain(other.map.keys()) {
            let v = self.get_flat(k).meet(&other.get_flat(k));
            if v != Flat::Bottom {
                out.map.insert(k.clone(), v);
            }
        }
        out
    }

    /// Join (∨) – least upper bound
    fn join(&self, other: &Self) -> Self {
        // If either side is global ⊤, the result is ⊤
        if self.is_top || other.is_top {
            return Self {
                map: HashMap::new(),
                is_top: true,
            };
        }

        let mut out = Self::new();
        for k in self.map.keys().chain(other.map.keys()) {
            let v = self.get_flat(k).join(&other.get_flat(k));
            if v != Flat::Bottom {
                out.map.insert(k.clone(), v);
            }
        }
        out
    }
}
