use std::collections::HashMap;

/// An extension to a HashMap that makes it operate as a counter.
///
/// For example:
///
/// ```
/// use std::collections::HashMap;
/// use aoc::counter::Counter;
///
/// let mut counter = HashMap::counter_from_iter(vec![1, 1, 5].into_iter());
/// counter.increment(5, 1);
/// ```
pub trait Counter<Key: std::hash::Hash + Eq> {
    fn counter_from_iter(iter: impl Iterator<Item = Key>) -> Self;
    fn increment(&mut self, key: Key, amount: usize);
}

impl<Key: std::hash::Hash + Eq> Counter<Key> for HashMap<Key, usize> {
    fn counter_from_iter(iter: impl Iterator<Item = Key>) -> Self {
        let mut counter = Self::new();
        for item in iter {
            counter.increment(item, 1);
        }
        counter
    }

    fn increment(&mut self, key: Key, amount: usize) {
        let target = self.entry(key).or_insert(0);
        *target += amount;
    }
}
