use std::collections::HashMap;

pub type Counter<T> = HashMap<T, usize>;

pub fn build_counter<T: Eq + std::hash::Hash>(iter: impl Iterator<Item = T>) -> Counter<T> {
    let mut counter = Counter::new();
    for item in iter {
        let target = counter.entry(item).or_insert(0);
        *target += 1;
    }
    counter
}
