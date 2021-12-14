use anyhow::Result;
use aoc::solver::Solver;
use std::collections::HashMap;

pub struct Day14;

type Rules = HashMap<(char, char), char>;
type Pairs = HashMap<(char, char), usize>;
type Problem = (char, Pairs, Rules);

fn parse_pairs(data: &str) -> Pairs {
    let mut pairs = Pairs::new();
    data.chars().collect::<Vec<_>>().windows(2).for_each(|x| {
        increment(&mut pairs, (x[0], x[1]), 1);
    });
    pairs
}

fn parse(data: &str) -> Problem {
    let mut lines = data.lines();
    let first = lines.next().unwrap();
    let pairs = parse_pairs(first);
    lines.next().unwrap();

    let mut rules = Rules::new();
    for line in lines {
        let mut chars = line.chars();
        let a = chars.next().unwrap();
        let b = chars.next().unwrap();
        (0..4).for_each(|_| {
            chars.next().unwrap();
        });
        let c = chars.next().unwrap();
        rules.insert((a, b), c);
    }

    (first.chars().last().unwrap(), pairs, rules)
}

fn increment<Key: std::hash::Hash + Eq>(
    counter: &mut HashMap<Key, usize>,
    key: Key,
    amount: usize,
) {
    let target = counter.entry(key).or_insert(0);
    *target += amount;
}

fn apply_rules(pairs: &Pairs, rules: &Rules) -> Pairs {
    let mut new_pairs = Pairs::new();
    for (&key, &value) in pairs {
        match rules.get(&key) {
            Some(&c) => {
                let (a, b) = key;
                increment(&mut new_pairs, (a, c), value);
                increment(&mut new_pairs, (c, b), value);
            }
            None => {
                new_pairs.insert(key, value);
            }
        }
    }
    new_pairs
}

fn apply_rules_multi(pairs: &Pairs, rules: &Rules, count: usize) -> Pairs {
    let mut pairs = pairs.clone();
    for _ in 0..count {
        pairs = apply_rules(&pairs, rules);
    }
    pairs
}

fn to_character_counts(pairs: &Pairs) -> HashMap<char, usize> {
    let mut counts = HashMap::new();
    for (&(a, _), &amount) in pairs {
        increment(&mut counts, a, amount);
    }
    counts
}

impl Solver<&str> for Day14 {
    fn part1(data: &str) -> Result<String> {
        let (last, pairs, rules) = parse(data);
        let pairs = apply_rules_multi(&pairs, &rules, 10);
        let mut counts = to_character_counts(&pairs);
        increment(&mut counts, last, 1);
        let min = *counts.values().min().unwrap();
        let max = *counts.values().max().unwrap();
        Ok((max - min).to_string())
    }

    fn part2(data: &str) -> Result<String> {
        let (last, pairs, rules) = parse(data);
        let pairs = apply_rules_multi(&pairs, &rules, 40);
        let mut counts = to_character_counts(&pairs);
        increment(&mut counts, last, 1);
        let min = *counts.values().min().unwrap();
        let max = *counts.values().max().unwrap();
        Ok((max - min).to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const DATA: &str = "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
";

    #[test]
    fn test_part1() {
        assert_eq!(Day14::part1(DATA).unwrap(), "1588");
    }

    #[test]
    fn test_part2() {
        assert_eq!(Day14::part2(DATA).unwrap(), "2188189693529");
    }
}
