use anyhow::Result;
use aoc::counter::Counter;
use aoc::solver::Solver;
use std::collections::HashMap;

pub struct Day14;

type Rules = HashMap<(char, char), char>;
type Pairs = HashMap<(char, char), usize>;
type Problem = (char, Pairs, Rules);

fn parse_pairs(data: &str) -> Pairs {
    Pairs::counter_from_iter(
        data.chars()
            .collect::<Vec<_>>()
            .windows(2)
            .map(|x| (x[0], x[1])),
    )
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

fn apply_rules(pairs: &Pairs, rules: &Rules) -> Pairs {
    let mut new_pairs = Pairs::new();
    for (&key, &value) in pairs {
        match rules.get(&key) {
            Some(&c) => {
                let (a, b) = key;
                new_pairs.increment((a, c), value);
                new_pairs.increment((c, b), value);
            }
            None => {
                new_pairs.insert(key, value);
            }
        }
    }
    new_pairs
}

fn apply_rules_multi(pairs: &Pairs, rules: &Rules, num_iter: usize) -> Pairs {
    let mut pairs = pairs.clone();
    for _ in 0..num_iter {
        pairs = apply_rules(&pairs, rules);
    }
    pairs
}

fn to_character_counts(pairs: &Pairs) -> HashMap<char, usize> {
    let mut counts = HashMap::new();
    for (&(a, _), &amount) in pairs {
        counts.increment(a, amount);
    }
    counts
}

fn solve(data: &str, num_iter: usize) -> usize {
    let (last, pairs, rules) = parse(data);
    let pairs = apply_rules_multi(&pairs, &rules, num_iter);
    let mut counts = to_character_counts(&pairs);
    counts.increment(last, 1);
    let min = *counts.values().min().unwrap();
    let max = *counts.values().max().unwrap();
    max - min
}

impl Solver<&str> for Day14 {
    fn part1(data: &str) -> Result<String> {
        Ok(solve(data, 10).to_string())
    }

    fn part2(data: &str) -> Result<String> {
        Ok(solve(data, 40).to_string())
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
