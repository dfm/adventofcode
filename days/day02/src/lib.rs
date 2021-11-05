use anyhow::Result;
use aoc::solver::Solver;
use std::collections::{HashMap, HashSet};

pub struct Day02 {}

impl Solver for Day02 {
    type Data = String;

    fn parse(input: &str) -> Result<Self::Data> {
        Ok(input.to_string())
    }

    fn part1(data: &Self::Data) -> Result<String> {
        let (two, three) = data
            .lines()
            .map(count_freq)
            .fold((0, 0), |(a, b), (x, y)| (a + x, b + y));
        Ok(format!("{}", two * three))
    }

    fn part2(_data: &Self::Data) -> Result<String> {
        Err(aoc::Error::NotImplemented.into())
    }
}

fn char_freq(word: &str) -> HashMap<char, u32> {
    let mut map = HashMap::new();
    for char in word.chars() {
        *map.entry(char).or_insert(0) += 1;
    }
    map
}

fn count_freq(word: &str) -> (i32, i32) {
    let counts: Vec<u32> = char_freq(word).into_values().collect();
    (counts.contains(&2).into(), counts.contains(&3).into())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        let data = String::from(
            "abcdef
bababc
abbcde
abcccd
aabcdd
abcdee
ababab",
        );
        assert_eq!(Day02::part1(&data).unwrap(), "12");
    }
}
