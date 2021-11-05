use anyhow::Result;
use aoc::solver::Solver;
use std::collections::HashMap;

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

    fn part2(data: &Self::Data) -> Result<String> {
        let words: Vec<&str> = data.lines().collect();
        for (i, word1) in words.iter().enumerate() {
            for word2 in words[i + 1..].iter() {
                if differ_by_one(word1, word2) {
                    return Ok(common_letters(word1, word2));
                }
            }
        }
        Ok(String::from("failed to solve"))
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

fn differ_by_one(word1: &str, word2: &str) -> bool {
    word1
        .chars()
        .zip(word2.chars())
        .map(|(c1, c2)| if c1 == c2 { 0 } else { 1 })
        .sum::<i32>()
        == 1
}

fn common_letters(word1: &str, word2: &str) -> String {
    word1
        .chars()
        .zip(word2.chars())
        .filter(|(c1, c2)| c1 == c2)
        .map(|(c1, _)| c1)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_differ_by_one() {
        assert!(differ_by_one("fghij", "fguij"));
    }

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

    #[test]
    fn test_part2() {
        let data = String::from(
            "abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz
",
        );
        assert_eq!(Day02::part2(&data).unwrap(), "fgij");
    }
}
