use anyhow::Result;
use aoc::solver::Solver;
use std::collections::HashSet;

pub struct Day01 {}

impl Solver for Day01 {
    type Data = Vec<i32>;

    fn parse(input: &str) -> Result<Self::Data> {
        let tokens: std::result::Result<Vec<_>, _> = input
            .split_whitespace()
            .map(|x| x.trim().parse::<i32>())
            .collect();
        Ok(tokens?)
    }

    fn part1(data: &Self::Data) -> Result<String> {
        let result: i32 = data.iter().sum();
        Ok(format!("{}", result))
    }

    fn part2(data: &Self::Data) -> Result<String> {
        let mut seen = HashSet::new();
        let mut current = 0;
        for n in data.iter().cycle() {
            current += n;
            if seen.contains(&current) {
                break;
            }
            seen.insert(current);
        }
        Ok(format!("{}", current))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parsing() {
        let data = Day01::parse("+1\n-1\n+1").unwrap();
        assert_eq!(data.len(), 3);
        assert_eq!(data[0], 1);
        assert_eq!(data[1], -1);
        assert_eq!(data[2], 1);
    }

    #[test]
    fn test_part1() {
        let data = Day01::parse("+1\n-2\n+3\n+1").unwrap();
        assert_eq!(Day01::part1(&data).unwrap(), "3");
    }

    #[test]
    fn test_part2() {
        let data = Day01::parse("+1\n-2\n+3\n+1").unwrap();
        assert_eq!(Day01::part2(&data).unwrap(), "2");
    }
}
