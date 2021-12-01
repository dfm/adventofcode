use anyhow::Result;
use aoc::solver::Solver;

pub struct Day{{ day }};

impl Solver for Day{{ day }} {
    type Data = Vec<i32>;

    fn parse(input: &str) -> Result<Self::Data> {
        let tokens: std::result::Result<Vec<_>, _> = input
            .split_whitespace()
            .map(|x| x.trim().parse::<i32>())
            .collect();
        Ok(tokens?)
    }

    fn part1(_data: &Self::Data) -> Result<String> {
        Err(aoc::Error::NotImplemented.into())
    }

    fn part2(_data: &Self::Data) -> Result<String> {
        Err(aoc::Error::NotImplemented.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {}

    #[test]
    fn test_part1() {}

    #[test]
    fn test_part2() {}
}
