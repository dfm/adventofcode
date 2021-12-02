use anyhow::{Context, Result};
use aoc::solver::Solver;
use regex::Regex;

pub struct Day02;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Dir {
    Forward,
    Down,
    Up,
}

#[derive(Debug, Copy, Clone)]
pub struct Instr(Dir, i32);

impl Solver for Day02 {
    type Data = Vec<Instr>;

    fn parse(input: &str) -> Result<Self::Data> {
        let re = Regex::new("(?m:^(?P<dir>[a-z]+) (?P<amt>[0-9]+)$)")?;
        let tokens: Option<Vec<_>> = re
            .captures_iter(input)
            .map(|c| {
                let dir = c.name("dir").unwrap().as_str();
                let amt = c.name("amt").unwrap().as_str().parse::<i32>().unwrap();
                match dir {
                    "forward" => Some(Instr(Dir::Forward, amt)),
                    "down" => Some(Instr(Dir::Down, amt)),
                    "up" => Some(Instr(Dir::Up, amt)),
                    _ => None,
                }
            })
            .collect();
        tokens.context("parsing")
    }

    fn part1(data: &Self::Data) -> Result<String> {
        let result = data.iter().fold((0, 0), |(x, y), instr| match instr.0 {
            Dir::Forward => (x + instr.1, y),
            Dir::Down => (x, y + instr.1),
            Dir::Up => (x, y - instr.1),
        });
        Ok(format!("{}", result.0 * result.1))
    }

    fn part2(data: &Self::Data) -> Result<String> {
        let result = data
            .iter()
            .fold((0, 0, 0), |(aim, x, y), instr| match instr.0 {
                Dir::Forward => (aim, x + instr.1, y + aim * instr.1),
                Dir::Down => (aim + instr.1, x, y),
                Dir::Up => (aim - instr.1, x, y),
            });
        Ok(format!("{}", result.1 * result.2))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let input = "forward 5
down 5
forward 8
up 3
down 8
forward 2
";
        let data = Day02::parse(input).unwrap();
        assert_eq!(data.len(), 6);
        assert_eq!(data[0].0, Dir::Forward);
        assert_eq!(data[0].1, 5);
        assert_eq!(data[1].0, Dir::Down);
        assert_eq!(data[1].1, 5);
        assert_eq!(data[3].0, Dir::Up);
        assert_eq!(data[3].1, 3);
    }

    #[test]
    fn test_part1() {
        let input = "forward 5
down 5
forward 8
up 3
down 8
forward 2
";
        let data = Day02::parse(input).unwrap();
        assert_eq!(Day02::part1(&data).unwrap(), "150");
    }

    #[test]
    fn test_part2() {
        let input = "forward 5
down 5
forward 8
up 3
down 8
forward 2
";
        let data = Day02::parse(input).unwrap();
        assert_eq!(Day02::part2(&data).unwrap(), "900");
    }
}
