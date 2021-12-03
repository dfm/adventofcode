use anyhow::{Context, Error, Result};
use aoc::Solver;
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

impl std::str::FromStr for Instr {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let re = Regex::new("^(?P<dir>[a-z]+) (?P<amt>[0-9]+)$")?;
        let c = re.captures(s).context("Matching regex")?;
        let dir = c.name("dir").unwrap().as_str();
        let amt = c.name("amt").unwrap().as_str().parse::<i32>().unwrap();
        match dir {
            "forward" => Ok(Instr(Dir::Forward, amt)),
            "down" => Ok(Instr(Dir::Down, amt)),
            "up" => Ok(Instr(Dir::Up, amt)),
            _ => unreachable!("Invalid direction"),
        }
    }
}

impl Solver<Vec<Instr>> for Day02 {
    fn part1(data: Vec<Instr>) -> Result<String> {
        let result = data.iter().fold((0, 0), |(x, y), instr| match instr.0 {
            Dir::Forward => (x + instr.1, y),
            Dir::Down => (x, y + instr.1),
            Dir::Up => (x, y - instr.1),
        });
        Ok(format!("{}", result.0 * result.1))
    }

    fn part2(data: Vec<Instr>) -> Result<String> {
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

    fn get_data() -> Vec<Instr> {
        let input = "forward 5
down 5
forward 8
up 3
down 8
forward 2
";
        let handler = &aoc::InputHandler::new(&input);
        handler.into()
    }

    #[test]
    fn test_parse() {
        let data = get_data();
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
        let data = get_data();
        assert_eq!(Day02::part1(data).unwrap(), "150");
    }

    #[test]
    fn test_part2() {
        let data = get_data();
        assert_eq!(Day02::part2(data).unwrap(), "900");
    }
}
