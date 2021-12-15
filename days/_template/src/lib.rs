use anyhow::Result;
use aoc::solver::Solver;

pub struct Day{{ day }};

impl Solver<&str> for Day{{ day }} {
    fn part1(data: &str) -> usize {
        0
    }

    fn part2(data: &str) -> usize {
        0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const DATA: &str = "";

    #[test]
    fn test_part1() {
        assert_eq!(Day{{ day }}::part1(DATA), 0);
    }

    #[test]
    fn test_part2() {
        assert_eq!(Day{{ day }}::part2(DATA), 0);
    }
}
