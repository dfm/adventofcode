use aoc::solver::Solver;
use aoc::{Error, Result};

use aoc_day01::Day01;
// __USE

macro_rules! run_solver {
    ( $day:tt ) => {{
        let data = $day::parse("face")?;
        $day::part1(&data)?;
        $day::part2(&data)
    }};
}

pub fn run_day(day: u8) -> Result<()> {
    match day {
        1 => run_solver!(Day01),
        // __MATCH
        _ => Err(Error::new_not_implemented()),
    }
}
