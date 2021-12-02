use crate::config::data_path;
use anyhow::{Context, Result};
use aoc::solver::Solver;
use std::fs;

use aoc_day01::Day01;
use aoc_day02::Day02;
// __USE

pub const MAX_DAY: u8 = 2;

macro_rules! run_solver {
    ( $day:tt, $data:expr ) => {
        Ok({
            let parsed = $day::parse(&$data)?;
            println!(" -> part 1: {}", $day::part1(&parsed)?);
            println!(" -> part 2: {}\n", $day::part2(&parsed)?);
        })
    };
}

pub fn run_day(day: u8) -> Result<()> {
    println!("=> DAY {:02}", day);

    // Read the input data file
    let filename = data_path(day)?;
    let data = fs::read_to_string(&filename).context(format!("{:?}", filename))?;

    // Execute the solver on these data
    match day {
        1 => run_solver!(Day01, data),
        2 => run_solver!(Day02, data),
        // __MATCH
        _ => Err(aoc::Error::NotImplemented.into()),
    }
}