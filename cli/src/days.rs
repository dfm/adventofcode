use anyhow::{Context, Result};
use aoc::solver::Solver;
use std::env;
use std::fs;
use std::path::Path;

use aoc_day01::Day01;
use aoc_day02::Day02;
// __USE

macro_rules! run_solver {
    ( $day:tt, $data:expr ) => {
        Ok({
            let parsed = $day::parse(&$data)?;
            println!(" -> part 1: {}", $day::part1(&parsed)?);
            println!(" -> part 2: {}", $day::part2(&parsed)?);
        })
    };
}

pub fn run_day(day: u8) -> Result<()> {
    println!("day: {}", day);

    // Read the input data file
    let dir = env::var("CARGO_MANIFEST_DIR")?;
    let path = Path::new(&dir);
    let path = path.parent().context("parent directory")?;
    let filename = path.join(format!("data/2018/{:02}", day));
    let data = fs::read_to_string(&filename).context(format!("{:?}", filename))?;

    // Execute the solver on these data
    match day {
        1 => run_solver!(Day01, data),
        2 => run_solver!(Day02, data),
        // __MATCH
        _ => Err(aoc::Error::NotImplemented.into()),
    }
}
