use crate::config::data_path;
use anyhow::Result;
use aoc::{InputHandler, Solver};

use aoc_day01::Day01;
use aoc_day02::Day02;
use aoc_day03::Day03;
use aoc_day04::Day04;
use aoc_day05::Day05;
use aoc_day06::Day06;
// __USE

pub const MAX_DAY: u8 = 6;

macro_rules! run_solver {
    ( $day:tt, $data:expr ) => {
        Ok({
            // let parsed = $day::parse(&$data)?;
            println!(" -> part 1: {}", $day::part1($data)?);
            println!(" -> part 2: {}\n", $day::part2($data)?);
        })
    };
}

pub fn run_day(day: u8) -> Result<()> {
    println!("=> DAY {:02}", day);

    // Read the input data file
    let filename = data_path(day)?;
    let handler = &InputHandler::from_file(&filename)?;
    // let data = fs::read_to_string(&filename).context(format!("{:?}", filename))?;

    // Execute the solver on these data
    match day {
        1 => run_solver!(Day01, handler.into()),
        2 => run_solver!(Day02, handler.into()),
        3 => run_solver!(Day03, handler.into()),
        4 => run_solver!(Day04, handler.into()),
        5 => run_solver!(Day05, handler.into()),
        6 => run_solver!(Day06, handler.into()),
        // __MATCH
        _ => Err(aoc::Error::NotImplemented.into()),
    }
}
