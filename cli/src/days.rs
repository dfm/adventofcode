use crate::config::data_path;
use anyhow::Result;
use aoc::{InputHandler, Solver};

use aoc_day01::Day01;
use aoc_day02::Day02;
use aoc_day03::Day03;
// __USE

pub const MAX_DAY: u8 = 3;

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
        // __MATCH
        _ => Err(aoc::Error::NotImplemented.into()),
    }
}
