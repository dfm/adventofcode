use crate::config::data_path;
use anyhow::Result;
use aoc::{InputHandler, Solver};

use aoc_day01::Day01;
use aoc_day02::Day02;
use aoc_day03::Day03;
use aoc_day04::Day04;
use aoc_day05::Day05;
use aoc_day06::Day06;
use aoc_day07::Day07;
use aoc_day08::Day08;
use aoc_day09::Day09;
use aoc_day10::Day10;
// __USE

pub const MAX_DAY: u8 = 10;

macro_rules! run_solver {
    ( $day:tt, $data:expr ) => {
        Ok(($day::part1($data)?, $day::part2($data)?))
    };
}

pub fn run_day(day: u8) -> Result<(String, String)> {
    let handler = load_data(day)?;
    run_day_with_data(day, &handler)
}

pub fn load_data(day: u8) -> Result<InputHandler> {
    let filename = data_path(day)?;
    InputHandler::from_file(&filename)
}

pub fn run_day_with_data(day: u8, handler: &InputHandler) -> Result<(String, String)> {
    match day {
        1 => run_solver!(Day01, handler.into()),
        2 => run_solver!(Day02, handler.into()),
        3 => run_solver!(Day03, handler.into()),
        4 => run_solver!(Day04, handler.into()),
        5 => run_solver!(Day05, handler.into()),
        6 => run_solver!(Day06, handler.into()),
        7 => run_solver!(Day07, handler.into()),
        8 => run_solver!(Day08, handler.into()),
        9 => run_solver!(Day09, handler.into()),
        10 => run_solver!(Day10, handler.into()),
        // __MATCH
        _ => Err(aoc::Error::NotImplemented.into()),
    }
}
