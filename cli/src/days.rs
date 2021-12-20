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
use aoc_day11::Day11;
use aoc_day12::Day12;
use aoc_day13::Day13;
use aoc_day14::Day14;
use aoc_day15::Day15;
use aoc_day16::Day16;
use aoc_day17::Day17;
use aoc_day18::Day18;
use aoc_day19::Day19;
use aoc_day20::Day20;
// __USE

pub const MAX_DAY: u8 = 20;

macro_rules! run_solver {
    ( $day:tt, $data:expr ) => {
        (
            $day::part1($data).to_string(),
            $day::part2($data).to_string(),
        )
    };
}

pub fn run_day(day: u8) -> (String, String) {
    let handler = load_data(day).unwrap();
    run_day_with_data(day, &handler)
}

pub fn load_data(day: u8) -> Result<InputHandler> {
    let filename = data_path(day)?;
    InputHandler::from_file(&filename)
}

pub fn run_day_with_data(day: u8, handler: &InputHandler) -> (String, String) {
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
        11 => run_solver!(Day11, handler.into()),
        12 => run_solver!(Day12, handler.into()),
        13 => run_solver!(Day13, handler.into()),
        14 => run_solver!(Day14, handler.into()),
        15 => run_solver!(Day15, handler.into()),
        16 => run_solver!(Day16, handler.into()),
        17 => run_solver!(Day17, handler.into()),
        18 => run_solver!(Day18, handler.into()),
        19 => run_solver!(Day19, handler.into()),
        20 => run_solver!(Day20, handler.into()),
        // __MATCH
        _ => unreachable!(),
    }
}
