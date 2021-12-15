use anyhow::{Context, Error, Result};
use aoc::counter::Counter;
use aoc::solver::Solver;
use regex::Regex;
use std::collections::HashMap;

pub struct Day05;

type Int = i32;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub struct Point {
    x: Int,
    y: Int,
}

#[derive(Debug, Copy, Clone)]
pub struct Line(Point, Point);

impl Line {
    fn get_trajectory(self) -> impl Iterator<Item = Point> {
        let x1 = self.0.x;
        let y1 = self.0.y;
        let dx = (self.1.x - x1).signum();
        let dy = (self.1.y - y1).signum();
        let tot = std::cmp::max((self.1.x - x1) * dx, (self.1.y - y1) * dy);
        (0..=tot).map(move |n| Point {
            x: x1 + dx * n,
            y: y1 + dy * n,
        })
    }
}

impl std::str::FromStr for Line {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let re = Regex::new("^(?P<x1>[0-9]+),(?P<y1>[0-9]+) -> (?P<x2>[0-9]+),(?P<y2>[0-9]+)$")?;
        let c = re.captures(s).context("Matching regex")?;
        let x1 = c.name("x1").context("Parsing x1")?.as_str().parse()?;
        let y1 = c.name("y1").context("Parsing y1")?.as_str().parse()?;
        let x2 = c.name("x2").context("Parsing x2")?.as_str().parse()?;
        let y2 = c.name("y2").context("Parsing y2")?.as_str().parse()?;
        Ok(Line(Point { x: x1, y: y1 }, Point { x: x2, y: y2 }))
    }
}

fn solve(data: Vec<Line>, filter: fn(&Line) -> bool) -> usize {
    let mut points = HashMap::new();
    for line in data.into_iter().filter(filter) {
        for point in line.get_trajectory() {
            points.increment(point, 1);
        }
    }
    points.into_values().filter(|&c| c >= 2).count()
}

impl Solver<Vec<Line>> for Day05 {
    fn part1(data: Vec<Line>) -> usize {
        solve(data, |&line| line.0.x == line.1.x || line.0.y == line.1.y)
    }

    fn part2(data: Vec<Line>) -> usize {
        solve(data, |&_| true)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn get_data() -> Vec<Line> {
        let input = "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
";
        let handler = &aoc::InputHandler::new(&input);
        handler.into()
    }

    #[test]
    fn test_parse() {
        let data = get_data();
        assert_eq!(data.len(), 10);
        assert_eq!(data[0].0.x, 0);
        assert_eq!(data[4].1.y, 4);
        assert_eq!(data[0].get_trajectory().count(), 6);
    }

    #[test]
    fn test_part1() {
        let data = get_data();
        assert_eq!(Day05::part1(data), 5);
    }

    #[test]
    fn test_part2() {
        let data = get_data();
        assert_eq!(Day05::part2(data), 12);
    }
}
