use anyhow::{Context, Result};
use aoc::solver::Solver;
use regex::Regex;

pub struct Day03 {}

type Grid = [[u8; 1000]; 1000];

#[derive(Debug)]
pub struct Vote {
    id: usize,
    left: usize,
    top: usize,
    width: usize,
    height: usize,
}

impl Solver for Day03 {
    type Data = Vec<Vote>;

    fn parse(input: &str) -> Result<Self::Data> {
        let re = Regex::new(
            r"(?m:^\#(?P<id>\d+) @ (?P<left>\d+),(?P<top>\d+): (?P<width>\d+)x(?P<height>\d+)$)",
        )?;
        re.captures_iter(input)
            .map(|m| {
                Ok(Vote {
                    id: m["id"].parse::<usize>()?,
                    left: m["left"].parse::<usize>()?,
                    top: m["top"].parse::<usize>()?,
                    width: m["width"].parse::<usize>()?,
                    height: m["height"].parse::<usize>()?,
                })
            })
            .collect()
    }

    fn part1(data: &Self::Data) -> Result<String> {
        let counts = count_overlaps(data);
        let over: usize = counts
            .iter()
            .map(|row| row.iter().filter(|&v| *v > 1).count())
            .sum();
        Ok(format!("{}", over))
    }

    fn part2(data: &Self::Data) -> Result<String> {
        let counts = count_overlaps(data);
        let result = data
            .iter()
            .find(|v| no_overlap(&counts, v))
            .context("finding no overlap")?;
        Ok(format!("{}", result.id))
    }
}

fn count_overlaps(data: &[Vote]) -> Grid {
    let mut counts = [[0; 1000]; 1000];
    data.iter().for_each(|v| {
        counts
            .iter_mut()
            .skip(v.left)
            .take(v.width)
            .for_each(|row| {
                (*row)
                    .iter_mut()
                    .skip(v.top)
                    .take(v.height)
                    .for_each(|value| *value += 1);
            });
    });
    counts
}

fn no_overlap(grid: &Grid, v: &Vote) -> bool {
    grid.iter()
        .skip(v.left)
        .take(v.width)
        .filter_map(|row| row.iter().skip(v.top).take(v.height).find(|&x| *x > 1))
        .next()
        .is_none()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parsing() {
        let data = Day03::parse("#123 @ 3,2: 5x4\n#5 @ 10,200: 50x41").unwrap();
        assert_eq!(data.len(), 2);
        assert_eq!(data[0].id, 123);
        assert_eq!(data[1].top, 200);
    }

    #[test]
    fn test_part1() {
        let data = Day03::parse(
            "#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2
",
        )
        .unwrap();
        assert_eq!(Day03::part1(&data).unwrap(), "4");
    }

    #[test]
    fn test_part2() {
        let data = Day03::parse(
            "#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2
",
        )
        .unwrap();
        assert_eq!(Day03::part2(&data).unwrap(), "3");
    }
}
