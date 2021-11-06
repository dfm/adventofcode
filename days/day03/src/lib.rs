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
        let mut over = 0;
        for x in 0..1000 {
            for y in 0..1000 {
                if counts[x][y] > 1 {
                    over += 1;
                }
            }
        }

        Ok(format!("{}", over))
    }

    fn part2(data: &Self::Data) -> Result<String> {
        let counts = count_overlaps(data);
        let result = data
            .iter()
            .filter(|v| no_overlap(&counts, v))
            .next()
            .context("finding no overlap")?;
        Ok(format!("{}", result.id))
    }
}

fn count_overlaps(data: &Vec<Vote>) -> Grid {
    let mut counts = [[0; 1000]; 1000];
    for v in data.iter() {
        for x in v.left..v.left + v.width {
            for y in v.top..v.top + v.height {
                counts[x][y] += 1;
            }
        }
    }
    counts
}

fn no_overlap(grid: &Grid, v: &Vote) -> bool {
    for x in v.left..v.left + v.width {
        for y in v.top..v.top + v.height {
            if grid[x][y] > 1 {
                return false;
            }
        }
    }
    return true;
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
