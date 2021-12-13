use anyhow::Result;
use aoc::solver::Solver;
use regex::Regex;
use std::collections::HashSet;

pub struct Day13;

#[derive(Debug, Clone)]
struct Paper {
    dots: Vec<(i32, i32)>,
    folds: Vec<(bool, i32)>,
}

impl Paper {
    fn new(data: &str) -> Self {
        let dot_re = Regex::new("^(?P<x>[0-9]+),(?P<y>[0-9]+)$").unwrap();
        let fold_re = Regex::new("^fold along (?P<axis>[a-z])=(?P<value>[0-9]+)$").unwrap();
        let mut dots = Vec::new();
        let mut folds = Vec::new();
        for line in data.lines() {
            if let Some(c) = dot_re.captures(line) {
                dots.push((
                    c.name("x").unwrap().as_str().parse().unwrap(),
                    c.name("y").unwrap().as_str().parse().unwrap(),
                ));
            } else if let Some(c) = fold_re.captures(line) {
                let axis = c.name("axis").unwrap().as_str();
                let value = c.name("value").unwrap().as_str().parse().unwrap();
                folds.push((axis == "x", value));
            }
        }
        Self { dots, folds }
    }

    fn fold(&mut self, (axis, value): (bool, i32)) {
        for (x, y) in self.dots.iter_mut() {
            if axis {
                *x = fold_impl(value, *x);
            } else {
                *y = fold_impl(value, *y);
            }
        }
    }

    fn folds(&mut self) {
        for (axis, value) in self.folds.iter() {
            for (x, y) in self.dots.iter_mut() {
                if *axis {
                    *x = fold_impl(*value, *x);
                } else {
                    *y = fold_impl(*value, *y);
                }
            }
        }
    }

    fn unique(&self) -> HashSet<(i32, i32)> {
        let mut unique = HashSet::new();
        for &dot in self.dots.iter() {
            unique.insert(dot);
        }
        unique
    }
}

fn fold_impl(x0: i32, x: i32) -> i32 {
    let delta = x - x0;
    if delta > 0 {
        x0 - delta
    } else {
        x
    }
}

impl std::fmt::Display for Paper {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let dots = self.unique();
        let (x_min, y_min) = dots.iter().fold((i32::MAX, i32::MAX), |(x0, y0), (x, y)| {
            (std::cmp::min(x0, *x), std::cmp::min(y0, *y))
        });
        let (x_max, y_max) = dots.iter().fold((i32::MIN, i32::MIN), |(x0, y0), (x, y)| {
            (std::cmp::max(x0, *x), std::cmp::max(y0, *y))
        });
        let mut result = String::new();
        result.push('\n');
        for y in y_min..=y_max {
            for x in x_min..=x_max {
                if dots.contains(&(x, y)) {
                    result.push('#');
                } else {
                    result.push('.');
                }
            }
            result.push('\n');
        }
        write!(f, "{}", result)
    }
}

impl Solver<&str> for Day13 {
    fn part1(data: &str) -> Result<String> {
        let mut paper = Paper::new(data);
        paper.fold(paper.folds[0]);
        Ok(paper.unique().len().to_string())
    }

    fn part2(data: &str) -> Result<String> {
        let mut paper = Paper::new(data);
        paper.folds();
        Ok(paper.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const DATA: &str = "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
";

    #[test]
    fn test_part1() {
        assert_eq!(Day13::part1(DATA).unwrap(), "17")
    }

    #[test]
    fn test_part2() {
        assert_eq!(
            Day13::part2(DATA).unwrap(),
            "\n#####\n#...#\n#...#\n#...#\n#####\n"
        )
    }
}
