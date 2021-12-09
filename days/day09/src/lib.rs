use anyhow::Result;
use aoc::solver::Solver;

pub struct Day09;

#[derive(Debug, Clone)]
struct Grid {
    width: usize,
    height: usize,
    data: Vec<u8>,
    visited: Vec<bool>,
}

impl Grid {
    fn new(data: &str) -> Self {
        let width = data.lines().next().unwrap().trim().len() + 2;
        let mut grid = Grid {
            width,
            height: 0,
            data: Vec::new(),
            visited: Vec::new(),
        };
        grid.data.resize(width, 10);
        for line in data.lines() {
            grid.data.push(10);
            for c in line.trim().chars() {
                grid.data.push(c as u8 - b'0');
            }
            grid.data.push(10);
        }
        grid.data.resize(grid.data.len() + width, 10);
        grid.height = grid.data.len() / width;
        grid.visited.resize(grid.data.len(), false);
        grid
    }

    fn idx(&self, x: usize, y: usize) -> usize {
        y * self.width + x
    }

    fn at(&self, x: usize, y: usize) -> u8 {
        self.data[self.idx(x, y)]
    }

    fn find_minima(&self) -> (usize, Vec<(usize, usize)>) {
        let mut minima = Vec::new();
        let mut risk: usize = 0;
        for y in 1..self.height - 1 {
            for x in 1..self.width - 1 {
                let target = self.at(x, y);
                if self.at(x, y - 1) > target
                    && self.at(x - 1, y) > target
                    && self.at(x + 1, y) > target
                    && self.at(x, y + 1) > target
                {
                    risk += 1 + target as usize;
                    minima.push((x, y));
                }
            }
        }
        (risk, minima)
    }

    fn expand_basin(&mut self, x: usize, y: usize) -> usize {
        let idx = self.idx(x, y);
        if self.visited[idx] || self.data[idx] >= 9 {
            0
        } else {
            self.visited[idx] = true;
            1 + self.expand_basin(x, y - 1)
                + self.expand_basin(x - 1, y)
                + self.expand_basin(x + 1, y)
                + self.expand_basin(x, y + 1)
        }
    }
}

impl Solver<&str> for Day09 {
    fn part1(data: &str) -> Result<String> {
        let grid = Grid::new(data);
        Ok(grid.find_minima().0.to_string())
    }

    fn part2(data: &str) -> Result<String> {
        let mut grid = Grid::new(data);
        let (_, minima) = grid.find_minima();
        let mut sizes = Vec::new();
        for (x, y) in minima {
            sizes.push(grid.expand_basin(x, y));
        }
        sizes.sort_unstable();
        let result: usize = sizes.iter().rev().take(3).product();
        Ok(result.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = "2199943210
3987894921
9856789892
8767896789
9899965678
";

    #[test]
    fn test_part1() {
        assert_eq!(Day09::part1(TEST_DATA).unwrap(), "15");
    }

    #[test]
    fn test_part2() {
        assert_eq!(Day09::part2(TEST_DATA).unwrap(), "1134");
    }
}
