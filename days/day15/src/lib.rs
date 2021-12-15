use aoc::solver::Solver;
use std::collections::VecDeque;

pub struct Day15;

struct Grid {
    width: usize,
    height: usize,
    risk: Vec<usize>,
}

impl Grid {
    fn new(data: &str, factor: u8) -> Self {
        let width = data.lines().next().unwrap().trim().len() * (factor as usize);
        let mut grid = Grid {
            width,
            height: 0,
            risk: Vec::new(),
        };
        for fy in 0..factor {
            for line in data.lines() {
                for fx in 0..factor {
                    for c in line.trim().chars() {
                        grid.risk
                            .push(((c as u8 - b'0' + fy + fx - 1) % 9 + 1) as usize);
                    }
                }
            }
        }
        grid.height = grid.risk.len() / width;
        grid
    }

    fn idx(&self, x: usize, y: usize) -> usize {
        y * self.width + x
    }

    fn in_bounds(&self, x: usize, y: usize, dx: i32, dy: i32) -> Option<(usize, usize, usize)> {
        let x = x as i32 + dx;
        let y = y as i32 + dy;
        if x >= 0 && x < self.width as i32 && y >= 0 && y < self.height as i32 {
            Some((x as usize, y as usize, self.idx(x as usize, y as usize)))
        } else {
            None
        }
    }

    fn dijkstra(&self) -> usize {
        let mut distance = Vec::new();
        distance.resize(self.risk.len(), usize::MAX);
        let mut heap = VecDeque::new();
        heap.push_back((0, (0, 0)));

        while let Some((cost, (x, y))) = heap.pop_front() {
            if cost > distance[self.idx(x, y)] {
                continue;
            }
            for (dx, dy) in [(0, -1), (-1, 0), (1, 0), (0, 1)] {
                if let Some((x_, y_, idx)) = self.in_bounds(x, y, dx, dy) {
                    let new_cost = cost + self.risk[idx];
                    if new_cost < distance[idx] {
                        heap.push_back((new_cost, (x_, y_)));
                        distance[idx] = new_cost;
                    }
                }
            }
        }

        distance[self.idx(self.width - 1, self.height - 1)]
    }
}

impl std::fmt::Display for Grid {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut result = String::new();
        for y in 0..self.height {
            for x in 0..self.width {
                let line = format!("{}", self.risk[self.idx(x, y)]);
                result.push_str(&line);
            }
            result.push('\n');
        }
        write!(f, "{}", result)
    }
}

impl Solver<&str> for Day15 {
    fn part1(data: &str) -> usize {
        let grid = Grid::new(data, 1);
        grid.dijkstra()
    }

    fn part2(data: &str) -> usize {
        let grid = Grid::new(data, 5);
        grid.dijkstra()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const DATA: &str = "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
";

    #[test]
    fn test_part1() {
        assert_eq!(Day15::part1(DATA), 40);
    }

    #[test]
    fn test_part2() {
        assert_eq!(Day15::part2(DATA), 315);
    }
}
