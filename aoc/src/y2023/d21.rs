use crate::helpers::{shortest_paths, CharGrid, ShortestPath};
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct Data {
  grid: CharGrid,
  start: (i64, i64),
}

impl Data {
  fn new(grid: CharGrid) -> Self {
    let idx = grid
      .data
      .iter()
      .enumerate()
      .find(|(_, &c)| c == b'S')
      .unwrap()
      .0;
    let (x, y) = grid.to_coords(idx);
    Self {
      grid,
      start: (x as i64, y as i64),
    }
  }

  fn get(&self, x: i64, y: i64) -> Option<u8> {
    self.grid.get_checked(
      wrap(x + self.start.0, self.grid.width),
      wrap(y + self.start.1, self.grid.height),
    )
  }

  fn paths(&self, radius: usize, start: (i64, i64)) -> HashMap<(i64, i64), usize> {
    let starts = vec![start];
    shortest_paths(self.clone(), &starts, radius)
  }

  fn solve(&self, radius: usize) -> usize {
    let paths = self.paths(radius, (0, 0));
    paths.iter().filter(|(_, &d)| d % 2 == radius % 2).count()
  }
}

impl ShortestPath for Data {
  type Coord = (i64, i64);
  fn neighbors(&self, current: &Self::Coord) -> Vec<(Self::Coord, usize)> {
    let (x, y) = *current;
    let mut neighbors = Vec::new();
    for (dx, dy) in [(0, -1), (-1, 0), (1, 0), (0, 1)] {
      match self.get(x + dx, y + dy) {
        Some(c) if c == b'.' || c == b'S' => neighbors.push(((x + dx, y + dy), 1)),
        _ => {}
      }
    }
    neighbors
  }
}

#[inline]
fn wrap(x: i64, rng: usize) -> i64 {
  let mut x = x;
  while x < 0 {
    x += rng as i64;
  }
  x % rng as i64
}

pub fn parse(data: &str) -> CharGrid {
  CharGrid::new(data)
}

pub fn part1(data: &CharGrid) -> usize {
  let radius = if data.width == 11 { 6 } else { 64 };
  let data = Data::new(data.clone());
  data.solve(radius)
}

pub fn part2(data: &CharGrid) -> i64 {
  let size = data.width;
  let data = Data::new(data.clone());

  let a1 = data.solve(size / 2) as i64;
  let a2 = data.solve(size / 2 + size) as i64;
  let a3 = data.solve(size / 2 + 2 * size) as i64;

  // Solved using sympy
  let t = ((26501365 - size / 2) / size) as i64;
  t * t * (a1 - 2 * a2 + a3) / 2 - t * (3 * a1 - 4 * a2 + a3) / 2 + a1
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA: &str = r"...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA);
    assert_eq!(part1(&data), 16);
  }
}
