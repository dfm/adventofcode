use crate::helpers::CharGrid;
use rayon::prelude::*;
use std::collections::HashSet;

fn propagate(
  grid: &CharGrid,
  start: (i64, i64, i64, i64),
  visited: &mut HashSet<(i64, i64, i64, i64)>,
) {
  let (mut x, mut y, mut dx, mut dy) = start;
  while grid.in_bounds(x, y) && !visited.contains(&(x, y, dx, dy)) {
    visited.insert((x, y, dx, dy));
    let value = grid.get(x as usize, y as usize);
    if value == b'|' && dx != 0 {
      propagate(grid, (x, y + 1, 0, 1), visited);
      propagate(grid, (x, y - 1, 0, -1), visited);
      return;
    }
    if value == b'-' && dy != 0 {
      propagate(grid, (x + 1, y, 1, 0), visited);
      propagate(grid, (x - 1, y, -1, 0), visited);
      return;
    }
    (x, y, dx, dy) = match (value, dx, dy) {
      (b'\\', 1, 0) | (b'/', -1, 0) => (x, y + 1, 0, 1),
      (b'\\', -1, 0) | (b'/', 1, 0) => (x, y - 1, 0, -1),
      (b'\\', 0, -1) | (b'/', 0, 1) => (x - 1, y, -1, 0),
      (b'\\', 0, 1) | (b'/', 0, -1) => (x + 1, y, 1, 0),
      _ => (x + dx, y + dy, dx, dy),
    };
  }
}

fn energized(visited: &HashSet<(i64, i64, i64, i64)>) -> usize {
  visited
    .iter()
    .map(|(x, y, _, _)| (x, y))
    .collect::<HashSet<_>>()
    .len()
}

fn solve(grid: &CharGrid, start: (i64, i64, i64, i64)) -> usize {
  let mut visited = HashSet::new();
  propagate(grid, start, &mut visited);
  energized(&visited)
}

pub fn parse(data: &str) -> CharGrid {
  CharGrid::new(data)
}

pub fn part1(data: &CharGrid) -> usize {
  solve(data, (0, 0, 1, 0))
}

pub fn part2(data: &CharGrid) -> usize {
  let max_x = (0..data.width)
    .into_par_iter()
    .map(|x| {
      solve(data, (x as i64, 0, 0, 1)).max(solve(data, (x as i64, (data.height - 1) as i64, 0, -1)))
    })
    .max()
    .unwrap();
  let max_y = (0..data.height)
    .into_par_iter()
    .map(|y| {
      solve(data, (0, y as i64, 1, 0)).max(solve(data, ((data.width - 1) as i64, y as i64, -1, 0)))
    })
    .max()
    .unwrap();
  max_x.max(max_y)
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA: &str = r".|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA);
    assert_eq!(part1(&data), 46);
  }

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA);
    assert_eq!(part2(&data), 51);
  }
}
