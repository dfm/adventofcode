use crate::helpers::CharGrid;
use rayon::prelude::*;
use std::collections::HashSet;

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
enum Dir {
  Right,
  Left,
  Up,
  Down,
}

fn propagate(grid: &CharGrid, start: (i64, i64, Dir), visited: &mut HashSet<(i64, i64, Dir)>) {
  let (mut x, mut y, mut d) = start;
  while grid.in_bounds(x, y) && !visited.contains(&(x, y, d)) {
    visited.insert((x, y, d));
    let value = grid.get(x as usize, y as usize);
    if matches!((value, d), (b'|', Dir::Right | Dir::Left)) {
      propagate(grid, (x, y + 1, Dir::Down), visited);
      propagate(grid, (x, y - 1, Dir::Up), visited);
      return;
    }
    if matches!((value, d), (b'-', Dir::Up | Dir::Down)) {
      propagate(grid, (x + 1, y, Dir::Right), visited);
      propagate(grid, (x - 1, y, Dir::Left), visited);
      return;
    }
    (x, y, d) = match (value, d) {
      (b'\\', Dir::Right) | (b'/', Dir::Left) => (x, y + 1, Dir::Down),
      (b'\\', Dir::Left) | (b'/', Dir::Right) => (x, y - 1, Dir::Up),
      (b'\\', Dir::Up) | (b'/', Dir::Down) => (x - 1, y, Dir::Left),
      (b'\\', Dir::Down) | (b'/', Dir::Up) => (x + 1, y, Dir::Right),
      (_, Dir::Right) => (x + 1, y, Dir::Right),
      (_, Dir::Left) => (x - 1, y, Dir::Left),
      (_, Dir::Up) => (x, y - 1, Dir::Up),
      (_, Dir::Down) => (x, y + 1, Dir::Down),
    };
  }
}

fn energized(visited: &HashSet<(i64, i64, Dir)>) -> usize {
  visited
    .iter()
    .map(|(x, y, _)| (x, y))
    .collect::<HashSet<_>>()
    .len()
}

pub fn parse(data: &str) -> CharGrid {
  CharGrid::new(data)
}

pub fn part1(data: &CharGrid) -> usize {
  let mut visited = HashSet::new();
  propagate(data, (0, 0, Dir::Right), &mut visited);
  energized(&visited)
}

pub fn part2(data: &CharGrid) -> usize {
  let max_x = (0..data.width)
    .into_par_iter()
    .map(|x| {
      let mut visited = HashSet::new();
      propagate(data, (x as i64, 0, Dir::Down), &mut visited);
      let value = energized(&visited);

      visited.clear();
      propagate(
        data,
        (x as i64, (data.height - 1) as i64, Dir::Up),
        &mut visited,
      );
      value.max(energized(&visited))
    })
    .max()
    .unwrap();

  let max_y = (0..data.height)
    .into_par_iter()
    .map(|y| {
      let mut visited = HashSet::new();
      propagate(data, (0, y as i64, Dir::Right), &mut visited);
      let value = energized(&visited);

      visited.clear();
      propagate(
        data,
        ((data.width - 1) as i64, y as i64, Dir::Left),
        &mut visited,
      );
      value.max(energized(&visited))
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
