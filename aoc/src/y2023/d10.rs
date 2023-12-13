use crate::helpers::CharGrid;
use std::collections::HashSet;

#[derive(Clone, Copy, Debug)]
enum Dir {
  Up,
  Down,
  Left,
  Right,
}

fn in_bounds(grid: &CharGrid, (x, y): (i64, i64)) -> bool {
  x >= 0 && x < grid.width as i64 && y >= 0 && y < grid.height as i64
}

fn step(grid: &CharGrid, ((x, y), d): ((i64, i64), Dir)) -> Option<((i64, i64), Dir)> {
  if !in_bounds(grid, (x, y)) {
    return None;
  }

  let value = grid.get(x as usize, y as usize);
  match (value, d) {
    (b'|', Dir::Down) => Some(((x, y + 1), Dir::Down)),
    (b'|', Dir::Up) => Some(((x, y - 1), Dir::Up)),
    (b'-', Dir::Left) => Some(((x - 1, y), Dir::Left)),
    (b'-', Dir::Right) => Some(((x + 1, y), Dir::Right)),
    (b'L', Dir::Down) => Some(((x + 1, y), Dir::Right)),
    (b'L', Dir::Left) => Some(((x, y - 1), Dir::Up)),
    (b'J', Dir::Down) => Some(((x - 1, y), Dir::Left)),
    (b'J', Dir::Right) => Some(((x, y - 1), Dir::Up)),
    (b'7', Dir::Up) => Some(((x - 1, y), Dir::Left)),
    (b'7', Dir::Right) => Some(((x, y + 1), Dir::Down)),
    (b'F', Dir::Up) => Some(((x + 1, y), Dir::Right)),
    (b'F', Dir::Left) => Some(((x, y + 1), Dir::Down)),
    _ => None,
  }
}

fn find_loop(grid: &CharGrid) -> Vec<(i64, i64)> {
  let (start, _) = grid
    .data
    .iter()
    .enumerate()
    .find(|(_, &c)| c == b'S')
    .unwrap();
  let (x, y) = grid.to_coords(start);
  for &pt in &[
    ((x as i64, y as i64 + 1), Dir::Down),
    ((x as i64, y as i64 - 1), Dir::Up),
    ((x as i64 + 1, y as i64), Dir::Right),
    ((x as i64 - 1, y as i64), Dir::Left),
  ] {
    let mut path: Vec<_> = (0..)
      .scan(pt, |state, _| {
        if let Some(s) = step(grid, *state) {
          *state = s;
          Some(s.0)
        } else {
          None
        }
      })
      .collect();
    if !path.is_empty() && path[path.len() - 1] == (x as i64, y as i64) {
      let mut result = vec![pt.0];
      result.append(&mut path);
      return result;
    }
  }
  unreachable!("Couldn't find loop");
}

pub fn parse(data: &str) -> CharGrid {
  CharGrid::new(data)
}

pub fn part1(data: &CharGrid) -> usize {
  find_loop(data).len() / 2
}

pub fn part2(data: &CharGrid) -> usize {
  let path: HashSet<_> = find_loop(data).into_iter().collect();
  (0..data.height as i64)
    .map(|y| {
      (0..data.width as i64)
        .fold((b'.', 0, 0), |(prev, cross, acc), x| {
          if path.contains(&(x, y)) {
            let c = data.get(x as usize, y as usize);
            if c == b'|' || (c == b'7' && prev == b'L') || (c == b'J' && prev == b'F') {
              (prev, cross + 1, acc)
            } else if c == b'-' {
              (prev, cross, acc)
            } else {
              (c, cross, acc)
            }
          } else if cross % 2 == 1 {
            (prev, cross, acc + 1)
          } else {
            (prev, cross, acc)
          }
        })
        .2
    })
    .sum()
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA1: &str = r"..F7.
.FJ|.
SJ.L7
|F--J
LJ...
";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA1);
    assert_eq!(part1(&data), 8);
  }

  const TEST_DATA2: &str = r"FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
";

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA2);
    assert_eq!(part2(&data), 10);
  }
}
