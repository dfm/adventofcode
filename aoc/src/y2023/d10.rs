use std::collections::HashSet;

#[derive(Clone, Copy, Debug)]
enum Dir {
  Up,
  Down,
  Left,
  Right,
}

#[derive(Default)]
pub struct Grid {
  width: usize,
  height: usize,
  data: Vec<char>,
}

impl Grid {
  fn to_coord(&self, idx: usize) -> (usize, usize) {
    let y = idx / self.width;
    (idx - y * self.width, y)
  }

  fn to_index(&self, (x, y): (usize, usize)) -> usize {
    y * self.width + x
  }

  fn in_bounds(&self, (x, y): (i64, i64)) -> bool {
    x >= 0 && x < self.width as i64 && y >= 0 && y < self.height as i64
  }

  fn step(&self, ((x, y), d): ((i64, i64), Dir)) -> Option<((i64, i64), Dir)> {
    if !self.in_bounds((x, y)) {
      return None;
    }

    let value = self.data[self.to_index((x as usize, y as usize))];
    match value {
      '.' => None,
      'S' => None,
      '|' => match d {
        Dir::Down => Some(((x, y + 1), Dir::Down)),
        Dir::Up => Some(((x, y - 1), Dir::Up)),
        _ => None,
      },
      '-' => match d {
        Dir::Left => Some(((x - 1, y), Dir::Left)),
        Dir::Right => Some(((x + 1, y), Dir::Right)),
        _ => None,
      },
      'L' => match d {
        Dir::Down => Some(((x + 1, y), Dir::Right)),
        Dir::Left => Some(((x, y - 1), Dir::Up)),
        _ => None,
      },
      'J' => match d {
        Dir::Down => Some(((x - 1, y), Dir::Left)),
        Dir::Right => Some(((x, y - 1), Dir::Up)),
        _ => None,
      },
      '7' => match d {
        Dir::Up => Some(((x - 1, y), Dir::Left)),
        Dir::Right => Some(((x, y + 1), Dir::Down)),
        _ => None,
      },
      'F' => match d {
        Dir::Up => Some(((x + 1, y), Dir::Right)),
        Dir::Left => Some(((x, y + 1), Dir::Down)),
        _ => None,
      },
      _ => panic!("Invalid square: '{}'", value),
    }
  }

  fn find_loop(&self) -> Vec<(i64, i64)> {
    let (start, _) = self
      .data
      .iter()
      .enumerate()
      .find(|(_, &c)| c == 'S')
      .unwrap();
    let (x, y) = self.to_coord(start);
    for &pt in &[
      ((x as i64, y as i64 + 1), Dir::Down),
      ((x as i64, y as i64 - 1), Dir::Up),
      ((x as i64 + 1, y as i64), Dir::Right),
      ((x as i64 - 1, y as i64), Dir::Left),
    ] {
      let mut path: Vec<_> = (0..)
        .scan(pt, |state, _| {
          if let Some(s) = self.step(*state) {
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
}

pub fn parse(data: &str) -> Grid {
  let mut grid: Grid = Default::default();
  for line in data.lines() {
    grid.width = line.chars().map(|c| grid.data.push(c)).count();
    grid.height += 1;
  }
  grid
}

pub fn part1(data: &Grid) -> usize {
  data.find_loop().len() / 2
}

pub fn part2(data: &Grid) -> usize {
  let path: HashSet<_> = data.find_loop().into_iter().collect();
  (0..data.height as i64)
    .map(|y| {
      (0..data.width as i64)
        .fold(('.', 0, 0), |(prev, cross, acc), x| {
          if path.contains(&(x, y)) {
            let c = data.data[data.to_index((x as usize, y as usize))];
            if c == '|' || (c == '7' && prev == 'L') || (c == 'J' && prev == 'F') {
              (prev, cross + 1, acc)
            } else if c == '-' {
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
