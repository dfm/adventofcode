use crate::helpers::CharGrid;

pub fn parse(data: &str) -> Vec<CharGrid> {
  data.split("\n\n").map(CharGrid::new).collect()
}

pub fn part1(data: &[CharGrid]) -> usize {
  data.iter().map(|d| find_mirror(d, None).unwrap()).sum()
}

pub fn part2(data: &[CharGrid]) -> usize {
  data
    .iter()
    .map(|d| {
      let orig = find_mirror(d, None).unwrap();
      let mut grid = d.clone();
      for n in 0..grid.data.len() {
        let tmp = grid.data[n];
        if tmp == b'.' {
          grid.data[n] = b'#';
        } else {
          grid.data[n] = b'.';
        }
        if let Some(r) = find_mirror(&grid, Some(orig)) {
          return r;
        }
        grid.data[n] = tmp;
      }
      panic!("failed");
    })
    .sum()
}

fn check_mirror(grid: &CharGrid, x: usize, y: usize, sx: usize, sy: usize) -> bool {
  let mut dx = sx;
  let mut dy = sy;
  while x + dx < grid.width && dx <= x + sx && y + dy < grid.height && dy <= y + sy {
    if grid.get(x + dx, y + dy) != grid.get(x + sx - dx, y + sy - dy) {
      return false;
    }
    dx += sx;
    dy += sy;
  }
  true
}

fn find_mirror(grid: &CharGrid, skip: Option<usize>) -> Option<usize> {
  for x in 0..grid.width - 1 {
    if let Some(s) = skip {
      if s == x + 1 {
        continue;
      }
    }
    if (0..grid.height).all(|y| check_mirror(grid, x, y, 1, 0)) {
      return Some(x + 1);
    }
  }
  for y in 0..grid.height - 1 {
    if let Some(s) = skip {
      if s == 100 * (y + 1) {
        continue;
      }
    }
    if (0..grid.width).all(|x| check_mirror(grid, x, y, 0, 1)) {
      return Some(100 * (y + 1));
    }
  }
  None
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA: &str = r"#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA);
    assert_eq!(part1(&data), 405);
  }

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA);
    assert_eq!(part2(&data), 400);
  }
}
