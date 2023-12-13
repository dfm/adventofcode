#[derive(Default, Debug, Clone)]
pub struct Grid {
  width: usize,
  height: usize,
  data: Vec<u8>,
}

impl Grid {
  fn new(data: &str) -> Self {
    let mut grid: Grid = Default::default();
    for line in data.lines() {
      grid.width = line.as_bytes().iter().map(|&c| grid.data.push(c)).count();
      grid.height += 1;
    }
    grid
  }

  fn to_index(&self, (x, y): (usize, usize)) -> usize {
    y * self.width + x
  }

  fn get(&self, x: usize, y: usize) -> u8 {
    self.data[self.to_index((x, y))]
  }

  fn check_mirror(&self, x: usize, y: usize, sx: usize, sy: usize) -> bool {
    let mut dx = sx;
    let mut dy = sy;
    while x + dx < self.width && dx <= x + sx && y + dy < self.height && dy <= y + sy {
      if self.get(x + dx, y + dy) != self.get(x + sx - dx, y + sy - dy) {
        return false;
      }
      dx += sx;
      dy += sy;
    }
    true
  }

  fn find_mirror(&self, skip: Option<usize>) -> Option<usize> {
    for x in 0..self.width - 1 {
      if let Some(s) = skip {
        if s == x + 1 {
          continue;
        }
      }
      if (0..self.height).all(|y| self.check_mirror(x, y, 1, 0)) {
        return Some(x + 1);
      }
    }
    for y in 0..self.height - 1 {
      if let Some(s) = skip {
        if s == 100 * (y + 1) {
          continue;
        }
      }
      if (0..self.width).all(|x| self.check_mirror(x, y, 0, 1)) {
        return Some(100 * (y + 1));
      }
    }
    None
  }
}

pub fn parse(data: &str) -> Vec<Grid> {
  data.split("\n\n").map(Grid::new).collect()
}

pub fn part1(data: &[Grid]) -> usize {
  data.iter().map(|d| d.find_mirror(None).unwrap()).sum()
}

pub fn part2(data: &[Grid]) -> usize {
  data
    .iter()
    .map(|d| {
      let orig = d.find_mirror(None).unwrap();
      let mut grid = d.clone();
      for n in 0..grid.data.len() {
        let tmp = grid.data[n];
        if tmp == b'.' {
          grid.data[n] = b'#';
        } else {
          grid.data[n] = b'.';
        }
        if let Some(r) = grid.find_mirror(Some(orig)) {
          return r;
        }
        grid.data[n] = tmp;
      }
      panic!("failed");
    })
    .sum()
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
    println!("{:?}", data);
    assert_eq!(part1(&data), 405);
  }

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA);
    assert_eq!(part2(&data), 400);
  }
}
