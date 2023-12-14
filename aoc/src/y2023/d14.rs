use std::collections::HashSet;

use crate::helpers::CharGrid;

pub fn parse(data: &str) -> CharGrid {
  CharGrid::new(data)
}

pub fn part1(data: &CharGrid) -> usize {
  let spheres = find_spheres(data);
  let spheres = tilt(data, &spheres, 0, -1);
  load(data.height, &spheres)
}

pub fn part2(data: &CharGrid) -> usize {
  let mut spheres = find_spheres(data);
  let mut acc = Vec::new();
  let mut current;

  for n in 0.. {
    (spheres, current) = cycle(data, &spheres);
    acc.push(current);

    if n > 10 {
      for period in 5..acc.len() / 2 {
        if (0..period).all(|i| acc[acc.len() - i - 1] == acc[acc.len() - period - i - 1]) {
          return acc[acc.len() - period - 1 + (1_000_000_000 - acc.len() + period) % (period)];
        }
      }
    }
  }
  unreachable!("No solution");
}

fn find_spheres(grid: &CharGrid) -> HashSet<(usize, usize)> {
  grid
    .data
    .iter()
    .enumerate()
    .filter_map(|(n, &c)| {
      if c == b'O' {
        Some(grid.to_coords(n))
      } else {
        None
      }
    })
    .collect()
}

fn cycle(grid: &CharGrid, spheres: &HashSet<(usize, usize)>) -> (HashSet<(usize, usize)>, usize) {
  let spheres = tilt(grid, spheres, 0, -1);
  let spheres = tilt(grid, &spheres, -1, 0);
  let spheres = tilt(grid, &spheres, 0, 1);
  let spheres = tilt(grid, &spheres, 1, 0);
  let l = load(grid.height, &spheres);
  (spheres, l)
}

fn tilt(
  grid: &CharGrid,
  spheres: &HashSet<(usize, usize)>,
  dirx: i64,
  diry: i64,
) -> HashSet<(usize, usize)> {
  spheres
    .iter()
    .map(|&(x, y)| {
      let mut x = x as i64 + dirx;
      let mut y = y as i64 + diry;
      let mut acc = 0;
      while x >= 0
        && x < grid.width as i64
        && y >= 0
        && y < grid.height as i64
        && grid.get(x as usize, y as usize) != b'#'
      {
        if spheres.contains(&(x as usize, y as usize)) {
          acc += 1;
        }
        x += dirx;
        y += diry;
      }
      (
        (x - (acc + 1) * dirx) as usize,
        (y - (acc + 1) * diry) as usize,
      )
    })
    .collect()
}

fn load(height: usize, spheres: &HashSet<(usize, usize)>) -> usize {
  spheres.iter().map(|(_, y)| height - y).sum()
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA: &str = r"O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA);
    println!("{}", data.height);
    assert_eq!(part1(&data), 136);
  }

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA);
    assert_eq!(part2(&data), 64);
  }
}
