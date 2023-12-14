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

  for n in 0..10_000 {
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
  panic!("Failure");
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
      let mut x = x as i64;
      let mut y = y as i64;
      let mut acc = 0;
      loop {
        let xp = x + dirx;
        let yp = y + diry;
        if xp < 0
          || xp >= grid.width as i64
          || yp < 0
          || yp >= grid.height as i64
          || grid.get(xp as usize, yp as usize) == b'#'
        {
          break;
        }
        if spheres.contains(&(xp as usize, yp as usize)) {
          acc += 1;
        }
        x = xp;
        y = yp;
      }
      ((x - acc * dirx) as usize, (y - acc * diry) as usize)
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
