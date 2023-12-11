use std::collections::HashSet;

pub fn parse(data: &str) -> Vec<(i64, i64)> {
  let mut s = Vec::new();
  for (y, line) in data.lines().enumerate() {
    for (x, c) in line.chars().enumerate() {
      if c == '#' {
        s.push((x as i64, y as i64));
      }
    }
  }
  s
}

fn expand(data: &mut [(i64, i64)], factor: i64) {
  let x: HashSet<i64> = data.iter().map(|&e| e.0).collect();
  let y: HashSet<i64> = data.iter().map(|&e| e.1).collect();
  let max_x = *x.iter().max().unwrap();
  let max_y = *y.iter().max().unwrap();

  let cols = (0..=max_x).collect::<HashSet<i64>>();
  let cols: Vec<_> = cols.difference(&x).copied().collect();
  let rows = (0..=max_y).collect::<HashSet<i64>>();
  let rows: Vec<_> = rows.difference(&y).copied().collect();

  data.iter_mut().for_each(|(x, y)| {
    *x += (factor - 1) * cols.iter().filter(|&&d| d < *x).count() as i64;
    *y += (factor - 1) * rows.iter().filter(|&&d| d < *y).count() as i64;
  });
}

fn solve(data: &[(i64, i64)], factor: i64) -> i64 {
  let mut data = data.to_vec();
  expand(&mut data, factor);

  (0..data.len() - 1)
    .map(|n| {
      (n + 1..data.len())
        .map(|m| (data[n].0 - data[m].0).abs() + (data[n].1 - data[m].1).abs())
        .sum::<i64>()
    })
    .sum()
}

pub fn part1(data: &[(i64, i64)]) -> i64 {
  solve(data, 2)
}

pub fn part2(data: &[(i64, i64)]) -> i64 {
  solve(data, 1_000_000)
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA: &str = r"...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA);
    assert_eq!(part1(&data), 374);
  }

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA);
    assert_eq!(part2(&data), 82000210);
  }
}
