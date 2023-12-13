use crate::helpers::CharGrid;
use itertools::Itertools;
use std::collections::{HashMap, HashSet};

fn filter_map_adjacent<'a, T, F>(
  grid: &'a CharGrid,
  f: F,
  idx: usize,
) -> impl Iterator<Item = T> + 'a
where
  F: Fn((i64, i64), u8) -> Option<T> + 'a,
{
  let (x0, y0) = grid.to_coords(idx);
  (-1..=1)
    .cartesian_product(-1..=1)
    .filter_map(move |(dy, dx)| {
      let x = x0 as i64 + dx;
      let y = y0 as i64 + dy;
      if 0 <= x && x < grid.width as i64 && 0 <= y && y < grid.height as i64 {
        Some((x, y))
      } else {
        None
      }
    })
    .filter_map(move |(x, y)| {
      let c = grid.get(x as usize, y as usize);
      f((x, y), c)
    })
}

fn build_graph<F>(grid: &CharGrid, f: F) -> Vec<(i64, HashSet<(i64, i64)>)>
where
  F: Fn(u8) -> bool,
{
  let mut graph = Vec::new();
  let mut current: Option<Vec<i64>> = None;
  let mut symbols = HashSet::new();
  for (n, &c) in grid.data.iter().enumerate() {
    if let Some(v) = (c as char).to_digit(10) {
      if let Some(x) = &mut current {
        x.push(v.into());
      } else {
        current = Some(vec![v.into()]);
      }
      symbols.extend(filter_map_adjacent(
        grid,
        |(x, y), c| if f(c) { Some((x, y)) } else { None },
        n,
      ));
    } else {
      if let Some(x) = current {
        let value = x
          .into_iter()
          .rev()
          .enumerate()
          .map(|(d, v)| 10i64.pow(d as u32) * v)
          .sum::<i64>();
        graph.push((value, symbols.clone()));
      }
      current = None;
      symbols.clear();
    }
  }
  graph
}

pub fn parse(data: &str) -> CharGrid {
  CharGrid::new(data)
}

pub fn part1(grid: &CharGrid) -> i64 {
  let graph = build_graph(grid, |c| c != b'.' && !c.is_ascii_digit());
  graph
    .iter()
    .map(|(v, s)| if s.is_empty() { 0 } else { *v })
    .sum()
}

pub fn part2(grid: &CharGrid) -> i64 {
  let graph = build_graph(grid, |c| c == b'*');
  let mut inv: HashMap<_, Vec<i64>> = HashMap::new();
  for (v, s) in graph.iter() {
    for &c in s {
      inv.entry(c).and_modify(|e| e.push(*v)).or_insert(vec![*v]);
    }
  }
  let mut acc = 0;
  for (_, n) in inv {
    if n.len() == 2 {
      let r: i64 = n.iter().product();
      acc += r;
    }
  }
  acc
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA: &str = r"467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA);
    assert_eq!(part1(&data), 4361);
  }

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA);
    assert_eq!(part2(&data), 467835);
  }
}
