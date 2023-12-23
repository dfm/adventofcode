use crate::helpers::CharGrid;
use std::collections::{HashMap, HashSet, VecDeque};

pub fn parse(data: &str) -> CharGrid {
  CharGrid::new(data)
}

pub fn part1(data: &CharGrid) -> usize {
  solve(data, |d, c| {
    matches!(
      (d, c),
      (_, Some(b'.'))
        | ((-1, 0), Some(b'<'))
        | ((1, 0), Some(b'>'))
        | ((0, -1), Some(b'^'))
        | ((0, 1), Some(b'v'))
    )
  })
}

pub fn part2(data: &CharGrid) -> usize {
  solve(data, |_, c| !matches!(c, None | Some(b'#')))
}

fn longest_distance(
  start: Coord,
  end: Coord,
  edges: &HashMap<Edge, usize>,
  cache: &HashSet<Coord>,
) -> Vec<usize> {
  if start == end {
    return vec![0];
  }
  let mut cache = cache.clone();
  cache.insert(start);
  edges
    .iter()
    .filter_map(|((a, b), dist)| {
      if *a != start || cache.contains(b) {
        None
      } else {
        Some(
          longest_distance(*b, end, edges, &cache)
            .iter()
            .map( |d| d + dist).collect::<Vec<_>>(),
        )
      }
    })
    .flatten()
    .collect()
}

fn solve<F>(data: &CharGrid, is_allowed: F) -> usize
where
  F: Fn(Coord, Option<u8>) -> bool,
{
  let edges = build_graph(data, is_allowed);

  longest_distance(
    (1, 0),
    (data.width as i64 - 2, data.height as i64 - 1),
    &edges,
    &HashSet::new(),
  )
  .into_iter()
  .max()
  .unwrap()
}

type Coord = (i64, i64);
type Edge = (Coord, Coord);

fn build_graph<F>(data: &CharGrid, is_allowed: F) -> HashMap<Edge, usize>
where
  F: Fn(Coord, Option<u8>) -> bool,
{
  let mut edges = HashMap::new();
  let mut visited = HashSet::new();
  let mut queue = VecDeque::new();
  queue.push_back(((1, 0), (0, 1)));
  visited.insert(((1, 0), (0, 1)));

  while let Some(next) = queue.pop_front() {
    let ((x0, y0), (dx, dy)) = next;
    let mut x = x0 + dx;
    let mut y = y0 + dy;
    let mut dist = 0;
    let mut past = HashSet::new();
    past.insert((x0, y0));
    past.insert((x, y));
    loop {
      let mut options = Vec::new();
      for (dx, dy) in [(0, -1), (-1, 0), (1, 0), (0, 1)] {
        let xp = x + dx;
        let yp = y + dy;
        if past.contains(&(xp, yp)) || !is_allowed((dx, dy), data.get_checked(xp, yp)) {
          continue;
        }
        options.push(((xp, yp), (dx, dy)));
      }
      dist += 1;
      match options.len() {
        0 if (x, y) != (data.width as i64 - 2, data.height as i64 - 1) => {
          break;
        }
        1 => {
          let ((xp, yp), _) = options[0];
          x = xp;
          y = yp;
          past.insert((xp, yp));
        }
        _ => {
          edges
            .entry(((x0, y0), (x, y)))
            .and_modify(|e| *e = dist.max(*e))
            .or_insert(dist);
          // edges.insert(((x0, y0), (dx, dy), (x, y)), dist);
          for (_, d) in options {
            if !visited.contains(&((x, y), d)) {
              visited.insert(((x, y), d));
              queue.push_back(((x, y), d));
            }
          }
          break;
        }
      }
    }
  }
  edges
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA: &str = r"#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#
";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA);
    assert_eq!(part1(&data), 94);
  }

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA);
    assert_eq!(part2(&data), 154);
  }
}
