use crate::helpers::{CharGrid, ShortestPath, shortest_distance};

struct Graph {
  min: usize,
  max: usize,
  grid: CharGrid<usize>,
}

impl ShortestPath for Graph {
  type Coord = (i64, i64, i64, i64, usize);

  fn is_target(&self, current: &Self::Coord) -> bool {
    current.0 == self.grid.width as i64 - 1 && current.1 == self.grid.height as i64 - 1
  }

  fn neighbors(&self, current: &Self::Coord) -> Vec<(Self::Coord, usize)> {
    let (x, y, dx, dy, acc) = *current;
    let mut neighbors = Vec::new();
    if acc < self.max {
      neighbors.push((x + dx, y + dy, dx, dy, acc + 1));
    }
    if acc >= self.min {
      neighbors.push((x - dy, y + dx, -dy, dx, 1));
      neighbors.push((x + dy, y - dx, dy, -dx, 1));
    }
    neighbors
      .into_iter()
      .filter(|&(x, y, _, _, _)| self.grid.in_bounds(x, y))
      .map(|c| (c, self.grid.get(c.0 as usize, c.1 as usize)))
      .collect()
  }
}

pub fn parse(data: &str) -> CharGrid<usize> {
  CharGrid::new_with(|c| (c - b'0') as usize, data)
}

pub fn part1(data: &CharGrid<usize>) -> usize {
  let graph = Graph {min: 0, max: 3, grid: data.clone()};
  let starts = vec![(0, 0, 1, 0, 1), (0, 0, 0, 1, 1)];
  shortest_distance(graph, &starts).unwrap()
}

pub fn part2(data: &CharGrid<usize>) -> usize {
  let graph = Graph {min: 4, max: 10, grid: data.clone()};
  let starts = vec![(0, 0, 1, 0, 1), (0, 0, 0, 1, 1)];
  shortest_distance(graph, &starts).unwrap()
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA: &str = r"2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA);
    assert_eq!(part1(&data), 102);
  }

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA);
    assert_eq!(part2(&data), 94);
  }
}
