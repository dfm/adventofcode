use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap};

pub fn gcd(a: usize, b: usize) -> usize {
  let mut a = a;
  let mut b = b;
  while b > 0 {
    let t = b;
    b = a % b;
    a = t;
  }
  a
}

pub fn lcm(a: usize, b: usize) -> usize {
  a * b / gcd(a, b)
}

// An indexed grid of coordinates
#[derive(Default, Clone)]
pub struct CharGrid<T = u8> {
  pub(crate) width: usize,
  pub(crate) height: usize,
  pub(crate) data: Vec<T>,
}

impl<T: std::default::Default> CharGrid<T> {
  pub fn new_with<F: Fn(u8) -> T>(f: F, data: &str) -> Self {
    let mut grid: Self = Default::default();
    for line in data.lines() {
      grid.width = line
        .as_bytes()
        .iter()
        .map(|&c| grid.data.push(f(c)))
        .count();
      grid.height += 1;
    }
    grid
  }
}

impl CharGrid<u8> {
  pub fn new(data: &str) -> Self {
    Self::new_with(|c| c, data)
  }
}

impl<T: Copy> CharGrid<T> {
  pub fn to_coords(&self, idx: usize) -> (usize, usize) {
    let y = idx / self.width;
    (idx - y * self.width, y)
  }

  pub fn to_index(&self, x: usize, y: usize) -> usize {
    y * self.width + x
  }

  pub fn in_bounds(&self, x: i64, y: i64) -> bool {
    0 <= x && x < self.width as i64 && 0 <= y && y < self.height as i64
  }

  pub fn get(&self, x: usize, y: usize) -> T {
    self.data[self.to_index(x, y)]
  }

  pub fn get_mut(&mut self, x: usize, y: usize) -> &mut T {
    let idx = self.to_index(x, y);
    &mut self.data[idx]
  }

  pub fn get_checked(&self, x: i64, y: i64) -> Option<T> {
    if self.in_bounds(x, y) {
      Some(self.data[self.to_index(x as usize, y as usize)])
    } else {
      None
    }
  }

  pub fn get_mut_checked(&mut self, x: i64, y: i64) -> Option<&mut T> {
    if self.in_bounds(x, y) {
      let idx = self.to_index(x as usize, y as usize);
      Some(&mut self.data[idx])
    } else {
      None
    }
  }
}

impl std::fmt::Debug for CharGrid<u8> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    for y in 0..self.height {
      for x in 0..self.width {
        write!(f, "{}", self.get(x, y) as char)?;
      }
      writeln!(f)?;
    }
    Ok(())
  }
}

impl std::fmt::Debug for CharGrid<bool> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    for y in 0..self.height {
      for x in 0..self.width {
        if self.get(x, y) {
          write!(f, "*")?;
        } else {
          write!(f, ".")?;
        }
      }
      writeln!(f)?;
    }
    Ok(())
  }
}

// Shortest path
pub trait ShortestPath {
  type Coord;
  fn is_target(&self, _current: &Self::Coord) -> bool {
    false
  }
  fn neighbors(&self, current: &Self::Coord) -> Vec<(Self::Coord, usize)>;
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct ShortestPathState<Coord> {
  cost: usize,
  coord: Coord,
}

impl<Coord: std::cmp::Ord> Ord for ShortestPathState<Coord> {
  fn cmp(&self, other: &Self) -> Ordering {
    other
      .cost
      .cmp(&self.cost)
      .then_with(|| self.coord.cmp(&other.coord))
  }
}

impl<Coord: std::cmp::Ord> PartialOrd for ShortestPathState<Coord> {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

pub fn shortest_distance<G, C>(graph: G, starts: &[C]) -> Option<usize>
where
  G: ShortestPath<Coord = C>,
  C: Copy + std::cmp::Ord + std::hash::Hash,
{
  let mut distances = HashMap::new();
  let mut heap = BinaryHeap::new();

  for &start in starts {
    distances.insert(start, 0);
    heap.push(ShortestPathState {
      cost: 0,
      coord: start,
    });
  }

  while let Some(ShortestPathState { cost, coord }) = heap.pop() {
    if graph.is_target(&coord) {
      return Some(cost);
    }

    if cost > *distances.get(&coord).unwrap_or(&usize::MAX) {
      continue;
    }

    for (neighbor, delta) in graph.neighbors(&coord) {
      let next = ShortestPathState {
        cost: cost + delta,
        coord: neighbor,
      };
      if next.cost < *distances.get(&next.coord).unwrap_or(&usize::MAX) {
        heap.push(next);
        distances.insert(next.coord, next.cost);
      }
    }
  }

  None
}

pub fn shortest_paths<G, C>(graph: G, starts: &[C], max_cost: usize) -> HashMap<C, usize>
where
  G: ShortestPath<Coord = C>,
  C: Copy + std::cmp::Ord + std::hash::Hash,
{
  let mut distances = HashMap::new();
  let mut heap = BinaryHeap::new();

  for &start in starts {
    distances.insert(start, 0);
    heap.push(ShortestPathState {
      cost: 0,
      coord: start,
    });
  }

  while let Some(ShortestPathState { cost, coord }) = heap.pop() {
    if cost >= max_cost {
      continue;
    }

    if cost > *distances.get(&coord).unwrap_or(&usize::MAX) {
      continue;
    }

    for (neighbor, delta) in graph.neighbors(&coord) {
      let next = ShortestPathState {
        cost: cost + delta,
        coord: neighbor,
      };
      if next.cost < *distances.get(&next.coord).unwrap_or(&usize::MAX) {
        heap.push(next);
        distances.insert(next.coord, next.cost);
      }
    }
  }

  distances
}
