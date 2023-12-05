use anyhow::Result;
use std::collections::HashSet;

type Map = (usize, usize, usize);
type Range = (usize, usize);

#[derive(Debug)]
pub struct Almanac {
  seeds: Vec<usize>,
  maps: Vec<Vec<Map>>,
}

pub fn parse(data: &str) -> Result<Almanac> {
  let mut parts = data.split("\n\n");

  let (_, seeds) = parts.next().unwrap().split_once(':').unwrap();
  let seeds = seeds
    .split_whitespace()
    .map(|s| s.parse().unwrap())
    .collect();

  let mut maps = Vec::new();
  for part in parts {
    let mut lines = part.lines();
    let _ = lines.next();
    maps.push(lines.map(parse_map).collect());
  }

  Ok(Almanac { seeds, maps })
}

pub fn part1(data: &Almanac) -> usize {
  data
    .seeds
    .iter()
    .map(|&seed| data.maps.iter().fold(seed, |n, m| apply_maps(n, m)))
    .min()
    .unwrap()
}

pub fn part2(data: &Almanac) -> usize {
  (0..data.seeds.len())
    .step_by(2)
    .map(|n| {
      let mut ranges = vec![(data.seeds[n], data.seeds[n] + data.seeds[n + 1] - 1)];
      for m in data.maps.iter() {
        ranges = ranges
          .into_iter()
          .flat_map(|r| apply_maps_to_range(r, m))
          .collect();
      }
      ranges.iter().map(|&(x, _)| x).min().unwrap()
    })
    .min().unwrap()
}

fn parse_map(s: &str) -> (usize, usize, usize) {
  let mut parts = s.split_whitespace();
  (
    parts.next().unwrap().parse().unwrap(),
    parts.next().unwrap().parse().unwrap(),
    parts.next().unwrap().parse().unwrap(),
  )
}

fn apply_maps(n: usize, m: &[Map]) -> usize {
  for &(t, s, w) in m {
    if s <= n && n <= s + w {
      return t + n - s;
    }
  }
  n
}

fn apply_maps_to_range((a, b): Range, m: &[Map]) -> Vec<Range> {
  let mut edges: HashSet<usize> = m.iter().map(|&(_, s, _)| s).collect();
  edges.extend(m.iter().map(|&(_, s, w)| s + w));
  let mut edges: Vec<usize> = edges.into_iter().filter(|&n| a < n && n < b).collect();
  edges.sort();

  if edges.is_empty() {
    return vec![(apply_maps(a, m), apply_maps(b, m))];
  }

  let mut result = vec![(apply_maps(a, m), apply_maps(edges[0] - 1, m))];
  for (n, &e) in edges.iter().take(edges.len() - 1).enumerate() {
    result.push((apply_maps(e, m), apply_maps(edges[n + 1] - 1, m)));
  }
  result.push((apply_maps(edges[edges.len() - 1], m), apply_maps(b, m)));
  result
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA: &str = r"seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA).unwrap();
    assert_eq!(part1(&data), 35);
  }

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA).unwrap();
    assert_eq!(part2(&data), 46);
  }
}
