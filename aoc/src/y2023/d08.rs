use std::collections::{HashMap, HashSet};

pub struct Data {
  start: Option<usize>,
  end: Option<usize>,
  starts: Vec<usize>,
  ends: HashSet<usize>,
  instr: String,
  steps: Vec<(usize, usize)>,
}

impl Data {
  fn update(&self, s: char, n: usize) -> usize {
    match s {
      'L' => self.steps[n].0,
      'R' => self.steps[n].1,
      _ => unreachable!("{}", s),
    }
  }
}

pub fn parse(data: &str) -> Data {
  let instr = data.lines().next().unwrap().to_string();

  let idx: HashMap<&str, usize> = data
    .lines()
    .skip(2)
    .enumerate()
    .map(|(n, s)| (&s[0..3], n))
    .collect();

  let steps = data
    .lines()
    .skip(2)
    .map(|line| {
      (
        *idx.get(&line[7..10]).unwrap(),
        *idx.get(&line[12..15]).unwrap(),
      )
    })
    .collect();

  let starts = idx
    .iter()
    .filter_map(|(&s, &n)| if s.ends_with('A') { Some(n) } else { None })
    .collect();
  let ends = idx
    .iter()
    .filter_map(|(&s, &n)| if s.ends_with('Z') { Some(n) } else { None })
    .collect();

  Data {
    start: idx.get("AAA").copied(),
    end: idx.get("ZZZ").copied(),
    starts,
    ends,
    instr,
    steps,
  }
}

pub fn part1(data: &Data) -> usize {
  let end = data.end.unwrap();
  let mut n = data.start.unwrap();
  for (i, s) in data.instr.chars().cycle().enumerate() {
    n = data.update(s, n);
    if n == end {
      return i + 1;
    }
  }
  0
}

fn gcd(a: usize, b: usize) -> usize {
  let mut a = a;
  let mut b = b;
  while b > 0 {
    let t = b;
    b = a % b;
    a = t;
  }
  a
}

fn lcm(a: usize, b: usize) -> usize {
  a * b / gcd(a, b)
}

pub fn part2(data: &Data) -> usize {
  let stats: Vec<usize> = data
    .starts
    .iter()
    .map(|&start| {
      let mut n = start;
      for (i, s) in data.instr.chars().cycle().enumerate() {
        n = data.update(s, n);
        if data.ends.contains(&n) {
          return i + 1;
        }
      }
      unreachable!()
    })
    .collect();
  stats.iter().fold(1, |acc, &n| lcm(acc, n))
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA1: &str = r"RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA1);
    assert_eq!(part1(&data), 2);
  }

  const TEST_DATA2: &str = r"LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
";

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA2);
    assert_eq!(part2(&data), 6);
  }
}
