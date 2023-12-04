use anyhow::Result;
use std::collections::HashSet;

fn numbers(s: &str) -> HashSet<usize> {
  s.split_whitespace()
    .filter(|s| !s.is_empty())
    .map(|s| s.parse::<usize>().unwrap())
    .collect()
}

pub fn parse(data: &str) -> Result<Vec<usize>> {
  Ok(
    data
      .lines()
      .map(|line| {
        let (_, line) = line.split_once(':').unwrap();
        let (a, b) = line.split_once('|').unwrap();
        let a = numbers(a);
        let b = numbers(b);
        a.intersection(&b).count()
      })
      .collect(),
  )
}

pub fn part1(data: &[usize]) -> usize {
  data
    .iter()
    .map(|&n| {
      if n == 0 {
        0
      } else {
        1 << (n - 1)
      }
    })
    .sum()
}

pub fn part2(data: &[usize]) -> usize {
  let mut copies = vec![1usize; data.len()];
  for (n, &m) in data.iter().enumerate() {
    let factor = copies[n];
    for i in 1..=m {
      copies[n + i] += factor;
    }
  }
  copies.iter().sum()
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA: &str = r"Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA).unwrap();
    assert_eq!(part1(&data), 13);
  }

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA).unwrap();
    assert_eq!(part2(&data), 30);
  }
}
