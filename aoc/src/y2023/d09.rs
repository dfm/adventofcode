pub fn parse(data: &str) -> (i64, i64) {
  data
    .lines()
    .map(|line| {
      let mut n: Vec<i64> = line
        .split_whitespace()
        .map(|n| n.parse().unwrap())
        .collect();
      let mut v = Vec::new();

      while n.iter().any(|&x| x != 0) {
        v.push((n[0], n[n.len() - 1]));
        n = n.windows(2).map(|d| d[1] - d[0]).collect();
      }

      v.iter()
        .rev()
        .fold((0, 0), |(d0, d1), &(x0, x1)| (x0 - d0, x1 + d1))
    })
    .fold((0, 0), |acc, d| (acc.0 + d.0, acc.1 + d.1))
}

pub fn part1(data: &(i64, i64)) -> i64 {
  data.1
}

pub fn part2(data: &(i64, i64)) -> i64 {
  data.0
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA: &str = r"0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA);
    assert_eq!(part1(&data), 114);
  }

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA);
    assert_eq!(part2(&data), 2);
  }
}
