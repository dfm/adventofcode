pub fn parse(data: &str) -> String {
  data.to_string()
}

pub fn part1(data: &str) -> i64 {
  0
}

pub fn part2(data: &str) -> i64 {
  0
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA: &str = r"0";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA).unwrap();
    assert_eq!(part1(&data), 0);
  }

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA).unwrap();
    assert_eq!(part2(&data), 0);
  }
}
