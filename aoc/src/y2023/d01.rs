use anyhow::Result;

pub fn parse(data: &str) -> Result<String> {
  Ok(data.to_string())
}

pub fn part1(data: &str) -> i64 {
  data
    .lines()
    .map(|line| {
      let d: Vec<_> = line
        .chars()
        .filter(|c| '1' <= *c && *c <= '9')
        .map(|c| c as i64 - '0' as i64)
        .collect();
      10 * d[0] + d[d.len() - 1]
    })
    .sum()
}

pub fn part2(data: &str) -> i64 {
  data
    .lines()
    .map(|line| {
      let len = line.len();
      let chars = line.as_bytes();
      let d: Vec<_> = (0..len)
        .filter_map(|i| {
          let c = chars[i];
          if (b'1'..=b'9').contains(&c) {
            return Some(c as i64 - b'0' as i64);
          }
          for &(key, val) in NUM_MAP {
            let end = i + key.len();
            if end <= line.len() && line[i..end] == *key {
              return Some(val);
            }
          }
          None
        })
        .collect();
      10 * d[0] + d[d.len() - 1]
    })
    .sum()
}

const NUM_MAP: &[(&str, i64)] = &[
  ("one", 1),
  ("two", 2),
  ("three", 3),
  ("four", 4),
  ("five", 5),
  ("six", 6),
  ("seven", 7),
  ("eight", 8),
  ("nine", 9),
];

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA1: &str = r"1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA1).unwrap();
    assert_eq!(part1(&data), 142);
  }

  const TEST_DATA2: &str = r"two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
";

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA2).unwrap();
    assert_eq!(part2(&data), 281);
  }
}
