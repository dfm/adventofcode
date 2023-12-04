use anyhow::Result;

pub fn parse(data: &str) -> Result<String> {
  Ok(data.to_string())
}

pub fn part1(data: &str) -> i64 {
  solve(data, |_| None, |_| None)
}

pub fn part2(data: &str) -> i64 {
  solve(
    data,
    |s| find_text(s, str::starts_with),
    |s| find_text(s, str::ends_with),
  )
}

fn solve<F, B>(data: &str, fwd: F, bwd: B) -> i64
where
  F: Copy + Fn(&str) -> Option<i64>,
  B: Copy + Fn(&str) -> Option<i64>,
{
  data
    .lines()
    .map(|line| {
      let len = line.len();
      let a = line
        .chars()
        .enumerate()
        .find_map(|(n, c)| char_to_digit(c).or_else(|| fwd(&line[n..len])))
        .unwrap();
      let b = line
        .chars()
        .rev()
        .enumerate()
        .find_map(|(n, c)| char_to_digit(c).or_else(|| bwd(&line[0..len - n])))
        .unwrap();
      10 * a + b
    })
    .sum()
}

fn char_to_digit(c: char) -> Option<i64> {
  if c.is_ascii_digit() {
    Some((c as i64) - ('0' as i64))
  } else {
    None
  }
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

fn find_text<'a, F>(s: &'a str, f: F) -> Option<i64>
where
  F: Fn(&'a str, &'a str) -> bool,
{
  for &(k, v) in NUM_MAP {
    if f(s, k) {
      return Some(v);
    }
  }
  None
}

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
