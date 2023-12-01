use anyhow::Result;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::one_of;
use nom::combinator::value;

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
      let d: Vec<_> = (0..line.len()).filter_map(|i| {
        alt((digit, text_digit))(&line[i..line.len()])
          .map_or_else(|_| None, |(_, n)| Some(n))
      }).collect();
      10 * d[0] + d[d.len() - 1]
    })
    .sum()
}

fn digit(i: &str) -> crate::parsers::Result<i64> {
  let (o, i) = one_of("123456789")(i)?;
  Ok((o, i as i64 - '0' as i64))
}

fn text_digit(i: &str) -> crate::parsers::Result<i64> {
  alt((
    value(1, tag("one")),
    value(2, tag("two")),
    value(3, tag("three")),
    value(4, tag("four")),
    value(5, tag("five")),
    value(6, tag("six")),
    value(7, tag("seven")),
    value(8, tag("eight")),
    value(9, tag("nine")),
  ))(i)
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
