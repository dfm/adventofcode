use crate::parsers::{finish, integer};
use anyhow::Result;
use nom::{character::complete::newline, multi::separated_list1};

pub fn parse(data: &str) -> Result<Vec<i64>> {
  finish(separated_list1(newline, integer)(data))
}

pub fn part1(_data: &[i64]) -> i64 {
  0
}

pub fn part2(_data: &[i64]) -> i64 {
  0
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA: &str = r"0";

  #[test]
  fn test_parse() {
    let result = parse(TEST_DATA).unwrap();
    assert_eq!(result, vec![0]);
  }
}
