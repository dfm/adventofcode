use crate::parsers::{finish, integer, newline_separated};
use anyhow::Result;

pub fn parse(data: &str) -> Result<Vec<i64>> {
  finish(newline_separated(integer, data))
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

  #[test]
  fn test_parse() {
    let result = parse(
      r"0
10",
    )
    .unwrap();
    assert_eq!(result.len(), 2);
    assert_eq!(result, vec![0, 10]);
  }
}
