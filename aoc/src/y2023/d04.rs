use crate::parsers::{finish, integer, ws};
use anyhow::Result;
use nom::{
  bytes::complete::{tag, take_until},
  character::complete::{newline, space1},
  multi::separated_list1,
  sequence::{pair, terminated},
  IResult,
};
use std::collections::HashSet;

#[derive(Debug, Default, Clone)]
pub struct Card {
  winning: HashSet<i64>,
  numbers: Vec<i64>,
}

fn card(i: &str) -> IResult<&str, Card> {
  let (i, _) = pair(take_until(": "), tag(": "))(i)?;
  let (i, winning) = terminated(ws(separated_list1(space1, integer)), ws(tag("|")))(i)?;
  let winning = HashSet::from_iter(winning);
  let (i, numbers) = ws(separated_list1(space1, integer))(i)?;
  Ok((i, Card { winning, numbers }))
}

pub fn parse(data: &str) -> Result<Vec<Card>> {
  finish(separated_list1(newline, card)(data))
}

pub fn part1(data: &[Card]) -> i64 {
  data
    .iter()
    .map(|card| {
      card.numbers.iter().fold(0, |s, n| {
        if card.winning.contains(n) {
          std::cmp::max(1, 2 * s)
        } else {
          s
        }
      })
    })
    .sum()
}

pub fn part2(data: &[Card]) -> usize {
  let matches: Vec<usize> = data
    .iter()
    .map(|c| {
      c.numbers
        .iter()
        .map(|n| c.winning.contains(n) as usize)
        .sum()
    })
    .collect();
  let mut copies = vec![1usize; matches.len()];
  for (n, &m) in matches.iter().enumerate() {
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