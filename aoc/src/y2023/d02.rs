use crate::parsers::{finish, integer};
use anyhow::Result;
use nom::{
  branch::alt, bytes::complete::tag, character::complete::newline, combinator::value,
  multi::separated_list1, sequence::pair, IResult,
};

#[derive(Copy, Clone, Debug)]
struct Draw(i64, i64, i64);

#[derive(Clone, Debug)]
pub struct Game {
  number: i64,
  draws: Vec<Draw>,
}

#[derive(Copy, Clone, Debug)]
enum Color {
  Red,
  Green,
  Blue,
}

fn one(i: &str) -> IResult<&str, (i64, Color)> {
  pair(
    integer,
    alt((
      value(Color::Red, tag(" red")),
      value(Color::Green, tag(" green")),
      value(Color::Blue, tag(" blue")),
    )),
  )(i)
}

fn draw(i: &str) -> IResult<&str, Draw> {
  let (i, parts) = separated_list1(tag(", "), one)(i)?;
  let mut draw = Draw(0, 0, 0);
  for (n, c) in parts {
    match c {
      Color::Red => draw.0 += n,
      Color::Green => draw.1 += n,
      Color::Blue => draw.2 += n,
    }
  }
  Ok((i, draw))
}

fn game(i: &str) -> IResult<&str, Game> {
  let (i, _) = tag("Game ")(i)?;
  let (i, number) = integer(i)?;
  let (i, _) = tag(": ")(i)?;
  let (i, draws) = separated_list1(tag("; "), draw)(i)?;
  Ok((i, Game { number, draws }))
}

pub fn parse(data: &str) -> Result<Vec<Game>> {
  finish(separated_list1(newline, game)(data))
}

pub fn part1(data: &[Game]) -> i64 {
  data
    .iter()
    .map(|game| {
      if game
        .draws
        .iter()
        .all(|draw| draw.0 <= 12 && draw.1 <= 13 && draw.2 <= 14)
      {
        game.number
      } else {
        0
      }
    })
    .sum()
}

pub fn part2(data: &[Game]) -> i64 {
  data
    .iter()
    .map(|game| {
      let a = game.draws.iter().map(|d| d.0).max().unwrap();
      let b = game.draws.iter().map(|d| d.1).max().unwrap();
      let c = game.draws.iter().map(|d| d.2).max().unwrap();
      a * b * c
    })
    .sum()
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA: &str = r"Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA).unwrap();
    assert_eq!(part1(&data), 8);
  }

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA).unwrap();
    assert_eq!(part2(&data), 2286);
  }
}
