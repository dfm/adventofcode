use anyhow::{anyhow, Context};
use nom::{
  branch::alt,
  character::complete::{char, digit1, newline, space0},
  combinator::{opt, recognize},
  error::ParseError,
  multi::separated_list1,
  sequence::{delimited, pair},
  IResult, ParseTo, Parser,
};

pub type Result<'a, O = &'a str> = IResult<&'a str, O>;

pub fn finish<O>(result: Result<O>) -> anyhow::Result<O> {
  let (rest, result) = result
    .map_err(|e| e.to_owned())
    .context("Failed to apply parser")?;
  if rest.trim().is_empty() {
    Ok(result)
  } else {
    Err(anyhow!("Incomplete parsing: '{}'", rest))
  }
}

pub fn newline_separated<'a, O, E, F>(f: F) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>, E>
where
  E: ParseError<&'a str>,
  F: Copy + Parser<&'a str, O, E>,
{
  move |i: &str| separated_list1(newline, f)(i)
}

pub fn double_newline_separated<'a, O, E, F>(
  f: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>, E>
where
  E: ParseError<&'a str>,
  F: Parser<&'a str, O, E>,
{
  separated_list1(pair(newline, newline), f)
}

pub fn integer<T: std::str::FromStr>(i: &str) -> Result<T> {
  let (i, s) = recognize_integer(i)?;
  match s.parse_to() {
    Some(v) => Ok((i, v)),
    None => Err(nom::Err::Error(nom::error::Error::new(
      i,
      nom::error::ErrorKind::Digit,
    ))),
  }
}

fn sign(i: &str) -> Result<bool> {
  let (i, s) = opt(alt((char('+'), char('-'))))(i)?;
  Ok((i, s.unwrap_or('+') == '+'))
}

fn recognize_integer(s: &str) -> Result {
  recognize(pair(sign, digit1))(s)
}

pub fn ws<'a, T, F, E: ParseError<&'a str>>(f: F) -> impl FnMut(&'a str) -> IResult<&'a str, T, E>
where
  F: Parser<&'a str, T, E>,
{
  delimited(space0, f, space0)
}
