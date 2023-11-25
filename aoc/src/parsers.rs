use anyhow::{anyhow, Context};
use nom::{
  branch::alt,
  character::complete::{char, digit1, newline},
  combinator::{opt, recognize},
  multi::separated_list1,
  sequence::pair,
  IResult, ParseTo,
};

type Result<'a, O = &'a str> = IResult<&'a str, O>;

pub fn finish<O>(result: Result<O>) -> anyhow::Result<O> {
  let (rest, result) = result
    .map_err(|e| e.to_owned())
    .context("Failed to apply parser")?;
  if rest.is_empty() {
    Ok(result)
  } else {
    Err(anyhow!("Incomplete parsing: {}", rest))
  }
}

pub fn newline_separated<P, O>(p: P, i: &str) -> Result<Vec<O>>
where
  P: FnMut(&str) -> Result<O>,
{
  separated_list1(newline, p)(i)
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
