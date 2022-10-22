/// Interface for reading and parsing input data
///
/// This module provides out-of-the-box support for problems with `Vec<T>`
/// input, where `T` implements the `FromStr` trait. For problems that require
/// more complex inputs, you can implement the `From<Input>` trait for your
/// type, for example:
///
/// ```
/// use aoc::Input;
///
/// impl From<Input> for T {
///   fn from(reader: Input) -> Self {
///     // ...
///   }
/// }
/// ```
use anyhow::{Context, Result};
use std::{fs, path::Path};

#[derive(Debug, Clone)]
pub struct Input {
    data: String,
}

impl Input {
    pub fn new(data: &str) -> Self {
        Self {
            data: data.to_string(),
        }
    }

    pub fn from_file(path: &Path) -> Result<Self> {
        let data = fs::read_to_string(&path).context(format!("{:?}", path))?;
        Ok(Self { data })
    }
}

impl<T> From<Input> for Vec<T>
where
    T: std::str::FromStr,
    <T as std::str::FromStr>::Err: std::fmt::Debug,
{
    fn from(reader: Input) -> Self {
        reader
            .data
            .lines()
            .map(|x| x.trim().parse::<T>().unwrap())
            .collect()
    }
}

impl From<Input> for String {
    fn from(reader: Input) -> Self {
        reader.data
    }
}
