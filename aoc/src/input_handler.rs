use anyhow::{Context, Result};
use std::fs;
use std::path::Path;

#[derive(Debug, Clone)]
pub struct InputHandler {
    pub data: String,
}

impl InputHandler {
    pub fn new(data: &'_ str) -> Self {
        Self {
            data: data.to_string(),
        }
    }

    pub fn from_file(filename: &Path) -> Result<Self> {
        let data = fs::read_to_string(&filename).context(format!("{:?}", filename))?;
        Ok(Self { data })
    }
}

impl<'a, T> From<&'a InputHandler> for Vec<T>
where
    T: std::str::FromStr,
    <T as std::str::FromStr>::Err: std::fmt::Debug,
{
    fn from(reader: &InputHandler) -> Self {
        reader
            .data
            .lines()
            .map(|x| x.trim().parse::<T>().unwrap())
            .collect()
    }
}

impl<'a> From<&'a InputHandler> for &'a str {
    fn from(reader: &InputHandler) -> &str {
        &reader.data
    }
}
