use anyhow::{Context, Result};
use std::env;
use std::fs;
use std::ops::FnOnce;
use std::path::PathBuf;

/// The primary interface that we use for running our solutions
///
/// This function downloads the dataset if needed, and then solves the problem
/// using the provided function.
pub fn run<P, S, I, O>(year: usize, day: usize, parse: P, solve: S) -> Result<String>
where
    P: FnOnce(&str) -> I,
    S: FnOnce(I) -> O,
    O: std::fmt::Display,
{
    let data = load_data(year, day)?;
    let input = parse(&data);
    let result = solve(input);
    Ok(result.to_string())
}

/// Load the dataset for the given year and day, downloading if needed
fn load_data(year: usize, day: usize) -> Result<String> {
    download_data(year, day)?;
    let path = data_path(year, day)?;
    let data = fs::read_to_string(&path).context(format!("{:?}", path))?;
    Ok(data)
}

// Functions for loading and checking the session key which can be stored in the
// `AOC_SESSION_KEY` environment variable or in the `.aoc` file in the local
// directory.
macro_rules! check_key {
    ( $key:expr ) => {{
        let key = $key.trim();
        if key.len() > 0 {
            return Some(key.to_string());
        }
    }};
}

fn get_session_key() -> Option<String> {
    if let Ok(key) = fs::read_to_string(".aoc") {
        check_key!(key)
    }
    if let Ok(key) = env::var("AOC_SESSION_KEY") {
        check_key!(key)
    }
    None
}

// Functions for downloading the dataset for a given year and day
fn data_dir() -> Result<PathBuf> {
    Ok(match env::var("AOC_DATA_DIR") {
        Ok(dir) => {
            let mut path = PathBuf::new();
            path.push(dir);
            path
        }
        _ => env::current_dir()?,
    })
}

fn data_path(year: usize, day: usize) -> Result<PathBuf> {
    let mut path = data_dir()?;
    path.push(format!("data/{}/{:02}", year, day));
    Ok(path)
}

fn download_data(year: usize, day: usize) -> Result<()> {
    // Skip if the input already exists
    let target_path = data_path(year, day)?;
    if target_path.exists() {
        return Ok(());
    }

    // Download the input file
    let key = get_session_key().context("Unable to load session key")?;
    let url = format!("https://adventofcode.com/{}/day/{}/input", year, day);
    let client = reqwest::blocking::Client::new();
    let mut response = client
        .get(url)
        .header(reqwest::header::COOKIE, format!("session={}", key))
        .send()?
        .error_for_status()?;

    // Create the target directory
    if let Some(parent) = target_path.parent() {
        if !parent.exists() {
            fs::create_dir_all(parent)?;
        }
    }

    // Write the data file
    let mut file = fs::File::create(target_path)?;
    response.copy_to(&mut file)?;

    Ok(())
}
