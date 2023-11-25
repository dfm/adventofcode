use anyhow::{Context, Result};
use std::{fs, path::PathBuf};

fn work_dir(name: &str) -> Result<PathBuf> {
    let mut path = dirs::home_dir().context("Failed to find home directory")?;
    path.push(name);
    path.push("aoc");
    fs::create_dir_all(path.clone())
        .with_context(|| format!("Failed to create: {}", path.display()))?;
    Ok(path)
}

fn cache_dir() -> Result<PathBuf> {
    work_dir(".cache")
}

fn config_dir() -> Result<PathBuf> {
    work_dir(".config")
}

fn data_path(day: u32) -> Result<PathBuf> {
    let mut path = cache_dir()?;
    path.push("2023");
    fs::create_dir_all(path.clone())
        .with_context(|| format!("Failed to create: {}", path.display()))?;
    path.push(format!("{:02}.txt", day));
    Ok(path)
}

pub fn session_key_path() -> Result<PathBuf> {
    let mut path = config_dir()?;
    path.push("key");
    Ok(path)
}

pub fn get_input(day: u32) -> Result<String> {
    let path = data_path(day)?;
    if !path.exists() {
        let key = fs::read_to_string(session_key_path()?).context("Failed to load session key")?;
        let url = format!("https://adventofcode.com/2023/day/{}/input", day);
        let client = reqwest::blocking::Client::new();
        let mut response = client
            .get(url)
            .header(reqwest::header::COOKIE, format!("session={}", key))
            .send()?
            .error_for_status()?;
        let mut file = fs::File::create(path.clone())
            .with_context(|| format!("Failed to create: {}", path.display()))?;
        response
            .copy_to(&mut file)
            .context("Failed to save downloaded data")?;
    }
    Ok(fs::read_to_string(path)?)
}
