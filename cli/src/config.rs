use anyhow::Result;
use std::env;
use std::path::PathBuf;

pub const YEAR: u16 = 2021;

pub fn data_dir() -> Result<PathBuf> {
    Ok(match env::var("AOC_DATA_DIR") {
        Ok(dir) => {
            let mut path = PathBuf::new();
            path.push(dir);
            path
        }
        _ => env::current_dir()?,
    })
}

pub fn data_path(day: u8) -> Result<PathBuf> {
    let mut path = data_dir()?;
    path.push(format!("data/{}/{:02}", YEAR, day));
    Ok(path)
}
