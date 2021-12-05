use anyhow::{Context, Result};
use std::env;
use std::path::{Path, PathBuf};

pub const YEAR: u16 = 2021;

pub fn data_dir() -> Result<PathBuf> {
    Ok(match env::var("AOC_DATA_DIR") {
        Ok(dir) => {
            let mut path = PathBuf::new();
            path.push(dir);
            path
        }
        _ => {
            if let Ok(manifest_dir) = env::var("CARGO_MANIFEST_DIR") {
                Path::new(&manifest_dir)
                    .parent()
                    .context("Finding path to manifest dir")?
                    .to_path_buf()
            } else {
                env::current_dir()?
            }
        }
    })
}

pub fn data_path(day: u8) -> Result<PathBuf> {
    let mut path = data_dir()?;
    path.push(format!("data/{}/{:02}", YEAR, day));
    Ok(path)
}
