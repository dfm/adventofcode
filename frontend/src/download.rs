use aoc::Result;
use std::env;
use std::fs;
use std::path::Path;

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

pub fn get_input(year: u16, day: u8) -> Result<()> {
    let key = get_session_key().ok_or("Unable to load session key")?;

    // Skip if the input already exists
    let filename = format!("data/{}/{:02}", year, day);
    let target_path = Path::new(&filename);
    if target_path.exists() {
        return Ok(());
    }

    // Download the input file
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

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
