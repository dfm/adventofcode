use anyhow::Result;
use clap::Parser;
use std::fs;
use std::path::PathBuf;
use toml_edit;

fn get_path(year: usize, day: usize) -> PathBuf {
    let path = PathBuf::new();
    path.join(format!("days/{:04}/{:02}", year, day))
}

fn copy_one_file(year: usize, day: usize, from: &PathBuf, to: &PathBuf) -> Result<()> {
    fs::create_dir_all(to.parent().unwrap())?;
    let data = fs::read_to_string(from)?;
    let data = data.replace("{{year}}", &format!("{}", year));
    let data = data.replace("{{day}}", &format!("{}", day));
    fs::write(to, data)?;
    Ok(())
}

fn copy_files(year: usize, day: usize) -> Result<()> {
    let template_path = PathBuf::from("days/template");
    let target_path = get_path(year, day);
    copy_one_file(
        year,
        day,
        &template_path.join("Cargo.toml"),
        &target_path.join("Cargo.toml"),
    )?;
    copy_one_file(
        year,
        day,
        &template_path.join("main.rs"),
        &target_path.join("main.rs"),
    )?;
    Ok(())
}

fn update_workspace(year: usize, day: usize) -> Result<()> {
    let path = get_path(year, day);
    let data = fs::read_to_string("Cargo.toml")?;
    let mut toml = data.parse::<toml_edit::Document>()?;
    let members = toml["workspace"]["members"].as_array_mut().unwrap();
    members.push(path.to_str().unwrap());
    fs::write("Cargo.toml", toml.to_string())?;
    Ok(())
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    day: usize,
    #[arg(short, long)]
    year: Option<usize>,
    #[arg(short, long)]
    clobber: bool,
}

fn main() {
    let cli = Cli::parse();
    let year = cli.year.unwrap_or(2022);
    let day = cli.day;

    let path = get_path(year, day);
    if path.join("Cargo.toml").exists() {
        if cli.clobber {
            fs::remove_dir_all(&path).unwrap();
        } else {
            println!("{} already exists; skipping.", path.to_str().unwrap());
            return;
        }
    }

    copy_files(year, day).unwrap();
    update_workspace(year, day).unwrap();
}
