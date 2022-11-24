use anyhow::Result;
use clap::Parser;
use std::fs;
use std::path::PathBuf;

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
        &template_path.join("src/main.rs"),
        &target_path.join("src/main.rs"),
    )?;
    Ok(())
}

fn add_to_workspace(year: usize, day: usize) -> Result<()> {
    let path = get_path(year, day);
    let data = fs::read_to_string("Cargo.toml")?;
    let mut toml = data.parse::<toml_edit::Document>()?;
    let members = toml["workspace"]["members"].as_array_mut().unwrap();
    members.push(path.to_str().unwrap());
    fs::write("Cargo.toml", toml.to_string())?;
    Ok(())
}

fn remove_from_workspace(year: usize, day: usize) -> Result<()> {
    let path = get_path(year, day);
    let path = path.to_str().unwrap();
    let data = fs::read_to_string("Cargo.toml")?;
    let mut toml = data.parse::<toml_edit::Document>()?;
    let members = toml["workspace"]["members"].as_array_mut().unwrap();
    let pos = members.iter().position(|x| x.as_str().unwrap() == path);
    if let Some(pos) = pos {
        members.remove(pos);
        fs::write("Cargo.toml", toml.to_string())?;
    }
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
    #[arg(short, long)]
    remove: bool,
}

fn main() {
    let cli = Cli::parse();
    let year = cli.year.unwrap_or(2022);
    let day = cli.day;

    let path = get_path(year, day);

    if cli.remove {
        if path.join("Cargo.toml").exists() {
            println!("Removing {}", path.display());
            fs::remove_dir_all(&path).unwrap();
            remove_from_workspace(year, day).unwrap();
        } else {
            println!("{} doesn't exist; skipping.", path.display());
        }
        return;
    }

    if path.join("Cargo.toml").exists() {
        if cli.clobber {
            fs::remove_dir_all(&path).unwrap();
        } else {
            println!("{} already exists; skipping.", path.display());
            return;
        }
    }

    copy_files(year, day).unwrap();
    add_to_workspace(year, day).unwrap();
}
