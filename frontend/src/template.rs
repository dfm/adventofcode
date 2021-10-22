use crate::error::Result;
use std::fs;
use std::path::Path;
use toml_edit;

pub fn setup_day(day: u8) -> Result<()> {
    copy_files(day)?;
    update_root_cargo(day)?;
    update_frontend_cargo(day)?;
    update_runner(day)?;
    Ok(())
}

fn copy_one_file(day: u8, from: &Path, to: &Path) -> Result<()> {
    fs::create_dir_all(to.parent().unwrap())?;
    let lib = fs::read_to_string(from)?;
    fs::write(to, lib.replace("{{ day }}", &format!("{:02}", day)))?;
    Ok(())
}

fn copy_files(day: u8) -> Result<()> {
    let template_path = Path::new("days/_template");
    let target_path = format!("days/day{:02}", day);
    let target_path = Path::new(&target_path);

    copy_one_file(
        day,
        &template_path.join("Cargo.toml"),
        &target_path.join("Cargo.toml"),
    )?;

    copy_one_file(
        day,
        &template_path.join("src/lib.rs"),
        &target_path.join("src/lib.rs"),
    )?;

    Ok(())
}

fn update_root_cargo(day: u8) -> Result<()> {
    let data = fs::read_to_string("Cargo.toml")?;
    let mut toml = data.parse::<toml_edit::Document>()?;
    let members = toml["workspace"]["members"].as_array_mut().unwrap();
    members.push(&format!("days/day{:02}", day));
    fs::write("Cargo.toml", toml.to_string())?;
    Ok(())
}

fn update_frontend_cargo(day: u8) -> Result<()> {
    let data = fs::read_to_string("frontend/Cargo.toml")?;
    let mut toml = data.parse::<toml_edit::Document>()?;
    let deps = toml["dependencies"].as_table_mut().unwrap();
    let item = format!("{{ path = \"../days/day{:02}\" }}", day);
    let item = item.parse::<toml_edit::Item>()?;
    let key = format!("aoc-day{:02}", day);
    deps.insert(&key, item);
    fs::write("frontend/Cargo.toml", toml.to_string())?;
    Ok(())
}

fn update_runner(day: u8) -> Result<()> {
    let src = fs::read_to_string("frontend/src/days.rs")?;
    let src = src.replace(
        "// __USE",
        &format!("use aoc_day{0:02}::Day{0:02};\n// __USE", day),
    );
    let src = src.replace(
        "// __MATCH",
        &format!("{0} => run_solver!(Day{0:02}),\n        // __MATCH", day),
    );
    fs::write("frontend/src/days.rs", src)?;
    Ok(())
}
