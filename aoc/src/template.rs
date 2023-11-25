use crate::api::YEAR;
use anyhow::{bail, Context};
use std::{env, path::PathBuf};

pub fn copy_template(day: u32) -> anyhow::Result<()> {
  let mut source_dir = PathBuf::new();
  source_dir.push("src");
  source_dir.push(format!("y{}", YEAR));
  if let Some(manifest_dir) = env::var_os("CARGO_MANIFEST_DIR") {
    source_dir = PathBuf::from(manifest_dir).join(source_dir);
  }

  let template_path = source_dir.join("_template.rs");
  let target_path = source_dir.join(format!("d{:02}.rs", day));

  if target_path.exists() {
    bail!("Source file already exists: {:?}", target_path);
  }

  println!("New source file created: {:?}", target_path);
  std::fs::copy(template_path, target_path).context("Failed to copy template")?;

  Ok(())
}
