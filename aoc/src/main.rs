use anyhow::Result;
use aoc::template::copy_template;
use chrono::{Datelike, Local};
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(author, version, long_about = None)]
#[command(about = "Advent of Code 2023")]
struct Cli {
  day: Option<u32>,

  #[arg(short, long)]
  today: bool,

  #[command(subcommand)]
  command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
  Login,
  New,
}

fn main() -> Result<()> {
  let cli = Cli::parse();
  let today: u32 = Local::now().day();

  match cli.command {
    Some(Commands::Login) => {
      let session_key = rpassword::prompt_password("adventofcode.com session key: ")?;
      let session_key_path = aoc::api::session_key_path()?;
      std::fs::write(session_key_path, session_key)?;
      println!("Saved session key")
    }
    Some(Commands::New) => {
      copy_template(cli.day.unwrap_or(today))?;
    }
    None => {
      let days = if cli.today {
        vec![today]
      } else if let Some(day) = cli.day {
        vec![day]
      } else {
        aoc::y2023::DAYS.to_vec()
      };

      for day in days {
        if !aoc::y2023::DAYS.contains(&day) {
          println!("\nSkipping {}; not implemented", day);
          continue;
        }
        println!("\nDay {}:", day);
        let input = aoc::api::get_input(day)?;
        let ((p1, p2), (d0, d1, d2)) = aoc::y2023::solve(day, &input);
        println!("Parsing took {:.2} ms", format_duration(&d0));
        println!("=> Part 1: {} ({:.2} ms)", p1, format_duration(&d1));
        println!("=> Part 2: {} ({:.2} ms)", p2, format_duration(&d2));
      }
    }
  }

  Ok(())
}

fn format_duration(d: &std::time::Duration) -> f64 {
  let t = d.subsec_micros() as f64;
  t / 1e3f64
}
