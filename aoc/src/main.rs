use anyhow::Result;
use chrono::{DateTime, Datelike, Utc};
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

  match cli.command {
    Some(Commands::Login) => {
      let session_key = rpassword::prompt_password("adventofcode.com session key: ")?;
      let session_key_path = aoc::api::session_key_path()?;
      std::fs::write(session_key_path, session_key)?;
      println!("Saved session key")
    }
    Some(Commands::New) => {

    }
    None => {
      let days = if cli.today {
        let today: DateTime<Utc> = Utc::now();
        vec![today.day()]
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
        let ((p1, p2), (d0, d1, d2)) = aoc::y2023::solve(day, &input)?;
        println!("Parsing took {} ns", d0.subsec_nanos());
        println!("=> Part 1: {} ({} ns)", p1, d1.subsec_nanos());
        println!("=> Part 2: {} ({} ns)", p2, d2.subsec_nanos());
      }
    }
  }

  Ok(())
}
