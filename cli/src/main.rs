use anyhow::{Context, Result};
use aoc_cli::{days, days::MAX_DAY, download, template};
use clap::{App, Arg, SubCommand};

fn main() -> Result<()> {
    let matches = App::new("Advent of Code")
        .about("holiday hacking")
        .subcommand(
            SubCommand::with_name("setup")
                .about("sets up a new day")
                .arg(
                    Arg::with_name("DAY")
                        .help("specify the day number")
                        .required(true),
                ),
        )
        .subcommand(SubCommand::with_name("download").about("download any missing data"))
        .arg(Arg::with_name("DAY").help("specify the day number"))
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("setup") {
        // Set up a new day
        let day = matches
            .value_of("DAY")
            .context("reading day number from command line")?
            .parse::<u8>()?;
        println!("Setting up template source ...");
        template::setup_day(day)?;
        println!("... Finished.");
    } else if matches.subcommand_matches("download").is_some() {
        for day in 1..=MAX_DAY {
            println!("Downloading data for Dec {} ...", day);
            download::get_input(day)?;
            println!("... Finished.");
        }
    } else {
        // Get the selected day number, or we'll just do all of the days otherwise
        let day = matches
            .value_of("DAY")
            .map(|v| v.parse::<u8>().ok())
            .flatten();

        if let Some(day) = day {
            days::run_day(day)?;
        } else {
            for day in 1..=MAX_DAY {
                days::run_day(day)?;
            }
        }
    }

    Ok(())
}
