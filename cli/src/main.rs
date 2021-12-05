use anyhow::Result;
use aoc_cli::{days, days::MAX_DAY, download, template};
use clap::{App, Arg, SubCommand};

fn main() -> Result<()> {
    let matches = App::new("Advent of Code")
        .about("holiday hacking")
        .subcommand(SubCommand::with_name("new").about("sets up the next day"))
        .subcommand(SubCommand::with_name("download").about("download any missing data"))
        .arg(Arg::with_name("DAY").help("specify the day number"))
        .get_matches();

    if matches.subcommand_matches("new").is_some() {
        let day = MAX_DAY + 1;
        println!("Setting up template source ...");
        template::setup_day(day)?;
        println!("... Finished.");
    } else if matches.subcommand_matches("download").is_some() {
        for day in 1..=MAX_DAY {
            download::get_input(day)?;
        }
    } else {
        // Get the selected day number, or we'll just do all of the days otherwise
        let day = matches
            .value_of("DAY")
            .map(|v| v.parse::<u8>().ok())
            .flatten();

        if let Some(day) = day {
            println!("=> DAY {:02}", day);
            let (p1, p2) = days::run_day(day)?;
            println!(" -> part 1: {}", p1);
            println!(" -> part 2: {}\n", p2);
        } else {
            for day in 1..=MAX_DAY {
                println!("=> DAY {:02}", day);
                let (p1, p2) = days::run_day(day)?;
                println!(" -> part 1: {}", p1);
                println!(" -> part 2: {}\n", p2);
            }
        }
    }

    Ok(())
}
