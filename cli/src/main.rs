use aoc_cli::{days, download, template};
use clap::{App, Arg, SubCommand};

fn main() {
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
        .subcommand(
            SubCommand::with_name("exec")
                .about("run a specific day")
                .arg(
                    Arg::with_name("DAY")
                        .help("specify the day number")
                        .required(true),
                ),
        )
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("setup") {
        // Set up a new day
        let day = matches.value_of("DAY").unwrap().parse::<u8>().unwrap();

        println!("Downloading data for Dec {} ...", day);
        download::get_input(2021, day).unwrap();
        println!("... Finished.");

        println!("Setting up template source ...");
        template::setup_day(day).unwrap();
        println!("... Finished.");
    } else if let Some(matches) = matches.subcommand_matches("exec") {
        let day = matches.value_of("DAY").unwrap().parse::<u8>().unwrap();
        days::run_day(day).unwrap();
    }
}
