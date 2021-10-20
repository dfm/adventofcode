use aoc::download;
use clap::{App, Arg, SubCommand};

fn main() {
    let matches = App::new("Advent of Code")
        .about("holiday hacking")
        .arg(
            Arg::with_name("year")
                .short("y")
                .long("year")
                .value_name("YEAR")
                .help("specify the year number")
                .takes_value(true),
        )
        .subcommand(
            SubCommand::with_name("setup")
                .about("sets up a new day")
                .arg(
                    Arg::with_name("DAY")
                        .help("specify the day number")
                        .required(true),
                ),
        )
        .get_matches();

    // Parse the year if provided
    let year = matches
        .value_of("year")
        .unwrap_or("2021")
        .parse::<u16>()
        .unwrap();

    // Set up a new day
    if let Some(matches) = matches.subcommand_matches("setup") {
        let day = matches.value_of("DAY").unwrap().parse::<u8>().unwrap();
        println!("Downloading data for Dec {}, {} ...", day, year);
        download::get_input(year, day).unwrap();
        println!("... Finished.")
    }
}
