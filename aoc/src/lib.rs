pub mod runner;
pub use runner::run;

#[macro_export]
macro_rules! main {
    ($($year:expr, $day:expr, $parse:expr, $part1:expr, $part2:expr),*) => {
        fn main() {
            $(
                println!("Part 1: {}", aoc::run($year, $day, $parse, $part1).unwrap());
                println!("Part 2: {}", aoc::run($year, $day, $parse, $part2).unwrap());
            )*
        }
    };
}

#[macro_export]
macro_rules! test {
    ($($func:expr, $input:expr, $expect:expr),*) => {
        $(
            assert_eq!($func($input), $expect);
        )*
    };
}
