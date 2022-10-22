pub mod input;
pub mod runner;
pub use input::Input;
pub use runner::run;

#[macro_export]
macro_rules! main {
    ($($year:expr, $day:expr, $part1:expr, $part2:expr),*) => {
        fn main() {
            $(
                aoc::run($year, $day, $part1).unwrap();
                aoc::run($year, $day, $part2).unwrap();
            )*
        }
    };
}

#[macro_export]
macro_rules! test {
    ($($func:expr, $input:expr, $expect:expr),*) => {
        $(
            let input = aoc::Input::new(&$input.to_string());
            assert_eq!($func(input.into()), $expect);
        )*
    };
}
