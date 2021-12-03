use anyhow::Result;

pub trait Solver<T> {
    fn part1(data: T) -> Result<String>;
    fn part2(data: T) -> Result<String>;
}
