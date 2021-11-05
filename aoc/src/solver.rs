use anyhow::Result;

pub trait Solver {
    type Data;
    fn parse(input: &str) -> Result<Self::Data>;
    fn part1(data: &Self::Data) -> Result<String>;
    fn part2(data: &Self::Data) -> Result<String>;
}
