use crate::error::Result;

pub trait Solver {
    type Data;
    fn parse(input: &str) -> Result<Self::Data>;
    fn part1(data: &Self::Data) -> Result<()>;
    fn part2(data: &Self::Data) -> Result<()>;
}
