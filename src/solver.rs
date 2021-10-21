use crate::error::Error;

pub trait Solver {
    type Data;
    fn parse(input: &str) -> Result<Self::Data, Error>;
    fn part1(data: &Self::Data) -> Result<(), Error>;
    fn part2(data: &Self::Data) -> Result<(), Error>;
}
