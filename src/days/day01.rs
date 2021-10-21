use crate::error::{Error, ErrorKind};
use crate::solver::Solver;

struct Day01 {}

impl Solver for Day01 {
    type Data = Vec<i32>;

    fn parse(input: &str) -> Result<Self::Data, Error> {
        Err(Error::new(ErrorKind::NotImplementedError))
    }

    fn part1(data: &Self::Data) -> Result<(), Error> {
        Err(Error::new(ErrorKind::NotImplementedError))
    }

    fn part2(data: &Self::Data) -> Result<(), Error> {
        Err(Error::new(ErrorKind::NotImplementedError))
    }
}
