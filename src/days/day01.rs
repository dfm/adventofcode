use crate::error::{Error, ErrorKind, Result};
use crate::solver::Solver;

struct Day01 {}

impl Solver for Day01 {
    type Data = Vec<i32>;

    fn parse(_input: &str) -> Result<Self::Data> {
        Err(Error::new(ErrorKind::NotImplementedError))
    }

    fn part1(_data: &Self::Data) -> Result<()> {
        Err(Error::new(ErrorKind::NotImplementedError))
    }

    fn part2(_data: &Self::Data) -> Result<()> {
        Err(Error::new(ErrorKind::NotImplementedError))
    }
}
