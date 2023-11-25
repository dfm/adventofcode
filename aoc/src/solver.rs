pub trait Solver<R1, R2 = R1>
where
    R1: std::fmt::Display,
    R2: std::fmt::Display,
{
    type Data;
    fn parse(data: &str) -> Self::Data;
    fn part1(data: &Self::Data) -> R1;
    fn part2(data: &Self::Data) -> R2;
}
