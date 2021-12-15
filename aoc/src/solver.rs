pub trait Parser<Input> {
    fn parse(data: &str) -> Input;
}

impl<ParserType, T> Parser<Vec<T>> for ParserType
where
    T: std::str::FromStr,
    <T as std::str::FromStr>::Err: std::fmt::Debug,
{
    fn parse(data: &str) -> Vec<T> {
        data.lines()
            .map(|x| x.trim().parse::<T>().unwrap())
            .collect()
    }
}

impl<ParserType> Parser<String> for ParserType {
    fn parse(data: &str) -> String {
        data.to_owned()
    }
}

pub trait Solver<Input, Output1 = usize, Output2 = Output1> {
    fn part1(data: Input) -> Output1;
    fn part2(data: Input) -> Output2;
}
