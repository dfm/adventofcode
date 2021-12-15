pub trait Solver<T, Output1 = usize, Output2 = Output1> {
    fn part1(data: T) -> Output1;
    fn part2(data: T) -> Output2;
}
