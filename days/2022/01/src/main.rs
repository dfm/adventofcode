type Input = Vec<usize>;

fn parse(input: &str) -> Input {
    aoc::parse_to_vec(input.lines())
}

fn part1(_input: Input) -> usize {
    0
}

fn part2(_input: Input) -> usize {
    0
}

aoc::main!(2022, 1, parse, part1, part2);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        aoc::test!(part1, parse(""), 0);
    }

    #[test]
    fn test_part2() {
        aoc::test!(part2, parse(""), 0);
    }
}
