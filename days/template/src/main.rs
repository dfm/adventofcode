type Input = String;

fn parse(input: &str) -> Input {
    input.to_string()
}

fn part1(input: Input) -> usize {
    0
}

fn part2(input: Input) -> usize {
    0
}

aoc::main!({{year}}, {{day}}, parse, part1, part2);

#[cfg(test)]
mod tests {
    use super::*;

    const DATA: &str = ""; 

    #[test]
    fn year{{year}}_day{{day}}_part1() {
        aoc::test!(part1, parse(DATA), 0);
    }

    #[test]
    fn year{{year}}_day{{day}}_part2() {
        aoc::test!(part2, parse(DATA), 0);
    }
}
