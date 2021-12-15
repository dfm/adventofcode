use aoc::solver::Solver;

pub struct Day01;

impl Solver<Vec<i32>> for Day01 {
    fn part1(data: Vec<i32>) -> usize {
        data.windows(2).filter(|w| w[0] < w[1]).count()
    }

    fn part2(data: Vec<i32>) -> usize {
        Self::part1(data.windows(3).map(|w| w.iter().sum()).collect())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        let data = vec![199, 200, 208, 210, 200, 207, 240, 269, 260, 263];
        assert_eq!(Day01::part1(data), 7);
    }

    #[test]
    fn test_part2() {
        let data = vec![199, 200, 208, 210, 200, 207, 240, 269, 260, 263];
        assert_eq!(Day01::part2(data), 5);
    }
}
