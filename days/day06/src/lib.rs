use anyhow::Result;
use aoc::solver::Solver;

pub struct Day06;

fn update(state: &mut [usize; 9]) {
    let num_zero = state[0];
    for n in 0..8 {
        state[n] = state[n + 1];
    }
    state[6] += num_zero;
    state[8] = num_zero;
}

fn parse(data: &str) -> [usize; 9] {
    let mut state = [0; 9];
    data.trim().split(',').for_each(|x| {
        state[x.parse::<usize>().unwrap()] += 1;
    });
    state
}

fn solve(n: usize, data: &str) -> usize {
    let mut state = parse(data);
    for _ in 0..n {
        update(&mut state);
    }
    state.iter().sum()
}

impl Solver<&str> for Day06 {
    fn part1(data: &str) -> Result<String> {
        Ok(solve(80, data).to_string())
    }

    fn part2(data: &str) -> Result<String> {
        Ok(solve(256, data).to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve() {
        let data = "3,4,3,1,2";
        assert_eq!(solve(1, &data), 5);
        assert_eq!(solve(18, &data), 26);
        assert_eq!(solve(80, &data), 5934);
    }

    #[test]
    fn test_part1() {
        let data = "3,4,3,1,2";
        assert_eq!(Day06::part1(&data).unwrap(), "5934");
    }

    #[test]
    fn test_part2() {
        let data = "3,4,3,1,2";
        assert_eq!(Day06::part2(&data).unwrap(), "26984457539");
    }
}
