use anyhow::Result;
use aoc::solver::Solver;

pub struct Day07;

impl Solver<&str> for Day07 {
    fn part1(data: &str) -> Result<String> {
        let mut x = data
            .trim()
            .split(',')
            .map(|x| x.parse::<i32>().unwrap())
            .collect::<Vec<_>>();
        x.sort();
        let mid = x.len() / 2;
        let median = x[mid];
        let fuel = x.iter().fold(0, |c, &v| c + (v - median).abs());

        Ok(fuel.to_string())
    }

    fn part2(data: &str) -> Result<String> {
        let x = data
            .trim()
            .split(',')
            .map(|x| x.parse::<i32>().unwrap())
            .collect::<Vec<_>>();

        let get_fuel = |&x0: &i32| {
            x.iter().fold(0, |c, &v| {
                let k = (v - x0).abs();
                c + k * (k + 1) / 2
            })
        };

        let &min = x.iter().min().unwrap();
        let &max = x.iter().max().unwrap();
        let x0 = (min..=max).min_by_key(get_fuel).unwrap();

        Ok(get_fuel(&x0).to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        let data = "16,1,2,0,4,2,7,1,2,14";
        assert_eq!(Day07::part1(&data).unwrap(), "37");
    }

    #[test]
    fn test_part2() {
        let data = "16,1,2,0,4,2,7,1,2,14";
        assert_eq!(Day07::part2(&data).unwrap(), "168");
    }
}
