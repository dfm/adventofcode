use aoc::solver::Solver;

pub struct Day07;

fn parse(data: &str) -> Vec<i32> {
    let mut x: Vec<_> = data.trim().split(',').map(|x| x.parse().unwrap()).collect();
    x.sort_unstable();
    x
}

impl Solver<&str, i32> for Day07 {
    fn part1(data: &str) -> i32 {
        let x = parse(data);
        let mid = x.len() / 2;
        let median = x[mid];
        x.iter().fold(0, |c, &v| c + (v - median).abs())
    }

    fn part2(data: &str) -> i32 {
        let x = parse(data);

        let get_fuel = |x0: i32| {
            x.iter().fold(0, |c, &v| {
                let k = (v - x0).abs();
                c + k * (k + 1) / 2
            })
        };

        let mean = x.iter().sum::<i32>() as f32 / x.len() as f32;
        std::cmp::min(get_fuel(mean as i32), get_fuel(mean as i32 + 1))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        let data = "16,1,2,0,4,2,7,1,2,14";
        assert_eq!(Day07::part1(&data), 37);
    }

    #[test]
    fn test_part2() {
        let data = "16,1,2,0,4,2,7,1,2,14";
        assert_eq!(Day07::part2(&data), 168);
    }
}
