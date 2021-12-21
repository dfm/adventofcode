use aoc::{counter::Counter, solver::Solver};
use std::collections::HashMap;

pub struct Day21;

type State = (u8, usize);

fn parse(data: &str) -> (u8, u8) {
    let parse_one = |line: &str| line.split(": ").skip(1).next().unwrap().parse().unwrap();
    let mut lines = data.lines();
    (
        parse_one(lines.next().unwrap()),
        parse_one(lines.next().unwrap()),
    )
}

fn step1(rolls: &mut impl Iterator<Item = usize>, (current, score): State) -> State {
    let delta = rolls.next().unwrap() + rolls.next().unwrap() + rolls.next().unwrap();
    let next = ((current - 1) as usize + delta) % 10 + 1;
    (next as u8, score + next)
}

fn step2(
    states: &mut HashMap<State, usize>,
    ((current, score), weight): (&State, &usize),
) -> (usize, usize) {
    let mut count = 0;
    let mut wins = 0;
    for (d, n) in [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)] {
        let next = ((current - 1) as usize + d) % 10 + 1;
        if score + next >= 21 {
            wins += n * weight;
        } else {
            states.increment((next as u8, score + next), weight * n);
            count += n * weight;
        }
    }
    (count, wins)
}

impl Solver<&str> for Day21 {
    fn part1(data: &str) -> usize {
        let mut s1 = 0;
        let mut s2 = 0;
        let (mut p1, mut p2) = parse(data);
        let mut rolls = (0usize..).map(|n| n % 100 + 1);
        let mut count = 0;
        loop {
            let result = step1(&mut rolls, (p1, s1));
            p1 = result.0;
            s1 = result.1;
            count += 3;
            if s1 >= 1000 {
                return s2 * count;
            }

            let result = step1(&mut rolls, (p2, s2));
            p2 = result.0;
            s2 = result.1;
            count += 3;
            if s2 >= 1000 {
                return s1 * count;
            }
        }
    }

    fn part2(data: &str) -> usize {
        let mut num2 = 1;
        let mut w1 = 0;
        let mut w2 = 0;
        let (p1, p2) = parse(data);
        let mut s1 = HashMap::new();
        s1.insert((p1, 0), 1);
        let mut s2 = HashMap::new();
        s2.insert((p2, 0), 1);

        for _ in 0..10 {
            let mut tmp = HashMap::new();
            let (num1, dw) = s1.iter().fold((0, 0), |(l, w), x| {
                let (dl, dw) = step2(&mut tmp, x);
                (l + dl, w + dw)
            });
            w1 += num2 * dw;
            s1 = tmp;

            let mut tmp = HashMap::new();
            let result = s2.iter().fold((0, 0), |(l, w), x| {
                let (dl, dw) = step2(&mut tmp, x);
                (l + dl, w + dw)
            });
            num2 = result.0;
            w2 += num1 * result.1;
            s2 = tmp;
        }

        std::cmp::max(w1, w2)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const DATA: &str = "Player 1 starting position: 4
Player 2 starting position: 8
";

    #[test]
    fn test_part1() {
        assert_eq!(Day21::part1(DATA), 739785);
    }

    #[test]
    fn test_part2() {
        assert_eq!(Day21::part2(DATA), 444356092776315);
    }
}
