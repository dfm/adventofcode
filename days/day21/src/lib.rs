use aoc::{counter::Counter, solver::Solver};
use std::collections::HashMap;

pub struct Day21;

type State = (usize, usize);
type FullState = (usize, usize, HashMap<State, usize>);

fn parse(data: &str) -> (State, State) {
    let parse_one = |line: &str| line.split(": ").nth(1).unwrap().parse().unwrap();
    let mut lines = data.lines();
    (
        (parse_one(lines.next().unwrap()), 0),
        (parse_one(lines.next().unwrap()), 0),
    )
}

fn step((position, score): &State, delta: usize) -> State {
    let next = ((position - 1) as usize + delta) % 10 + 1;
    (next, score + next)
}

fn advance1(rolls: &mut impl Iterator<Item = usize>, state: &State) -> State {
    let delta = rolls.next().unwrap() + rolls.next().unwrap() + rolls.next().unwrap();
    step(state, delta)
}

fn advance_all(
    states: &mut HashMap<State, usize>,
    (state, weight): (&State, &usize),
) -> (usize, usize) {
    let mut count = 0;
    let mut wins = 0;
    for (delta, n) in [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)] {
        let new_state = step(state, delta);
        if new_state.1 >= 21 {
            wins += n * weight;
        } else {
            states.increment(new_state, weight * n);
            count += n * weight;
        }
    }
    (count, wins)
}

fn advance2(count: usize, (_, wins, states): &FullState) -> FullState {
    let mut tmp = HashMap::new();
    let result = states.iter().fold((0, 0), |(total, wins), x| {
        let (all, more_wins) = advance_all(&mut tmp, x);
        (total + all, wins + more_wins)
    });
    (result.0, wins + count * result.1, tmp)
}

impl Solver<&str> for Day21 {
    fn part1(data: &str) -> usize {
        let mut count = 0;
        let mut rolls = (0usize..).map(|n| n % 100 + 1);
        let (mut s1, mut s2) = parse(data);
        while (s1.1 < 1000) && (s2.1 < 1000) {
            if count % 2 == 0 {
                s1 = advance1(&mut rolls, &s1);
            } else {
                s2 = advance1(&mut rolls, &s2);
            }
            count += 1;
        }
        std::cmp::min(s1.1, s2.1) * count * 3
    }

    fn part2(data: &str) -> usize {
        let (s1, s2) = parse(data);
        let mut state1 = (1, 0, HashMap::new());
        state1.2.insert(s1, 1);
        let mut state2 = (1, 0, HashMap::new());
        state2.2.insert(s2, 1);
        for _ in 0..10 {
            state1 = advance2(state2.0, &state1);
            state2 = advance2(state1.0, &state2);
        }
        std::cmp::max(state1.1, state2.1)
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
