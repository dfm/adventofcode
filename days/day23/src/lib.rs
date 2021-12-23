use aoc::solver::Solver;
use std::collections::{HashMap, VecDeque};

pub struct Day23;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum Fill {
    Empty,
    A(u8),
    B(u8),
    C(u8),
    D(u8),
}
type State = [Fill; 19];

// LAYOUT:
// 8  9 10 11 12 13 14 15 16 17 18
//       1     3     5     7
//       0     2     4     6
type Edges = Vec<usize>;
type Layout = [Edges; 19];
fn get_layout() -> Layout {
    [
        vec![1],
        vec![0, 10],
        vec![3],
        vec![2, 12],
        vec![5],
        vec![4, 14],
        vec![7],
        vec![6, 16],
        vec![9],
        vec![8, 10],
        vec![1, 9, 11],
        vec![10, 12],
        vec![3, 11, 13],
        vec![12, 14],
        vec![5, 13, 15],
        vec![14, 16],
        vec![7, 15, 17],
        vec![16, 18],
        vec![17],
    ]
}

impl From<char> for Fill {
    fn from(c: char) -> Self {
        match c {
            'A' => Fill::A(0),
            'B' => Fill::B(0),
            'C' => Fill::C(0),
            'D' => Fill::D(0),
            _ => Fill::Empty,
        }
    }
}

fn parse(data: &str) -> State {
    let mut lines = data.lines();
    let mut state = [Fill::Empty; 19];
    lines.next().unwrap();
    lines.next().unwrap();
    for n0 in [1, 0] {
        for (n, c) in lines
            .next()
            .unwrap()
            .chars()
            .skip(3)
            .step_by(2)
            .take(4)
            .enumerate()
        {
            state[n0 + 2 * n] = c.into();
        }
    }
    state
}

// fn get_rep(s: Fill) -> char {
//     match s {
//         Fill::A(_) => 'A',
//         Fill::B(_) => 'B',
//         Fill::C(_) => 'C',
//         Fill::D(_) => 'D',
//         _ => '.',
//     }
// }

// fn show(state: &State) {
//     for idx in 8..19 {
//         print!("{}", get_rep(state[idx]));
//     }
//     println!();
//     print!("  ");
//     for idx in (1..8).step_by(2) {
//         print!("{} ", get_rep(state[idx]));
//     }
//     println!();
//     print!("  ");
//     for idx in (0..8).step_by(2) {
//         print!("{} ", get_rep(state[idx]));
//     }
//     println!();
// }

fn check_rules(state: &State, fill: Fill, idx: usize) -> bool {
    match (fill, idx) {
        (_, 10 | 12 | 14 | 16) => false,
        (Fill::A(1), 0) => true,
        (Fill::A(_), 1) => matches!(state[0], Fill::A(_)),
        (Fill::B(1), 2) => true,
        (Fill::B(_), 3) => matches!(state[2], Fill::B(_)),
        (Fill::C(1), 4) => true,
        (Fill::C(_), 5) => matches!(state[4], Fill::C(_)),
        (Fill::D(1), 6) => true,
        (Fill::D(_), 7) => matches!(state[6], Fill::D(_)),
        (_, 0..=7) => false,
        (Fill::A(1) | Fill::B(1) | Fill::C(1) | Fill::D(1), _) => false,
        _ => true,
    }
}

fn allowed_moves(
    layout: &Layout,
    state: &State,
    start: usize,
    init_energy: usize,
    disallowed: Option<usize>,
) -> Vec<(usize, State)> {
    let ident = state[start];
    let (energy, new_ident) = match ident {
        Fill::A(x) => (1, Fill::A(x + 1)),
        Fill::B(x) => (10, Fill::B(x + 1)),
        Fill::C(x) => (100, Fill::C(x + 1)),
        Fill::D(x) => (1000, Fill::D(x + 1)),
        Fill::Empty => return Vec::new(),
    };
    let mut new_state = *state;
    new_state[start] = Fill::Empty;
    let mut moves = Vec::new();
    for &neighbor in layout[start].iter().filter(|&&n| Some(n) != disallowed) {
        if matches!(state[neighbor], Fill::Empty) {
            if check_rules(state, ident, neighbor) {
                new_state[neighbor] = new_ident;
                moves.push((init_energy + energy, new_state));
            }
            new_state[neighbor] = ident;
            let mut others = allowed_moves(
                layout,
                &new_state,
                neighbor,
                init_energy + energy,
                Some(start),
            );
            new_state[neighbor] = Fill::Empty;
            moves.append(&mut others);
        }
    }
    moves
}

fn check(state: &State) -> bool {
    matches!(state[0], Fill::A(_))
        && matches!(state[1], Fill::A(_))
        && matches!(state[2], Fill::B(_))
        && matches!(state[3], Fill::B(_))
        && matches!(state[4], Fill::C(_))
        && matches!(state[5], Fill::C(_))
        && matches!(state[6], Fill::D(_))
        && matches!(state[7], Fill::D(_))
}

fn dijkstra(init_state: &State) -> usize {
    let layout = get_layout();
    let mut win = usize::MAX;
    let mut total_energy = HashMap::new();
    let mut stack = VecDeque::new();
    stack.push_back((0, *init_state));
    while let Some((energy, state)) = stack.pop_front() {
        if energy >= win {
            continue;
        }
        if check(&state) {
            win = win.min(energy);
            continue;
        }
        let &prev_energy = total_energy.get(&state).unwrap_or(&usize::MAX);
        if energy > prev_energy {
            continue;
        }
        for idx in 0..19 {
            if matches!(state[idx], Fill::Empty)
                || matches!(
                    state[idx],
                    Fill::A(2) | Fill::B(2) | Fill::C(2) | Fill::D(2)
                )
            {
                continue;
            }
            for (new_energy, new_state) in allowed_moves(&layout, &state, idx, energy, None) {
                let &prev_energy = total_energy.get(&new_state).unwrap_or(&usize::MAX);
                if new_energy < prev_energy {
                    stack.push_back((new_energy, new_state));
                    total_energy.insert(new_state, new_energy);
                }
            }
        }
    }
    win
}

impl Solver<&str> for Day23 {
    fn part1(data: &str) -> usize {
        let state = parse(data);
        dijkstra(&state)
    }

    fn part2(_data: &str) -> usize {
        0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const DATA: &str = "#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########";

    #[test]
    fn test_part1() {
        assert_eq!(Day23::part1(DATA), 12521);
    }

    #[test]
    fn test_part2() {
        assert_eq!(Day23::part2(DATA), 0);
    }
}
