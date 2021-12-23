use aoc::solver::Solver;
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap};

pub struct Day23;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Fill {
    Empty,
    A(u8),
    B(u8),
    C(u8),
    D(u8),
}
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct State<const SIZE: usize>([Fill; SIZE]);

type Edges = Vec<usize>;
struct Layout<const SIZE: usize>([Edges; SIZE]);

// LAYOUT 1:
// 8  9 10 11 12 13 14 15 16 17 18
//       1     3     5     7
//       0     2     4     6
fn get_layout1() -> Layout<19> {
    Layout([
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
    ])
}

// LAYOUT 1:
// 16 17 18 19 20 21 22 23 24 25 26
//        3     7    11    15
//        2     6    10    14
//        1     5     9    13
//        0     4     8    12
fn get_layout2() -> Layout<27> {
    Layout([
        vec![1],
        vec![0, 2],
        vec![1, 3],
        vec![2, 18],
        vec![5],
        vec![4, 6],
        vec![5, 7],
        vec![6, 20],
        vec![9],
        vec![8, 10],
        vec![9, 11],
        vec![10, 22],
        vec![13],
        vec![12, 14],
        vec![13, 15],
        vec![14, 24],
        vec![17],
        vec![16, 18],
        vec![3, 17, 19],
        vec![18, 20],
        vec![7, 19, 21],
        vec![20, 22],
        vec![11, 21, 23],
        vec![22, 24],
        vec![15, 23, 25],
        vec![24, 26],
        vec![25],
    ])
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

fn parse<const SIZE: usize>(data: &str) -> State<SIZE> {
    let mut lines = data.lines();
    let mut state = State([Fill::Empty; SIZE]);
    lines.next().unwrap();
    lines.next().unwrap();
    let step = if SIZE == 19 { 2 } else { 4 };
    for n0 in [step - 1, 0] {
        for (n, c) in lines
            .next()
            .unwrap()
            .chars()
            .skip(3)
            .step_by(2)
            .take(4)
            .enumerate()
        {
            state.0[n0 + step * n] = c.into();
        }
    }
    state
}

// fn get_rep(s: Fill) -> String {
//     match s {
//         Fill::A(x) => format!("A{}", x),
//         Fill::B(x) => format!("B{}", x),
//         Fill::C(x) => format!("C{}", x),
//         Fill::D(x) => format!("D{}", x),
//         _ => "..".to_string(),
//     }
// }

// fn show<const SIZE: usize>(state: &State<SIZE>) {
//     for idx in SIZE - 11..SIZE {
//         print!("{}", get_rep(state.0[idx]));
//     }
//     println!();
//     let step = if SIZE == 19 { 2 } else { 4 };
//     for n in 0..step {
//         print!("    ");
//         for idx in (step - n - 1..step * 4).step_by(step) {
//             print!("{}  ", get_rep(state.0[idx]));
//         }
//         println!();
//     }
//     println!();
// }

fn check_rules<const SIZE: usize>(state: &State<SIZE>, fill: Fill, idx: usize) -> bool {
    if SIZE == 19 {
        match (fill, idx) {
            (_, 10 | 12 | 14 | 16) => false,
            (Fill::A(1), 0) => true,
            (Fill::A(_), 1) => matches!(state.0[0], Fill::A(_)),
            (Fill::B(1), 2) => true,
            (Fill::B(_), 3) => matches!(state.0[2], Fill::B(_)),
            (Fill::C(1), 4) => true,
            (Fill::C(_), 5) => matches!(state.0[4], Fill::C(_)),
            (Fill::D(1), 6) => true,
            (Fill::D(_), 7) => matches!(state.0[6], Fill::D(_)),
            (_, 0..=7) => false,
            (Fill::A(1) | Fill::B(1) | Fill::C(1) | Fill::D(1), _) => false,
            _ => true,
        }
    } else {
        match (fill, idx) {
            (_, 18 | 20 | 22 | 24) => false,

            (Fill::A(_), 0) => true,
            (Fill::A(_), 1) => matches!(state.0[0], Fill::A(_)),
            (Fill::A(_), 2) => matches!(state.0[0], Fill::A(_)) && matches!(state.0[1], Fill::A(_)),
            (Fill::A(_), 3) => {
                matches!(state.0[0], Fill::A(_))
                    && matches!(state.0[1], Fill::A(_))
                    && matches!(state.0[2], Fill::A(_))
            }

            (Fill::B(_), 4) => true,
            (Fill::B(_), 5) => matches!(state.0[4], Fill::B(_)),
            (Fill::B(_), 6) => matches!(state.0[4], Fill::B(_)) && matches!(state.0[5], Fill::B(_)),
            (Fill::B(_), 7) => {
                matches!(state.0[4], Fill::B(_))
                    && matches!(state.0[5], Fill::B(_))
                    && matches!(state.0[6], Fill::B(_))
            }

            (Fill::C(_), 8) => true,
            (Fill::C(_), 9) => matches!(state.0[8], Fill::C(_)),
            (Fill::C(_), 10) => {
                matches!(state.0[8], Fill::C(_)) && matches!(state.0[9], Fill::C(_))
            }
            (Fill::C(_), 11) => {
                matches!(state.0[8], Fill::C(_))
                    && matches!(state.0[9], Fill::C(_))
                    && matches!(state.0[10], Fill::C(_))
            }

            (Fill::D(_), 12) => true,
            (Fill::D(_), 13) => matches!(state.0[12], Fill::D(_)),
            (Fill::D(_), 14) => {
                matches!(state.0[12], Fill::D(_)) && matches!(state.0[13], Fill::D(_))
            }
            (Fill::D(_), 15) => {
                matches!(state.0[12], Fill::D(_))
                    && matches!(state.0[13], Fill::D(_))
                    && matches!(state.0[14], Fill::D(_))
            }

            (_, 0..=15) => false,
            (Fill::A(0) | Fill::B(0) | Fill::C(0) | Fill::D(0), _) => true,
            (Fill::A(_) | Fill::B(_) | Fill::C(_) | Fill::D(_), _) => false,
            _ => true,
        }
    }
}

fn allowed_moves<const SIZE: usize>(
    layout: &Layout<SIZE>,
    state: &State<SIZE>,
    start: usize,
    init_energy: usize,
    disallowed: Option<usize>,
) -> Vec<(usize, State<SIZE>)> {
    let ident = state.0[start];
    let (energy, new_ident) = match ident {
        Fill::A(x) => (1, Fill::A(x + 1)),
        Fill::B(x) => (10, Fill::B(x + 1)),
        Fill::C(x) => (100, Fill::C(x + 1)),
        Fill::D(x) => (1000, Fill::D(x + 1)),
        Fill::Empty => return Vec::new(),
    };
    let mut new_state = *state;
    new_state.0[start] = Fill::Empty;
    let mut moves = Vec::new();
    for &neighbor in layout.0[start].iter().filter(|&&n| Some(n) != disallowed) {
        if matches!(new_state.0[neighbor], Fill::Empty) {
            if check_rules(&new_state, ident, neighbor) {
                new_state.0[neighbor] = new_ident;
                moves.push((init_energy + energy, new_state));
            }
            new_state.0[neighbor] = ident;
            let mut others = allowed_moves(
                layout,
                &new_state,
                neighbor,
                init_energy + energy,
                Some(start),
            );
            new_state.0[neighbor] = Fill::Empty;
            moves.append(&mut others);
        }
    }
    moves
}

fn check<const SIZE: usize>(state: &State<SIZE>) -> bool {
    if SIZE == 19 {
        matches!(state.0[0], Fill::A(_))
            && matches!(state.0[1], Fill::A(_))
            && matches!(state.0[2], Fill::B(_))
            && matches!(state.0[3], Fill::B(_))
            && matches!(state.0[4], Fill::C(_))
            && matches!(state.0[5], Fill::C(_))
            && matches!(state.0[6], Fill::D(_))
            && matches!(state.0[7], Fill::D(_))
    } else {
        matches!(state.0[0], Fill::A(_))
            && matches!(state.0[1], Fill::A(_))
            && matches!(state.0[2], Fill::A(_))
            && matches!(state.0[3], Fill::A(_))
            && matches!(state.0[4], Fill::B(_))
            && matches!(state.0[5], Fill::B(_))
            && matches!(state.0[6], Fill::B(_))
            && matches!(state.0[7], Fill::B(_))
            && matches!(state.0[8], Fill::C(_))
            && matches!(state.0[9], Fill::C(_))
            && matches!(state.0[10], Fill::C(_))
            && matches!(state.0[11], Fill::C(_))
            && matches!(state.0[12], Fill::D(_))
            && matches!(state.0[13], Fill::D(_))
            && matches!(state.0[14], Fill::D(_))
            && matches!(state.0[15], Fill::D(_))
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
struct ToTry<const SIZE: usize> {
    energy: usize,
    state: State<SIZE>,
}

impl<const SIZE: usize> Ord for ToTry<SIZE> {
    fn cmp(&self, other: &Self) -> Ordering {
        other
            .energy
            .cmp(&self.energy)
            .then_with(|| self.state.cmp(&other.state))
    }
}

impl<const SIZE: usize> PartialOrd for ToTry<SIZE> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn dijkstra<const SIZE: usize>(layout: &Layout<SIZE>, init_state: &State<SIZE>) -> Option<usize> {
    let mut total_energy = HashMap::new();
    let mut stack = BinaryHeap::new();
    stack.push(ToTry {
        energy: 0,
        state: *init_state,
    });
    while let Some(ToTry { energy, state }) = stack.pop() {
        if check(&state) {
            return Some(energy);
        }
        let &prev_energy = total_energy.get(&state).unwrap_or(&usize::MAX);
        if energy > prev_energy {
            continue;
        }
        for idx in 0..SIZE {
            if matches!(state.0[idx], Fill::Empty)
                || matches!(
                    state.0[idx],
                    Fill::A(2) | Fill::B(2) | Fill::C(2) | Fill::D(2)
                )
            {
                continue;
            }
            for (new_energy, new_state) in allowed_moves(layout, &state, idx, energy, None) {
                let &prev_energy = total_energy.get(&new_state).unwrap_or(&usize::MAX);
                if new_energy < prev_energy {
                    stack.push(ToTry {
                        energy: new_energy,
                        state: new_state,
                    });
                    total_energy.insert(new_state, new_energy);
                }
            }
        }
    }
    None
}

impl Solver<&str> for Day23 {
    fn part1(data: &str) -> usize {
        let layout = get_layout1();
        let state: State<19> = parse(data);
        dijkstra(&layout, &state).unwrap()
    }

    fn part2(data: &str) -> usize {
        let layout = get_layout2();
        let mut state: State<27> = parse(data);
        state.0[1] = Fill::D(0);
        state.0[2] = Fill::D(0);
        state.0[5] = Fill::B(0);
        state.0[6] = Fill::C(0);
        state.0[9] = Fill::A(0);
        state.0[10] = Fill::B(0);
        state.0[13] = Fill::C(0);
        state.0[14] = Fill::A(0);
        dijkstra(&layout, &state).unwrap()
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
        assert_eq!(Day23::part2(DATA), 44169);
    }
}
