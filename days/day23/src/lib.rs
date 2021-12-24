use aoc::solver::Solver;
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap};

pub struct Day23;

type Token = (usize, u8);
const HALLWAY_LEN: usize = 11;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
struct Room<const SIZE: usize> {
    allowed: usize,
    content: [Option<Token>; SIZE],
}

#[derive(Copy, Clone, Debug)]
struct State<const SIZE: usize> {
    energy: usize,
    rooms: [Room<SIZE>; 4],
    hallway: [Option<Token>; HALLWAY_LEN],
}

impl<const SIZE: usize> Room<SIZE> {
    fn new(allowed: usize) -> Self {
        Self {
            allowed,
            content: [None; SIZE],
        }
    }

    fn maybe_exit(&mut self) -> Option<(usize, Token)> {
        if self.check_stack().is_some() {
            return None;
        }
        for (n, token) in self.content.iter_mut().rev().enumerate() {
            if let Some(value) = *token {
                if value.1 > 0 {
                    return None;
                }
                *token = None;
                return Some(((n + 1) * value.0, value));
            }
        }
        None
    }

    fn maybe_enter(&mut self, token: &Token) -> Option<usize> {
        let &(value, moves) = token;
        if value != self.allowed {
            return None;
        }
        if let Some(idx) = self.check_stack() {
            self.content[idx] = Some((value, moves + 1));
            return Some(token.0 * (SIZE - idx));
        }
        None
    }

    fn check_stack(&self) -> Option<usize> {
        let mut count = 0;
        for c in self.content.iter() {
            if let Some((c, _)) = *c {
                if c != self.allowed {
                    return None;
                }
                count += 1;
            }
        }
        Some(count)
    }

    fn check(&self) -> bool {
        if let Some(c) = self.check_stack() {
            c == SIZE
        } else {
            false
        }
    }
}

impl<const SIZE: usize> State<SIZE> {
    fn new(data: &str) -> Self {
        let mut rooms = [Room::new(1), Room::new(10), Room::new(100), Room::new(1000)];
        let mut lines = data.lines();
        lines.next().unwrap();
        lines.next().unwrap();
        for i in [SIZE - 1, 0] {
            for (n, c) in lines
                .next()
                .unwrap()
                .chars()
                .skip(3)
                .step_by(2)
                .take(4)
                .enumerate()
            {
                rooms[n].content[i] = match c {
                    'A' => Some((1, 0)),
                    'B' => Some((10, 0)),
                    'C' => Some((100, 0)),
                    'D' => Some((1000, 0)),
                    _ => None,
                }
            }
        }
        Self {
            energy: 0,
            rooms,
            hallway: [None; HALLWAY_LEN],
        }
    }

    fn get_all_moves(&self) -> Vec<Self> {
        let mut moves = Vec::new();
        for room in 0..4 {
            if let Some(more) = self.exit_room(room) {
                moves.extend(more);
            }
        }
        for (n, token) in self.hallway.iter().enumerate() {
            if let Some(token) = token {
                moves.extend(self.without(n).get_hallway_moves(token, n, n));
            }
        }
        moves
    }

    fn without(&self, at: usize) -> Self {
        let mut new_state = *self;
        new_state.hallway[at] = None;
        new_state
    }

    fn with(&self, token: Token, at: usize) -> Self {
        let mut new_state = *self;
        new_state.hallway[at] = Some(token);
        new_state
    }

    fn exit_room(&self, idx: usize) -> Option<Vec<Self>> {
        let current = 2 + idx * 2;
        if self.hallway[current].is_none() {
            let mut new_state = *self;
            if let Some((delta, token)) = new_state.rooms[idx].maybe_exit() {
                new_state.energy += delta;
                return Some(new_state.get_hallway_moves(&token, current, current));
            }
        }
        None
    }

    fn maybe_enter_room(&self, token: &Token, current: usize) -> Option<Self> {
        match current {
            2 | 4 | 6 | 8 => {
                let mut new_state = *self;
                if let Some(delta) = new_state.rooms[current / 2 - 1].maybe_enter(token) {
                    new_state.energy += delta;
                    Some(new_state)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn get_hallway_moves(&self, token: &Token, current: usize, previous: usize) -> Vec<Self> {
        let mut moves = Vec::new();
        let mut new_state = *self;
        new_state.energy += token.0;

        match current {
            2 | 4 | 6 | 8 => {
                moves.extend(self.maybe_enter_room(token, current));
            }
            _ => {
                if token.1 == 0 {
                    moves.push(self.with((token.0, token.1 + 1), current));
                }
            }
        }

        for delta in [-1, 1] {
            let next = current as i32 + delta;
            if next < 0 {
                continue;
            }
            let next = next as usize;
            if next < HALLWAY_LEN && previous != next && self.hallway[next].is_none() {
                let mut more = new_state.get_hallway_moves(token, next, current);
                moves.append(&mut more);
            }
        }
        moves
    }

    fn check(&self) -> bool {
        self.rooms.iter().all(|r| r.check())
    }
}

impl<const SIZE: usize> std::cmp::PartialEq for State<SIZE> {
    fn eq(&self, other: &Self) -> bool {
        (self.hallway == other.hallway) && (self.rooms == other.rooms)
    }
}
impl<const SIZE: usize> std::cmp::Eq for State<SIZE> {}

impl<const SIZE: usize> std::hash::Hash for State<SIZE> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.hallway.hash(state);
        self.rooms.hash(state);
    }
}

impl<const SIZE: usize> Ord for State<SIZE> {
    fn cmp(&self, other: &Self) -> Ordering {
        other.energy.cmp(&self.energy)
    }
}

impl<const SIZE: usize> PartialOrd for State<SIZE> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<const SIZE: usize> std::fmt::Display for State<SIZE> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        fn to_char(c: Option<Token>) -> char {
            if let Some((c, _)) = c {
                match c {
                    1 => 'A',
                    10 => 'B',
                    100 => 'C',
                    1000 => 'D',
                    _ => '!',
                }
            } else {
                '.'
            }
        }

        let mut result = format!("energy: {}\n", self.energy);
        for c in self.hallway {
            result.push(to_char(c));
        }
        result.push('\n');
        for row in 0..SIZE {
            result.push_str("  ");
            for room in 0..4 {
                result.push(to_char(self.rooms[room].content[SIZE - row - 1]));
                result.push(' ');
            }
            result.push('\n');
        }
        write!(f, "{}", result)
    }
}

fn solve<const SIZE: usize>(state: &State<SIZE>) -> Option<usize> {
    let mut total_energy = HashMap::new();
    let mut stack = BinaryHeap::new();
    stack.push(*state);
    while let Some(state) = stack.pop() {
        if state.check() {
            return Some(state.energy);
        }

        let &prev_energy = total_energy.get(&state).unwrap_or(&usize::MAX);
        if state.energy > prev_energy {
            continue;
        }

        for next in state.get_all_moves() {
            let &prev_energy = total_energy.get(&next).unwrap_or(&usize::MAX);
            if next.energy < prev_energy {
                stack.push(next);
                total_energy.insert(next, next.energy);
            }
        }
    }
    None
}

impl Solver<&str> for Day23 {
    fn part1(data: &str) -> usize {
        let state = State::<2>::new(data);
        solve(&state).unwrap()
    }

    fn part2(data: &str) -> usize {
        let mut state = State::<4>::new(data);
        state.rooms[0].content[1] = Some((1000, 0));
        state.rooms[0].content[2] = Some((1000, 0));
        state.rooms[1].content[1] = Some((10, 0));
        state.rooms[1].content[2] = Some((100, 0));
        state.rooms[2].content[1] = Some((1, 0));
        state.rooms[2].content[2] = Some((10, 0));
        state.rooms[3].content[1] = Some((100, 0));
        state.rooms[3].content[2] = Some((1, 0));
        solve(&state).unwrap()
    }
}

// #D#C#B#A#
// #D#B#A#C#

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
