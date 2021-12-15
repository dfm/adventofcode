use aoc::solver::Solver;

pub struct Day11;

const SIZE: usize = 10;

#[derive(Debug)]
struct Grid {
    energy: [u8; SIZE * SIZE],
    exploded: [bool; SIZE * SIZE],
}

impl Grid {
    fn new(data: &str) -> Self {
        let mut energy = [0; SIZE * SIZE];
        let mn = b'0';
        let mx = b'9';
        let data = data
            .chars()
            .filter_map(|c| {
                let c = c as u8;
                if c >= mn && c <= mx {
                    Some(c - mn)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        energy[..SIZE * SIZE].clone_from_slice(&data[..SIZE * SIZE]);
        Self {
            energy,
            exploded: [false; SIZE * SIZE],
        }
    }

    fn to_explode(&self) -> bool {
        self.energy
            .iter()
            .zip(self.exploded.iter())
            .any(|(&e, &f)| !f && e > 9)
    }

    fn do_explode(&mut self) {
        for x in 0..SIZE {
            for y in 0..SIZE {
                let n = self.idx(x, y);
                if !self.exploded[n] && self.energy[n] > 9 {
                    self.exploded[n] = true;
                    self.incr(x, y, -1, -1);
                    self.incr(x, y, -1, 0);
                    self.incr(x, y, -1, 1);
                    self.incr(x, y, 0, -1);
                    self.incr(x, y, 0, 1);
                    self.incr(x, y, 1, -1);
                    self.incr(x, y, 1, 0);
                    self.incr(x, y, 1, 1);
                }
            }
        }
    }

    fn idx(&self, x: usize, y: usize) -> usize {
        x * SIZE + y
    }

    fn step1(&mut self) {
        self.energy.iter_mut().for_each(|e| *e += 1);
        self.exploded.iter_mut().for_each(|f| *f = false);
    }

    fn incr(&mut self, x: usize, y: usize, dx: i32, dy: i32) {
        let x = x as i32 + dx;
        let y = y as i32 + dy;
        if x >= 0 && x < SIZE as i32 && y >= 0 && y < SIZE as i32 {
            self.energy[self.idx(x as usize, y as usize)] += 1;
        }
    }

    fn step2(&mut self) {
        self.do_explode();
        while self.to_explode() {
            self.do_explode();
        }
    }

    fn step3(&mut self) -> usize {
        self.energy
            .iter_mut()
            .zip(self.exploded.iter())
            .filter_map(|(e, &f)| {
                if f {
                    *e = 0;
                    Some(0)
                } else {
                    None
                }
            })
            .count()
    }

    fn step(&mut self) -> usize {
        self.step1();
        self.step2();
        self.step3()
    }
}

impl std::fmt::Display for Grid {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut result = String::new();
        for row in 0..SIZE {
            for col in 0..SIZE {
                let line = format!("{}", self.energy[self.idx(row, col)]);
                result.push_str(&line);
            }
            result.push('\n');
        }
        write!(f, "{}", result)
    }
}

impl Solver<&str> for Day11 {
    fn part1(data: &str) -> usize {
        let mut grid = Grid::new(data);
        (0..100).map(|_| grid.step()).sum()
    }

    fn part2(data: &str) -> usize {
        let mut grid = Grid::new(data);
        (1..).find(|_| grid.step() == SIZE * SIZE).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const DATA: &str = "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
";

    #[test]
    fn test_part1() {
        assert_eq!(Day11::part1(DATA), 1656);
    }

    #[test]
    fn test_part2() {
        assert_eq!(Day11::part2(DATA), 195);
    }
}
