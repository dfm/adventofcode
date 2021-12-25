use aoc::solver::Solver;

pub struct Day25;

#[derive(Copy, Clone)]
enum Cell {
    Empty,
    East,
    South,
}

struct Grid {
    width: usize,
    height: usize,
    value: Vec<Cell>,
}

impl Grid {
    fn new(data: &str) -> Self {
        let width = data.lines().next().unwrap().trim().len();
        let mut grid = Grid {
            width,
            height: 0,
            value: Vec::new(),
        };
        for line in data.lines() {
            for c in line.trim().chars() {
                grid.value.push(match c {
                    '>' => Cell::East,
                    'v' => Cell::South,
                    _ => Cell::Empty,
                });
            }
        }
        grid.height = grid.value.len() / width;
        grid
    }

    fn east(&self, n: usize) -> usize {
        if n % self.width == self.width - 1 {
            n + 1 - self.width
        } else {
            n + 1
        }
    }

    fn south(&self, n: usize) -> usize {
        if n >= (self.height - 1) * self.width {
            n % self.width
        } else {
            n + self.width
        }
    }

    fn step(&self) -> (usize, Self) {
        let mut count = 0;
        let mut grid = Grid {
            width: self.width,
            height: self.height,
            value: Vec::new(),
        };
        grid.value.resize(self.width * self.height, Cell::Empty);

        for (n, v) in self.value.iter().enumerate() {
            if matches!(v, Cell::East) {
                let target = self.east(n);
                if matches!(self.value[target], Cell::Empty) {
                    grid.value[target] = Cell::East;
                    count += 1;
                } else {
                    grid.value[n] = Cell::East;
                }
            }
        }

        for (n, v) in self.value.iter().enumerate() {
            if matches!(v, Cell::South) {
                let target = self.south(n);
                if matches!(grid.value[target], Cell::Empty)
                    && !matches!(self.value[target], Cell::South)
                {
                    grid.value[target] = Cell::South;
                    count += 1;
                } else {
                    grid.value[n] = Cell::South;
                }
            }
        }

        (count, grid)
    }
}

impl std::fmt::Display for Cell {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Cell::Empty => '.',
                Cell::East => '>',
                Cell::South => 'v',
            }
        )
    }
}

impl std::fmt::Display for Grid {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut n = 0;
        let mut result = String::new();
        for _ in 0..self.height {
            for _ in 0..self.width {
                let line = format!("{}", self.value[n]);
                n += 1;
                result.push_str(&line);
            }
            result.push('\n');
        }
        write!(f, "{}", result)
    }
}

impl Solver<&str> for Day25 {
    fn part1(data: &str) -> usize {
        let mut count = 0;
        let mut grid = Grid::new(data);
        loop {
            let result = grid.step();
            count += 1;
            if result.0 == 0 {
                break;
            }
            grid = result.1;
        }
        count
    }

    fn part2(_data: &str) -> usize {
        0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const DATA: &str = "v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>
";

    #[test]
    fn test_part1() {
        assert_eq!(Day25::part1(DATA), 58);
    }

    #[test]
    fn test_part2() {
        assert_eq!(Day25::part2(DATA), 0);
    }
}
