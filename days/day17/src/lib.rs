use aoc::solver::Solver;
use regex::Regex;

pub struct Day17;

#[derive(Debug)]
struct Target {
    xmin: i32,
    xmax: i32,
    ymin: i32,
    ymax: i32,
}

struct State {
    x: i32,
    y: i32,
    vx: i32,
    vy: i32,
}

impl State {
    fn new(vx: i32, vy: i32) -> Self {
        Self { x: 0, y: 0, vx, vy }
    }

    fn step(&mut self) {
        self.x += self.vx;
        self.y += self.vy;
        self.vx -= self.vx.signum();
        self.vy -= 1;
    }
}

impl Target {
    fn new(data: &str) -> Self {
        let re = Regex::new("^target area: x=(?P<x1>-?[0-9]+)..(?P<x2>-?[0-9]+), y=(?P<y1>-?[0-9]+)..(?P<y2>-?[0-9]+)$").unwrap();
        let c = re.captures(data.trim()).unwrap();
        let x1 = c.name("x1").unwrap().as_str().parse().unwrap();
        let x2 = c.name("x2").unwrap().as_str().parse().unwrap();
        let y1 = c.name("y1").unwrap().as_str().parse().unwrap();
        let y2 = c.name("y2").unwrap().as_str().parse().unwrap();
        Self {
            xmin: std::cmp::min(x1, x2),
            xmax: std::cmp::max(x1, x2),
            ymin: std::cmp::min(y1, y2),
            ymax: std::cmp::max(y1, y2),
        }
    }

    fn max_height(&self) -> i32 {
        let vy = -self.ymin - 1;
        vy * vy - vy * (vy - 1) / 2
    }

    fn all(&self) -> usize {
        let vx_min = ((0.25 + 2.0 * (self.xmin as f32)).sqrt() - 0.5).ceil() as i32;
        let vx_max = self.xmax;
        let vy_min = self.ymin;
        let vy_max = -self.ymin - 1;
        let mut count = 0;
        for vx in vx_min..=vx_max {
            for vy in vy_min..=vy_max {
                if self.integrate(vx, vy).is_some() {
                    count += 1;
                }
            }
        }
        count
    }

    fn hits(&self, x: i32, y: i32) -> bool {
        (self.xmin <= x) && (x <= self.xmax) && (self.ymin <= y) && (y <= self.ymax)
    }

    fn integrate(&self, vx: i32, vy: i32) -> Option<i32> {
        let mut max = 0;
        let mut state = State::new(vx, vy);
        while (state.x <= self.xmax) && (state.y >= self.ymin) {
            state.step();
            max = std::cmp::max(max, state.y);
            if self.hits(state.x, state.y) {
                return Some(max);
            }
        }
        None
    }
}

impl Solver<&str, i32, usize> for Day17 {
    fn part1(data: &str) -> i32 {
        let target = Target::new(data);
        target.max_height()
    }

    fn part2(data: &str) -> usize {
        let target = Target::new(data);
        target.all()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const DATA: &str = "target area: x=20..30, y=-10..-5";

    #[test]
    fn test_integrate() {
        let target = Target::new(DATA);
        assert_eq!(target.integrate(6, 3), Some(6));
        assert_eq!(target.integrate(9, 0), Some(0));
        assert_eq!(target.integrate(17, -4), None);
        assert_eq!(target.integrate(6, 9), Some(45));
    }

    #[test]
    fn test_part1() {
        assert_eq!(Day17::part1(DATA), 45);
    }

    #[test]
    fn test_part2() {
        assert_eq!(Day17::part2(DATA), 112);
    }
}
