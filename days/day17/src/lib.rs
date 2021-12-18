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

    fn get_max_height(&self) -> i32 {
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
                if self.check_vel(vx, vy) {
                    count += 1;
                }
            }
        }
        count
    }

    fn solve_for_time(&self, vy: i32, y: i32) -> Option<i32> {
        let vy = vy as f32;
        let y = y as f32;
        let b = 2.0 * vy + 1.0;
        let arg = b * b - 8.0 * y;
        if arg < 0.0 {
            None
        } else {
            Some((0.5 * (b + arg.sqrt())) as i32)
        }
    }

    fn check_vel(&self, vx: i32, vy: i32) -> bool {
        if let Some(tmin) = self.solve_for_time(vy, self.ymax) {
            if let Some(tmax) = self.solve_for_time(vy, self.ymin) {
                for t in tmin..=tmax {
                    let tx = std::cmp::min(vx, t);
                    let x = vx * tx - vx.signum() * tx * (tx - 1) / 2;
                    let y = vy * t - t * (t - 1) / 2;
                    if self.check_pos(x, y) {
                        return true;
                    }
                }
            }
        }
        false
    }

    fn check_pos(&self, x: i32, y: i32) -> bool {
        (self.xmin <= x) && (x <= self.xmax) && (self.ymin <= y) && (y <= self.ymax)
    }
}

impl Solver<&str, i32, usize> for Day17 {
    fn part1(data: &str) -> i32 {
        let target = Target::new(data);
        target.get_max_height()
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
        assert_eq!(target.check_vel(6, 3), true);
        assert_eq!(target.check_vel(9, 0), true);
        assert_eq!(target.check_vel(17, -4), false);
        assert_eq!(target.check_vel(6, 9), true);
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
