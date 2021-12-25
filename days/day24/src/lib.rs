use aoc::solver::Solver;

pub struct Day24;

type Int = i64;

fn get_params_for_output(a: Int, c: Int, expect: Int) -> Option<Vec<(Int, Int)>> {
    if a <= 0 {
        return Some((1..=9).map(|w| (w, 26 * expect + w - a)).collect());
    } else if a >= 10 {
        for w in 1..=9 {
            let delta = expect - w - c;
            if delta % 26 == 0 {
                let z = delta / 26;
                return Some(vec![(w, z)]);
            }
        }
    }
    None
}

fn reverse_engineer(params: &[(Int, Int)], target: Int) -> Vec<Vec<(Int, Int)>> {
    let mut results = Vec::new();
    if let Some((&(a, c), rest)) = params.split_first() {
        if let Some(next_target) = get_params_for_output(a, c, target) {
            if rest.is_empty() {
                return vec![next_target];
            }
            for t in next_target {
                for next in reverse_engineer(rest, t.1) {
                    let mut next = next;
                    next.push(t);
                    results.push(next);
                }
            }
        }
    }
    results
}

fn solve(data: &str) -> (Int, Int) {
    let a = data
        .lines()
        .rev()
        .skip(12)
        .step_by(18)
        .map(|l| l.split_whitespace().last().unwrap().parse().unwrap());
    let c = data
        .lines()
        .rev()
        .skip(2)
        .step_by(18)
        .map(|l| l.split_whitespace().last().unwrap().parse().unwrap());
    let params: Vec<(Int, Int)> = a.zip(c).collect();
    let results = reverse_engineer(&params, 0);
    results
        .iter()
        .filter(|v| v.len() == 14 && v[0].1 == 0)
        .map(|v| v.iter().fold(0, |acc, (x, _)| acc * 10 + x))
        .fold((Int::MAX, Int::MIN), |(mn, mx), n| (mn.min(n), mx.max(n)))
}

impl Solver<&str, Int> for Day24 {
    fn part1(data: &str) -> Int {
        solve(data).1
    }

    fn part2(data: &str) -> Int {
        solve(data).0
    }
}
