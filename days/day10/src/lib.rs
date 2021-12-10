use anyhow::Result;
use aoc::solver::Solver;

pub struct Day10;

fn score_stack(stack: &[char]) -> usize {
    let mut score = 0;
    for c in stack.iter().rev() {
        score = 5 * score
            + match c {
                '(' => 1,
                '[' => 2,
                '{' => 3,
                _ => 4,
            };
    }
    score
}

fn find_corrupt(line: &str) -> (usize, usize) {
    let mut stack = Vec::new();
    for c in line.chars() {
        match c {
            '(' | '[' | '{' | '<' => stack.push(c),
            _ => {
                let open = stack.pop().unwrap();
                match c {
                    ')' if open != '(' => return (3, 0),
                    ']' if open != '[' => return (57, 0),
                    '}' if open != '{' => return (1197, 0),
                    '>' if open != '<' => return (25137, 0),
                    _ => {}
                }
            }
        }
    }
    (0, score_stack(&stack))
}

impl Solver<&str> for Day10 {
    fn part1(data: &str) -> Result<String> {
        let result: usize = data.lines().map(|l| find_corrupt(l).0).sum();
        Ok(result.to_string())
    }

    fn part2(data: &str) -> Result<String> {
        let mut result: Vec<_> = data
            .lines()
            .filter_map(|l| {
                let (ok, score) = find_corrupt(l);
                if ok == 0 {
                    Some(score)
                } else {
                    None
                }
            })
            .collect();
        result.sort_unstable();
        let result = result[result.len() / 2];

        Ok(result.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const DATA: &str = "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
";

    #[test]
    fn test_find_corrupt() {
        assert_eq!(find_corrupt("{([(<{}[<>[]}>{[]{[(<()>").0, 1197);
        assert_eq!(find_corrupt("[[<[([]))<([[{}[[()]]]").0, 3);
        assert_eq!(find_corrupt("[{[{({}]{}}([{[{{{}}([]").0, 57);
        assert_eq!(find_corrupt("[<(<(<(<{}))><([]([]()").0, 3);
        assert_eq!(find_corrupt("<{([([[(<>()){}]>(<<{{").0, 25137);
    }

    #[test]
    fn test_score_stack() {
        assert_eq!(score_stack(&['<', '{', '(', '[']), 294);
        assert_eq!(
            score_stack(&['<', '{', '[', '{', '[', '{', '{', '[', '[']),
            995444
        );
    }

    #[test]
    fn test_part1() {
        assert_eq!(Day10::part1(DATA).unwrap(), "26397");
    }

    #[test]
    fn test_part2() {
        assert_eq!(Day10::part2(DATA).unwrap(), "288957");
    }
}
