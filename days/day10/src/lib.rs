use anyhow::{Error, Result};
use aoc::solver::Solver;

pub struct Day10;

#[derive(Debug, Clone, PartialEq)]
pub enum Parse {
    Syntax(usize),
    Incomplete(usize),
}

impl std::str::FromStr for Parse {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(parse(s))
    }
}

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

fn parse(line: &str) -> Parse {
    let mut stack = Vec::new();
    for c in line.chars() {
        match c {
            '(' | '[' | '{' | '<' => stack.push(c),
            _ => {
                let open = stack.pop().unwrap();
                match c {
                    ')' if open != '(' => return Parse::Syntax(3),
                    ']' if open != '[' => return Parse::Syntax(57),
                    '}' if open != '{' => return Parse::Syntax(1197),
                    '>' if open != '<' => return Parse::Syntax(25137),
                    _ => {}
                }
            }
        }
    }
    Parse::Incomplete(score_stack(&stack))
}

impl Solver<Vec<Parse>> for Day10 {
    fn part1(data: Vec<Parse>) -> Result<String> {
        let result: usize = data
            .iter()
            .filter_map(|v| match v {
                Parse::Syntax(v) => Some(v),
                _ => None,
            })
            .sum();
        Ok(result.to_string())
    }

    fn part2(data: Vec<Parse>) -> Result<String> {
        let mut result: Vec<_> = data
            .iter()
            .filter_map(|v| match v {
                Parse::Incomplete(v) => Some(v),
                _ => None,
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

    fn get_data() -> Vec<Parse> {
        let handler = &aoc::InputHandler::new(&DATA);
        handler.into()
    }

    #[test]
    fn test_parse() {
        assert_eq!(parse("{([(<{}[<>[]}>{[]{[(<()>"), Parse::Syntax(1197));
        assert_eq!(parse("[[<[([]))<([[{}[[()]]]"), Parse::Syntax(3));
        assert_eq!(parse("[{[{({}]{}}([{[{{{}}([]"), Parse::Syntax(57));
        assert_eq!(parse("[<(<(<(<{}))><([]([]()"), Parse::Syntax(3));
        assert_eq!(parse("<{([([[(<>()){}]>(<<{{"), Parse::Syntax(25137));
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
        assert_eq!(Day10::part1(get_data()).unwrap(), "26397");
    }

    #[test]
    fn test_part2() {
        assert_eq!(Day10::part2(get_data()).unwrap(), "288957");
    }
}
