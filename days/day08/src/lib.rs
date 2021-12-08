use anyhow::Result;
use aoc::solver::Solver;
use std::collections::HashSet;

pub struct Day08;

fn to_digit(data: &str) -> u8 {
    let n0 = 'a' as usize;
    let mut digit = 0;
    for c in data.trim().chars() {
        digit |= 1 << (c as usize - n0);
    }
    digit
}

fn character_counts(data: &str) -> [u8; 7] {
    let n0 = 'a' as usize;
    let mut result = [0; 7];
    data.chars()
        .filter(|&c| c != ' ')
        .for_each(|c| result[c as usize - n0] += 1);
    result
}

fn find_with_counts(counts: &[u8; 7], target: u8) -> u8 {
    let index = counts.iter().position(|&v| v == target).unwrap();
    1 << index
}

fn find_with_len(parts: &mut HashSet<u8>, target: u32) -> u8 {
    let result = parts
        .iter()
        .find(|t| t.count_ones() == target)
        .unwrap()
        .clone();
    parts.remove(&result);
    result
}

fn solve_one(line: &str) -> usize {
    let mut parts = line.split(" | ");
    let input_str = parts.next().unwrap();
    let input_counts = character_counts(&input_str);
    let mut input = input_str.split_whitespace().map(to_digit).collect();
    let mut key = [0; 10];

    // We can work out some edges based on character counts
    let bl = find_with_counts(&input_counts, 4);
    let tl = find_with_counts(&input_counts, 6);
    let br = find_with_counts(&input_counts, 9);

    // Some digits have a unique number of edges
    key[1] = find_with_len(&mut input, 2);
    key[4] = find_with_len(&mut input, 4);
    key[7] = find_with_len(&mut input, 3);
    key[8] = find_with_len(&mut input, 7);

    // Work out the rest
    key[9] = key[8] & !bl;
    key[3] = key[9] & !tl;
    key[2] = key[8] & !br & !tl;
    input.remove(&key[9]);
    input.remove(&key[3]);
    input.remove(&key[2]);
    key[5] = find_with_len(&mut input, 5);
    let tr = key[9] & !key[5];
    key[6] = key[8] & !tr;
    input.remove(&key[6]);
    key[0] = *input.iter().next().unwrap();

    parts
        .next()
        .unwrap()
        .split_whitespace()
        .rev()
        .enumerate()
        .fold(0, |acc, (n, v)| {
            let target = to_digit(v);
            acc + key.iter().position(|&k| k == target).unwrap() * usize::pow(10, n as u32)
        })
}

impl Solver<&str> for Day08 {
    fn part1(data: &str) -> Result<String> {
        let result: usize = data
            .lines()
            .map(|l| {
                let mut parts = l.split(" | ");
                parts.next().unwrap();
                parts
                    .next()
                    .unwrap()
                    .split_whitespace()
                    .filter(|p| {
                        let n = p.trim().len();
                        n == 2 || n == 3 || n == 4 || n == 7
                    })
                    .count()
            })
            .sum();
        Ok(result.to_string())
    }

    fn part2(data: &str) -> Result<String> {
        let result: usize = data.lines().map(solve_one).sum();
        Ok(result.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str =
        "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
";

    #[test]
    fn test_to_digit() {
        assert_eq!(to_digit("abdg"), 75);
    }

    #[test]
    fn test_solve_one() {
        let data =
            "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf";
        assert_eq!(solve_one(&data), 5353);
    }

    #[test]
    fn test_part1() {
        assert_eq!(Day08::part1(&TEST_DATA).unwrap(), "26");
    }

    #[test]
    fn test_part2() {
        assert_eq!(Day08::part2(&TEST_DATA).unwrap(), "61229");
    }
}
