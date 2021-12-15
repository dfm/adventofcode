use aoc::solver::Solver;
use std::collections::HashMap;

pub struct Day08;

type Hash = [u8; 6];
type Map = HashMap<Hash, usize>;
const BASELINE: &str = "abcefg cf acdeg acdfg bcdf abdfg abdefg acf abcdefg abcdfg";

fn get_mapper() -> Map {
    let counts = get_character_counts(BASELINE);
    get_hashes(&counts, BASELINE)
        .iter()
        .enumerate()
        .map(|(n, &h)| (h, n))
        .collect()
}

fn get_character_counts(data: &str) -> [usize; 7] {
    let n0 = 'a' as usize;
    let mut result = [0; 7];
    data.chars()
        .filter(|&c| c != ' ')
        .for_each(|c| result[c as usize - n0] += 1);
    result
}

fn get_hash(counts: &[usize; 7], data: &str) -> Hash {
    let mut result = [0; 6];
    for c in data.trim().chars() {
        result[counts[c as usize - 'a' as usize] - 4] += 1;
    }
    result
}

fn get_hashes(counts: &[usize; 7], data: &str) -> Vec<Hash> {
    data.split_whitespace()
        .map(|w| get_hash(counts, w))
        .collect()
}

fn solve_one(mapper: &Map, line: &str) -> usize {
    let mut parts = line.split(" | ");
    let counts = get_character_counts(parts.next().unwrap());
    let hashes = get_hashes(&counts, parts.next().unwrap());
    hashes
        .iter()
        .rev()
        .enumerate()
        .fold(0, |acc, (n, v)| acc + mapper[v] * usize::pow(10, n as u32))
}

impl Solver<&str> for Day08 {
    fn part1(data: &str) -> usize {
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
        result
    }

    fn part2(data: &str) -> usize {
        let mapper = get_mapper();
        data.lines().map(|l| solve_one(&mapper, l)).sum()
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
    fn test_solve_one() {
        let mapper = get_mapper();
        let data =
            "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf";
        assert_eq!(solve_one(&mapper, &data), 5353);
    }

    #[test]
    fn test_part1() {
        assert_eq!(Day08::part1(&TEST_DATA), 26);
    }

    #[test]
    fn test_part2() {
        assert_eq!(Day08::part2(&TEST_DATA), 61229);
    }
}
