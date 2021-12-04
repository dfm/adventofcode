use anyhow::Result;
use aoc::solver::Solver;

pub struct Day04;
type Int = u32;
const SIZE: usize = 5;

impl Solver<&str> for Day04 {
    fn part1(data: &str) -> Result<String> {
        let (draws, mut cards) = parse(data);
        for &n in draws.iter() {
            for card in cards.iter_mut() {
                card.mark(n);
                if card.check() {
                    return Ok((card.score() * n).to_string());
                }
            }
        }
        Ok("Something went wrong".to_string())
    }

    fn part2(data: &str) -> Result<String> {
        let (draws, mut cards) = parse(data);
        for &n in draws.iter() {
            let mut remove = vec![];
            for card in cards.iter_mut() {
                card.mark(n);
                remove.push(card.check());
            }
            if cards.len() == 1 && cards[0].check() {
                return Ok((cards[0].score() * n).to_string());
            }
            for (ind, &rm) in remove.iter().enumerate().rev() {
                if rm {
                    cards.remove(ind);
                }
            }
        }
        Ok("Something went wrong".to_string())
    }
}

fn parse(data: &str) -> (Vec<Int>, Vec<Card>) {
    let mut lines = data.lines();
    let draws = lines
        .next()
        .unwrap()
        .trim()
        .split(",")
        .map(|n| n.trim().parse().unwrap())
        .collect();

    let mut cards = vec![];
    while lines.next().is_some() {
        if let Some(block) = (0..SIZE).map(|_| lines.next()).collect::<Option<Vec<_>>>() {
            let block = block.join("\n");
            cards.push(Card::new(&block));
        }
    }
    (draws, cards)
}

#[derive(Clone, Debug)]
struct Card {
    numbers: [Int; SIZE * SIZE],
    mask: [bool; SIZE * SIZE],
}

impl std::fmt::Display for Card {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut result = String::new();
        for row in 0..SIZE {
            for col in 0..SIZE {
                let line = format!(" {:2} ", self.numbers[row * SIZE + col]);
                result.push_str(&line);
            }
            result.push('\n');
        }
        write!(f, "{}", result)
    }
}

impl Card {
    fn new(data: &str) -> Self {
        let parts = data.split_whitespace();
        let parts = parts.map(|p| p.trim().parse().unwrap()).collect::<Vec<_>>();
        let mut numbers = [0; SIZE * SIZE];
        for n in 0..SIZE * SIZE {
            numbers[n] = parts[n];
        }
        Card {
            numbers: numbers,
            mask: [false; SIZE * SIZE],
        }
    }

    fn mark(&mut self, number: Int) {
        for (&n, m) in self.numbers.iter().zip(self.mask.iter_mut()) {
            if n == number {
                *m = true;
            }
        }
    }

    fn row(&self, n: usize) -> impl Iterator<Item = (&bool, &Int)> {
        self.mask
            .iter()
            .zip(self.numbers.iter())
            .skip(n * SIZE)
            .take(SIZE)
    }

    fn col(&self, n: usize) -> impl Iterator<Item = (&bool, &Int)> {
        self.mask
            .iter()
            .zip(self.numbers.iter())
            .skip(n)
            .step_by(SIZE)
            .take(SIZE)
    }

    fn score(&self) -> Int {
        self.mask
            .iter()
            .zip(self.numbers.iter())
            .filter(|(&m, _)| !m)
            .fold(0, |a, (_, &x)| a + x)
    }

    fn check(&self) -> bool {
        for n in 0..SIZE {
            if self.row(n).find(|(&m, _)| !m).is_none() {
                return true;
            }
            if self.col(n).find(|(&m, _)| !m).is_none() {
                return true;
            }
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
        ";

    #[test]
    fn test_card() {
        let data = "22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19
";
        let mut card = Card::new(data);
        assert_eq!(card.numbers[0], 22);
        assert_eq!(card.numbers[7], 23);
        assert_eq!(card.mask[7], false);
        card.mark(23);
        assert_eq!(card.mask[7], true);

        assert!(!card.check());
        card.mark(24);
        card.mark(8);
        card.mark(2);
        card.mark(4);
        assert!(card.check());
        assert_eq!(card.score(), card.numbers.iter().sum::<Int>() - 61);
    }

    #[test]
    fn test_parse() {
        let (draws, cards) = parse(&TEST_DATA);
        assert_eq!(draws.len(), 27);
        assert_eq!(cards.len(), 3);
    }

    #[test]
    fn test_part1() {
        let result = Day04::part1(&TEST_DATA).unwrap();
        assert_eq!(result, "4512");
    }

    #[test]
    fn test_part2() {
        let result = Day04::part2(&TEST_DATA).unwrap();
        assert_eq!(result, "1924");
    }
}
