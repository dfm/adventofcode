#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct Card([usize; 5], usize);

impl Card {
  fn new(v: Vec<usize>) -> Self {
    let c = v.try_into().unwrap();
    Card(c, compute_score(&c))
  }

  fn jokers(&mut self) {
    for n in 0..self.0.len() {
      if self.0[n] == 11 {
        self.0[n] = 0;
      }
    }
    self.1 = compute_score(&self.0);
  }
}

impl Ord for Card {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    let s1 = self.1;
    let s2 = other.1;
    if s1 != s2 {
      return s1.cmp(&s2);
    }
    for (a, b) in self.0.iter().zip(other.0.iter()) {
      if a != b {
        return a.cmp(b);
      }
    }
    std::cmp::Ordering::Equal
  }
}

impl PartialOrd for Card {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

fn compute_score(card: &[usize; 5]) -> usize {
  let mut c = [0usize; 15];
  card.iter().for_each(|n| c[*n] += 1);

  let nj = c[0];
  c[0] = 0;

  let (mx, _) = c.into_iter().enumerate().max_by_key(|(_, n)| *n).unwrap();
  c[mx] += nj;

  let mut d = [0usize; 5];
  c.into_iter().skip(1).for_each(|n| {
    if n > 0 {
      d[n - 1] += 1
    }
  });
  match d {
    [0, 0, 0, 0, 1] => 6,
    [1, 0, 0, 1, 0] => 5,
    [0, 1, 1, 0, 0] => 4,
    [2, 0, 1, 0, 0] => 3,
    [1, 2, 0, 0, 0] => 2,
    [3, 1, 0, 0, 0] => 1,
    [5, 0, 0, 0, 0] => 0,
    _ => panic!("Invalid hand {:?}", d),
  }
}

pub fn parse(data: &str) -> Vec<(Card, usize)> {
  data
    .lines()
    .map(|line| {
      let (card, bid) = line.split_once(' ').unwrap();
      let card = Card::new(
        card
          .chars()
          .map(|c| {
            if c.is_ascii_digit() {
              c as usize - '0' as usize
            } else {
              match c {
                'T' => 10,
                'J' => 11,
                'Q' => 12,
                'K' => 13,
                'A' => 14,
                _ => panic!("Invalid card {}", c),
              }
            }
          })
          .collect::<Vec<_>>(),
      );
      (card, bid.parse().unwrap())
    })
    .collect()
}

fn score(mut stack: Vec<(Card, usize)>) -> usize {
  stack.sort_by_key(|(c, _)| *c);
  stack
    .iter()
    .enumerate()
    .map(|(n, (_, b))| (n + 1) * b)
    .sum()
}

pub fn part1(data: &[(Card, usize)]) -> usize {
  let stack: Vec<(Card, usize)> = data.to_vec();
  score(stack)
}

pub fn part2(data: &[(Card, usize)]) -> usize {
  let mut stack: Vec<(Card, usize)> = data.to_vec();
  stack.iter_mut().for_each(|x| x.0.jokers());
  score(stack)
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA: &str = r"32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA);
    assert_eq!(part1(&data), 6440);
  }

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA);
    assert_eq!(part2(&data), 5905);
  }
}
