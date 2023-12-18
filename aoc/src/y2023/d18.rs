pub fn parse(data: &str) -> String {
  data.to_string()
}

pub fn part1(data: &str) -> i64 {
  solve(data.lines().map(|line| {
    let mut parts = line.split_whitespace();
    let (dx, dy) = match parts.next().unwrap() {
      "R" => (1, 0),
      "L" => (-1, 0),
      "U" => (0, 1),
      "D" => (0, -1),
      _ => unreachable!(),
    };
    let dist = parts.next().unwrap().parse::<i64>().unwrap();
    (dx, dy, dist)
  }))
}

pub fn part2(data: &str) -> i64 {
  solve(data.lines().map(|line| {
    let mut parts = line.split_whitespace().skip(2);
    let hex = parts.next().unwrap();
    let dist = i64::from_str_radix(&hex[2..7], 16).unwrap();
    let (dx, dy) = match &hex[7..8] {
      "0" => (1, 0),
      "1" => (0, -1),
      "2" => (-1, 0),
      "3" => (0, 1),
      _ => unreachable!(),
    };
    (dx, dy, dist)
  }))
}

fn solve(parts: impl Iterator<Item = (i64, i64, i64)>) -> i64 {
  let area = parts
    .fold((0, 0, 0), |(x, y, c), (dx, dy, d)| {
      (x + d * dx, y + d * dy, c + (2 * y + d * dy) * d * dx + d)
    })
    .2;
  area / 2 + 1
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA: &str = r"R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA);
    assert_eq!(part1(&data), 62);
  }

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA);
    assert_eq!(part2(&data), 952408144115);
  }
}
