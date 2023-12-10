pub fn parse(data: &str) -> String {
  data.to_string()
}

fn win(t: i64, d: i64) -> i64 {
  let delta = ((t*t - 4 * d) as f64).sqrt() as i64;
  let vmax = (t + delta) / 2; 
  let vmin = (t - delta) / 2; 
  let a = vmax + (-1..=1)
    .find(|&dv| {
      let v0 = vmax + dv;
      let d0 = v0 * (t - v0);
      d0 <= d
    })
    .unwrap();
  let b = vmin - (-1..=1)
    .find(|&dv| {
      let v0 = vmin - dv;
      let d0 = v0 * (t - v0);
      d0 <= d
    })
    .unwrap();
  a - b - 1
}

pub fn part1(data: &str) -> i64 {
  let mut lines = data.lines().map(|line| {
    let (_, line) = line.split_once(':').unwrap();
    line
      .split_whitespace()
      .map(|n| n.parse::<i64>().unwrap())
  });
  let times = lines.next().unwrap();
  let distances = lines.next().unwrap();
  times
    // .iter()
    .zip(distances)
    .map(|(t, d)| win(t, d))
    .product()
}

pub fn part2(data: &str) -> i64 {
  let mut lines = data.lines().map(|line| {
    let (_, line) = line.split_once(':').unwrap();
    line.replace(' ', "").parse::<i64>().unwrap()
  });
  let t = lines.next().unwrap();
  let d = lines.next().unwrap();
  win(t, d)
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA: &str = r"Time:      7  15   30
Distance:  9  40  200
";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA);
    assert_eq!(part1(&data), 288);
    // assert_eq!(result, vec![0]);
  }

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA);
    assert_eq!(part2(&data), 71503);
  }
}
