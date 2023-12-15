pub fn parse(data: &str) -> String {
  data.chars().filter(|c| !c.is_whitespace()).collect()
}

pub fn part1(data: &str) -> usize {
  data.split(',').map(comp_hash).sum()
}

pub fn part2(data: &str) -> usize {
  let mut boxes: Vec<Vec<(String, usize)>> = vec![Default::default(); 256];
  for step in data.split(',') {
    let (label, foc) = step.split_once(|c| matches!(c, '=' | '-')).unwrap();
    let id = comp_hash(label);
    let b = boxes.get_mut(id).unwrap();
    if foc.is_empty() {
      b.retain(|(k, _)| k != label);
    } else {
      let foc = foc.parse().unwrap();
      if let Some(r) = b.iter_mut().find(|(k, _)| k == label) {
        r.1 = foc;
      } else {
        b.push((label.to_string(), foc));
      }
    }
  }
  boxes
    .iter()
    .enumerate()
    .map(|(n, b)| {
      (n + 1)
        * b
          .iter()
          .enumerate()
          .map(|(m, (_, f))| f * (1 + m))
          .sum::<usize>()
    })
    .sum()
}

fn comp_hash(s: &str) -> usize {
  s.chars().fold(0, |v, c| (17 * (v + c as usize)) % 256)
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA: &str = r"rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA);
    assert_eq!(part1(&data), 1320);
  }

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA);
    assert_eq!(part2(&data), 145);
  }
}
