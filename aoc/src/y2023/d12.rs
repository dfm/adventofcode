use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Data {
  data: Vec<u8>,
  groups: Vec<usize>,
}

pub fn parse(data: &str) -> Vec<Data> {
  data
    .lines()
    .map(|line| {
      let (a, b) = line.split_once(' ').unwrap();
      let groups = b.split(',').map(|s| s.parse().unwrap()).collect();
      Data {
        data: a.as_bytes().to_vec(),
        groups,
      }
    })
    .collect()
}

pub fn part1(data: &[Data]) -> usize {
  data
    .iter()
    .map(|d| {
      let mut cache = HashMap::new();
      solve(&d.data, None, &d.groups, &mut cache)
    })
    .sum()
}

pub fn part2(data: &[Data]) -> usize {
  data
    .iter()
    .map(|d| {
      let mut groups = d.groups.clone();
      let mut data = d.data.clone();
      for _ in 1..5 {
        groups.append(&mut d.groups.clone());
        data.push(b'?');
        data.append(&mut d.data.clone());
      }
      let mut cache = HashMap::new();
      solve(&data, None, &groups, &mut cache)
    })
    .sum()
}

fn solve(
  data: &[u8],
  current: Option<usize>,
  groups: &[usize],
  cache: &mut HashMap<(usize, Option<usize>, usize), usize>,
) -> usize {
  let key = (data.len(), current, groups.len());

  if let Some(v) = cache.get(&key) {
    return *v;
  }

  let v = if data.is_empty() {
    // Finished
    match (current, groups) {
      (Some(n), &[m]) => (n == m) as usize,
      (None, &[]) => 1,
      _ => 0,
    }
  } else {
    match (data[0], current, groups) {
      // No group
      (b'.', None, _) => solve(&data[1..], None, groups, cache),
      // New group
      (b'#', None, [_, ..]) => solve(&data[1..], Some(1), groups, cache),
      // Accumulate group
      (b'#', Some(n), _) => solve(&data[1..], Some(n + 1), groups, cache),
      // Accumulate group on question mark
      (b'?', Some(n), &[g, ..]) if n < g => solve(&data[1..], Some(n + 1), groups, cache),
      // End group
      (b'.' | b'?', Some(n), &[g, ..]) if g == n => solve(&data[1..], None, &groups[1..], cache),
      // Branch
      (b'?', None, _) => {
        solve(&data[1..], Some(1), groups, cache) + solve(&data[1..], None, groups, cache)
      }
      _ => 0,
    }
  };

  cache.insert(key, v);
  v
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA: &str = r"???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA);
    assert_eq!(part1(&data), 21);
  }

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA);
    assert_eq!(part2(&data), 525152);
  }
}
