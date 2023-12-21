use crate::helpers::lcm;
use std::collections::{HashMap, VecDeque};

pub type Data = HashMap<String, Mod>;

#[derive(Debug, Clone)]
pub enum Type {
  Flip(bool),
  Conj(HashMap<String, bool>),
  Broad,
}

#[derive(Debug, Clone)]
pub struct Mod {
  typ: Type,
  targets: Vec<String>,
}

impl Mod {
  fn signal(&mut self, from: &str, signal: bool) -> Vec<(String, bool)> {
    let signal = match &mut self.typ {
      Type::Broad => Some(signal),
      Type::Flip(val) if !signal => {
        *val = !*val;
        Some(!*val)
      }
      Type::Conj(val) => {
        *val.get_mut(from).unwrap() = signal;
        Some(!val.iter().all(|(_, v)| *v))
      }
      _ => None,
    };
    if let Some(signal) = signal {
      self
        .targets
        .iter()
        .map(|s| (s.to_string(), signal))
        .collect()
    } else {
      Vec::new()
    }
  }
}

pub fn parse(data: &str) -> Data {
  let mut data = data
    .lines()
    .map(|line| {
      let (mut name, targets) = line.split_once(" -> ").unwrap();
      let typ = if name.starts_with('&') {
        name = &name[1..];
        Type::Conj(HashMap::new())
      } else if name.starts_with('%') {
        name = &name[1..];
        Type::Flip(true)
      } else {
        Type::Broad
      };
      (
        name.to_string(),
        Mod {
          typ,
          targets: targets.split(", ").map(|s| s.to_string()).collect(),
        },
      )
    })
    .collect::<HashMap<_, _>>();

  let mut parents: HashMap<String, Vec<String>> = HashMap::new();
  for (n, m) in data.iter() {
    for t in m.targets.iter() {
      parents
        .entry(t.to_string())
        .and_modify(|v| v.push(n.to_string()))
        .or_insert_with(|| vec![n.to_string()]);
    }
  }

  for (n, m) in data.iter_mut() {
    if let Type::Conj(v) = &mut m.typ {
      for p in parents.get(n).unwrap() {
        v.insert(p.to_string(), false);
      }
    }
  }

  data
}

pub fn part1(data: &Data) -> usize {
  let mut state = data.clone();
  let mut low = 0;
  let mut high = 0;
  for _ in 0..1000 {
    let mut queue: VecDeque<_> =
      vec![("button".to_string(), "broadcaster".to_string(), false)].into();
    while let Some((from, to, signal)) = queue.pop_front() {
      if signal {
        high += 1;
      } else {
        low += 1;
      }
      if let Some(m) = state.get_mut(&to) {
        queue.extend(
          m.signal(&from, signal)
            .iter()
            .map(|(a, b)| (to.to_string(), a.to_string(), *b)),
        );
      }
    }
  }
  low * high
}

pub fn part2(data: &Data) -> usize {
  let mut result = 1;
  for root in data.get("broadcaster").unwrap().targets.iter() {
    let mut next = root.clone();
    let mut acc = 0;
    for depth in 0.. {
      let targets = &data.get(&next).unwrap().targets;
      if targets
        .iter()
        .any(|t| matches!(data.get(t).unwrap().typ, Type::Conj(_)))
      {
        acc += 1 << depth;
      }
      if let Some(nxt) = targets.iter().find_map(|t| {
        if let Type::Flip(_) = data.get(t).unwrap().typ {
          Some(t.to_string())
        } else {
          None
        }
      }) {
        next = nxt;
      } else {
        break;
      }
    }
    result = lcm(result, acc);
  }
  result
}

#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA1: &str = r"broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a
";

  #[test]
  fn test_part1a() {
    let data = parse(TEST_DATA1);
    assert_eq!(part1(&data), 32000000);
  }

  const TEST_DATA2: &str = r"broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output
";

  #[test]
  fn test_part1b() {
    let data = parse(TEST_DATA2);
    assert_eq!(part1(&data), 11687500);
  }
}
