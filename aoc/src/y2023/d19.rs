use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Cond {
  Lt(usize, usize, String),
  Gt(usize, usize, String),
  No(String),
}

pub type Data = (HashMap<String, Vec<Cond>>, Vec<[usize; 4]>);

pub fn parse(data: &str) -> Data {
  let mut workflows = HashMap::new();
  let mut lines = data.lines();
  loop {
    let line = lines.next().unwrap();
    if line.is_empty() {
      break;
    }
    let (name, rest) = line.split_once('{').unwrap();
    workflows.insert(
      name.to_string(),
      rest[..rest.len() - 1].split(',').map(Cond::from).collect(),
    );
  }
  let parts = lines
    .map(|line| {
      line[1..line.len() - 1]
        .split(',')
        .map(|p| p[2..].parse::<usize>().unwrap())
        .collect::<Vec<_>>()
        .try_into()
        .unwrap()
    })
    .collect();
  (workflows, parts)
}

pub fn part1(data: &Data) -> usize {
  let (workflows, parts) = data;
  parts
    .iter()
    .filter_map(|part| {
      let mut part = *part;
      let mut workflow = "in".to_string();
      while let Some(conds) = workflows.get(&workflow) {
        for cond in conds {
          if let Some(next) = cond.apply(&mut part) {
            match next {
              "R" => return None,
              "A" => return Some(part.iter().sum::<usize>()),
              _ => {
                workflow = next.to_string();
                break;
              }
            }
          }
        }
      }
      None
    })
    .sum()
}

pub fn part2(data: &Data) -> usize {
  let workflows = &data.0;
  let mut invert: HashMap<&str, Vec<(&str, usize)>> = HashMap::new();
  for (name, workflow) in workflows.iter() {
    for (n, cond) in workflow.iter().enumerate() {
      invert
        .entry(cond.name())
        .and_modify(|v| v.push((name, n)))
        .or_insert(vec![(name, n)]);
    }
  }

  let mut results = Vec::new();
  let mut queue: Vec<_> = invert
    .get("A")
    .unwrap()
    .iter()
    .map(|&v| (v, [(0, 4001); 4]))
    .collect();
  while let Some(((name, idx), mut ranges)) = queue.pop() {
    let conds = workflows.get(name).unwrap();
    conds[idx].apply_ranges(&mut ranges);
    for cond in conds.iter().take(idx){
      cond.invert().apply_ranges(&mut ranges);
    }
    if name == "in" {
      results.push(ranges);
    } else {
      queue.extend(invert.get(name).unwrap().iter().map(|&v| (v, ranges)));
    }
  }
  results
    .iter()
    .map(|r| r.iter().map(|(a, b)| b - a - 1).product::<usize>())
    .sum()
}

impl Cond {
  fn name(&self) -> &str {
    match self {
      Cond::Lt(_, _, name) | Cond::Gt(_, _, name) | Cond::No(name) => name
    }
  }

  fn invert(&self) -> Self {
    match self.clone() {
      Cond::Lt(idx, val, name) => Cond::Gt(idx, val - 1, name),
      Cond::Gt(idx, val, name) => Cond::Lt(idx, val + 1, name),
      Cond::No(name) => Cond::No(name),
    }
  }

  fn apply<'a>(&'a self, part: &mut [usize; 4]) -> Option<&'a str> {
    match self {
      Cond::Lt(idx, val, name) if part[*idx] < *val => Some(name),
      Cond::Gt(idx, val, name) if part[*idx] > *val => Some(name),
      Cond::No(name) => Some(name),
      _ => None,
    }
  }

  fn apply_ranges(&self, ranges: &mut [(usize, usize); 4]) {
    match self {
      Cond::Lt(idx, val, _) => ranges[*idx].1 = ranges[*idx].1.min(*val),
      Cond::Gt(idx, val, _) => ranges[*idx].0 = ranges[*idx].0.max(*val),
      _ => {}
    }
  }
}

impl From<&str> for Cond {
  fn from(value: &str) -> Self {
    if let Some((from, to)) = value.split_once(':') {
      if let Some((a, b)) = from.split_once('<') {
        Cond::Lt(to_idx(a), b.parse().unwrap(), to.to_string())
      } else {
        let (a, b) = from.split_once('>').unwrap();
        Cond::Gt(to_idx(a), b.parse().unwrap(), to.to_string())
      }
    } else {
      Cond::No(value.to_string())
    }
  }
}

fn to_idx(s: &str) -> usize {
  match s {
    "x" => 0,
    "m" => 1,
    "a" => 2,
    "s" => 3,
    _ => unreachable!(),
  }
}


#[cfg(test)]
mod tests {
  use super::*;

  const TEST_DATA: &str = r"px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
";

  #[test]
  fn test_part1() {
    let data = parse(TEST_DATA);
    println!("{:?}", data);
    assert_eq!(part1(&data), 19114);
  }

  #[test]
  fn test_part2() {
    let data = parse(TEST_DATA);
    assert_eq!(part2(&data), 167409079868000);
  }
}
