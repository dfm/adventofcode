use std::collections::HashMap;

pub type Rule = (Option<(usize, bool, usize)>, String);
pub type Part = [usize; 4];
pub type Data = (HashMap<String, Vec<Rule>>, Vec<Part>);

fn to_idx(s: &str) -> usize {
  match s {
    "x" => 0,
    "m" => 1,
    "a" => 2,
    "s" => 3,
    _ => unreachable!(),
  }
}

pub fn parse(data: &str) -> Data {
  let mut rules = HashMap::new();
  let mut lines = data.lines();
  loop {
    let line = lines.next().unwrap();
    if line.is_empty() {
      break;
    }

    let (name, rest) = line.split_once('{').unwrap();
    rules.insert(
      name.to_string(),
      rest[..rest.len() - 1]
        .split(',')
        .map(|p| {
          if let Some((from, to)) = p.split_once(':') {
            if let Some((a, b)) = from.split_once('<') {
              (Some((to_idx(a), true, b.parse().unwrap())), to.to_string())
            } else {
              let (a, b) = from.split_once('>').unwrap();
              (Some((to_idx(a), false, b.parse().unwrap())), to.to_string())
            }
          } else {
            (None, p.to_string())
          }
        })
        .collect(),
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
  (rules, parts)
}

fn apply_rule<'a>(rule: &'a Rule, part: &mut Part) -> Option<&'a str> {
  if let Some((idx, flag, val)) = rule.0 {
    if (flag && part[idx] < val) || (!flag && part[idx] > val) {
      Some(&rule.1)
    } else {
      None
    }
  } else {
    Some(&rule.1)
  }
}

pub fn part1(data: &Data) -> usize {
  data
    .1
    .iter()
    .filter_map(|part| {
      let mut part = *part;
      let mut workflow = "in".to_string();
      while let Some(rules) = data.0.get(&workflow) {
        for rule in rules {
          if let Some(next) = apply_rule(rule, &mut part) {
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
  let mut invert: HashMap<String, Vec<(&str, usize)>> = HashMap::new();
  for (name, workflow) in data.0.iter() {
    for (n, rule) in workflow.iter().enumerate() {
      invert
        .entry(rule.1.to_string())
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
    let rules = data.0.get(name).unwrap();
    if let Some((n, flag, v)) = rules[idx].0 {
      if flag {
        ranges[n].1 = ranges[n].1.min(v);
      } else {
        ranges[n].0 = ranges[n].0.max(v);
      }
    }

    for (r, _) in rules.iter().take(idx) {
      let (n, flag, v) = r.unwrap();
      if flag {
        ranges[n].0 = ranges[n].0.max(v - 1);
      } else {
        ranges[n].1 = ranges[n].1.min(v + 1);
      }
    }
    if name == "in" {
      results.push(ranges);
    } else {
      queue.extend(invert.get(name).unwrap().iter().map(|&v| (v, ranges)));
    }
  }
  results.iter().map(|r| r.iter().map(|(a, b)| b - a - 1).product::<usize>()).sum()
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
