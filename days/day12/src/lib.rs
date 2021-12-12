use anyhow::Result;
use aoc::solver::Solver;
use std::collections::HashMap;

pub struct Day12;

const START: &str = "start";
const END: &str = "end";
type Graph = HashMap<String, Vec<String>>;

fn can_visit(visited: &HashMap<String, usize>, current: String, max_visit: usize) -> bool {
    if current == START {
        return false;
    }
    let flag = visited.values().any(|&v| v >= max_visit);
    *visited.get(&current).unwrap_or(&0) < if flag { 1 } else { max_visit }
}

fn parse(data: &str) -> Graph {
    let mut graph = Graph::new();
    for line in data.lines() {
        let mut parts = line.split("-");
        let start = parts.next().unwrap();
        let end = parts.next().unwrap();
        let target = graph.entry(start.to_string()).or_insert(Vec::new());
        target.push(end.to_string());
        let target = graph.entry(end.to_string()).or_insert(Vec::new());
        target.push(start.to_string());
    }
    graph
}

fn find_paths(
    graph: &Graph,
    visited: &HashMap<String, usize>,
    current: String,
    max_visit: usize,
) -> Vec<Vec<String>> {
    if current == END {
        return vec![vec![END.to_string()]];
    }
    let mut paths = Vec::new();
    let mut visited = visited.clone();
    if current.to_lowercase() == current {
        let target = visited.entry(current.to_string()).or_insert(0);
        *target += 1;
    }
    for neighbor in graph.get(&current.clone()).unwrap() {
        if !can_visit(&visited, neighbor.clone(), max_visit) {
            continue;
        }
        for subpath in find_paths(graph, &visited, neighbor.clone(), max_visit).iter_mut() {
            subpath.insert(0, current.clone());
            paths.push(subpath.clone());
        }
    }
    paths
}

fn find_all_paths(graph: &Graph, max_visit: usize) -> Vec<Vec<String>> {
    let visited = HashMap::new();
    find_paths(graph, &visited, START.to_string(), max_visit)
        .into_iter()
        .filter(|p| *p.last().unwrap() == END)
        .collect()
}

impl Solver<&str> for Day12 {
    fn part1(data: &str) -> Result<String> {
        let graph = parse(data);
        let paths = find_all_paths(&graph, 1);
        Ok(paths.len().to_string())
    }

    fn part2(data: &str) -> Result<String> {
        let graph = parse(data);
        let paths = find_all_paths(&graph, 2);
        Ok(paths.len().to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const DATA1: &str = "start-A
start-b
A-c
A-b
b-d
A-end
b-end
";
    const DATA2: &str = "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc
";

    const DATA3: &str = "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW
";

    #[test]
    fn test_part1() {
        assert_eq!(Day12::part1(DATA1).unwrap(), "10");
        assert_eq!(Day12::part1(DATA2).unwrap(), "19");
        assert_eq!(Day12::part1(DATA3).unwrap(), "226");
    }

    #[test]
    fn test_part2() {
        assert_eq!(Day12::part2(DATA1).unwrap(), "36");
        assert_eq!(Day12::part2(DATA2).unwrap(), "103");
        assert_eq!(Day12::part2(DATA3).unwrap(), "3509");
    }
}
