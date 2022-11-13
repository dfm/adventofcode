use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashMap},
    hash::Hash,
};

#[derive(Debug, Copy, Clone)]
struct Node<N> {
    node: N,
    distance: usize,
}

impl<N> PartialEq for Node<N> {
    fn eq(&self, other: &Self) -> bool {
        self.distance.eq(&other.distance)
    }
}

impl<N> Eq for Node<N> {}

impl<N> Ord for Node<N> {
    fn cmp(&self, other: &Self) -> Ordering {
        other.distance.cmp(&self.distance)
    }
}

impl<N> PartialOrd for Node<N> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub trait Graph<N> {
    fn neighbors(&self, node: &N) -> Vec<(N, usize)>;
}

impl<N> Graph<N> for HashMap<N, Vec<(N, usize)>>
where
    N: Hash + Eq + Copy,
{
    fn neighbors(&self, node: &N) -> Vec<(N, usize)> {
        self.get(node).cloned().unwrap_or_default()
    }
}

impl Graph<usize> for Vec<Vec<(usize, usize)>> {
    fn neighbors(&self, node: &usize) -> Vec<(usize, usize)> {
        self[*node].clone()
    }
}

fn is_shorter<N: Hash + Eq>(distances: &HashMap<N, usize>, node: N, distance: usize) -> bool {
    distances.get(&node).map_or(true, |&d| distance <= d)
}

pub fn shortest_paths<G, N>(graph: &G, start: N, end: Option<N>) -> HashMap<N, usize>
where
    G: Graph<N>,
    N: Hash + Eq + Copy,
{
    let mut distances: HashMap<N, usize> = HashMap::new();
    distances.insert(start, 0);

    let mut queue = BinaryHeap::new();
    queue.push(Node {
        node: start,
        distance: 0,
    });

    while let Some(Node { node, distance }) = queue.pop() {
        if Some(node) == end {
            return distances;
        }
        if !is_shorter(&distances, node, distance) {
            continue;
        }
        for (next, delta) in graph.neighbors(&node) {
            let next = Node {
                node: next,
                distance: distance + delta,
            };
            if is_shorter(&distances, next.node, next.distance) {
                queue.push(next);
                distances.insert(next.node, next.distance);
            }
        }
    }
    distances
}

pub fn shortest_path<G, N>(graph: &G, start: N, end: N) -> Option<usize>
where
    G: Graph<N>,
    N: Hash + Eq + Copy,
{
    shortest_paths(graph, start, Some(end)).get(&end).copied()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shortest_paths_hash_map() {
        let mut graph = HashMap::new();
        graph.insert(0, vec![(1, 1), (2, 10)]);
        graph.insert(1, vec![(3, 2)]);
        graph.insert(2, vec![(1, 1), (3, 3), (4, 1)]);
        graph.insert(3, vec![(4, 2), (0, 7)]);

        let result = shortest_paths(&graph, 0, None);
        assert_eq!(result.get(&1).cloned(), Some(1));
        assert_eq!(result.get(&3).cloned(), Some(3));
        assert_eq!(result.get(&4).cloned(), Some(5));

        let result = shortest_paths(&graph, 3, None);
        assert_eq!(result.get(&0).cloned(), Some(7));

        let result = shortest_paths(&graph, 4, None);
        assert_eq!(result.get(&0).cloned(), None);
    }

    #[test]
    fn test_shortest_path_vec() {
        struct Edge {
            node: usize,
            cost: usize,
        }

        impl Graph<usize> for Vec<Vec<Edge>> {
            fn neighbors(&self, node: &usize) -> Vec<(usize, usize)> {
                self[*node].iter().map(|e| (e.node, e.cost)).collect()
            }
        }

        let graph = vec![
            // Node 0
            vec![Edge { node: 2, cost: 10 }, Edge { node: 1, cost: 1 }],
            // Node 1
            vec![Edge { node: 3, cost: 2 }],
            // Node 2
            vec![
                Edge { node: 1, cost: 1 },
                Edge { node: 3, cost: 3 },
                Edge { node: 4, cost: 1 },
            ],
            // Node 3
            vec![Edge { node: 0, cost: 7 }, Edge { node: 4, cost: 2 }],
            // Node 4
            vec![],
        ];

        assert_eq!(shortest_path(&graph, 0, 1), Some(1));
        assert_eq!(shortest_path(&graph, 0, 3), Some(3));
        assert_eq!(shortest_path(&graph, 3, 0), Some(7));
        assert_eq!(shortest_path(&graph, 0, 4), Some(5));
        assert_eq!(shortest_path(&graph, 4, 0), None);
    }
}
