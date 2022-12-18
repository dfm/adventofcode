#ifndef AOC_GRAPH_HPP
#define AOC_GRAPH_HPP

#include <cstddef>
#include <cstdint>
#include <iterator>
#include <optional>
#include <queue>
#include <unordered_map>
#include <utility>

namespace aoc {
namespace graph {

namespace detail {

template <typename V, typename D>
struct distance_cmp {
  constexpr bool operator()(const std::pair<V, D>& lhs,
                            const std::pair<V, D>& rhs) const {
    // This is backwards from usual - we want to sort such that the *shortest*
    // distance is highest priority
    return lhs.second > rhs.second;
  }
};

}  // namespace detail

template <typename DistanceMap, typename Node, typename Distance>
inline bool update_distance(DistanceMap&, const Node&, const Distance&);

template <typename Node, typename Distance>
inline bool update_distance(std::unordered_map<Node, Distance>& map,
                            const Node& node, const Distance& distance) {
  auto lookup = map.find(node);
  if (lookup != map.end()) {
    if (lookup->second > distance) {
      lookup->second = distance;
      return true;
    } else {
      return false;
    }
  }
  map.insert_or_assign(node, distance);
  return true;
}

template <typename Graph, typename Node = typename Graph::node_type,
          typename Distance = std::uint64_t,
          typename DistanceMap = std::unordered_map<Node, Distance>>
struct shortest_path {
  using Cursor = std::pair<Node, Distance>;

  Graph graph;
  DistanceMap distance_map;
  std::priority_queue<Cursor, std::vector<Cursor>,
                      detail::distance_cmp<Node, Distance>>
      queue;

  shortest_path(Graph& graph, const Node& start) : graph(graph) {
    update_distance(distance_map, start, Distance(0));
    queue.push({start, Distance(0)});
  }

  shortest_path(Graph&& graph, const Node& start) : graph(std::move(graph)) {
    update_distance(distance_map, start, Distance(0));
    queue.push({start, Distance(0)});
  }

  inline std::optional<Cursor> next() {
    if (queue.empty()) return {};
    auto current = queue.top();
    auto [node, distance] = current;
    for (auto [next_node, edge_distance] : graph.neighbors(node)) {
      auto next_distance = distance + edge_distance;
      if (update_distance(distance_map, next_node, next_distance)) {
        queue.push({next_node, next_distance});
      }
    }
    queue.pop();
    return current;
  }

  inline DistanceMap run() {
    while (next()) {
    }
    return distance_map;
  }

  inline std::optional<Distance> run(const Node& to) {
    while (true) {
      auto propose = next();
      if (!propose) break;
      auto [node, distance] = propose.value();
      if (node == to) return distance;
    }
    return {};
  }
};

}  // namespace graph
}  // namespace aoc

#endif
