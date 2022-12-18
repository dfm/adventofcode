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

template <typename Value, typename Container>
struct Iterator {
  using iterator_category = std::forward_iterator_tag;
  using value_type = Value;
  using pointer = Value*;
  using reference = Value&;

  inline void advance() { current = container->next(); }

  Iterator() {}
  Iterator(Container* container) : container(container) { advance(); }
  reference operator*() { return current; }
  pointer operator->() { return &current; }
  Iterator& operator++() {
    advance();
    return *this;
  }
  Iterator operator++(int) {
    Iterator tmp = *this;
    advance();
    return tmp;
  }
  friend bool operator==(const Iterator& a, const Iterator& b) {
    return a.current == b.current;
  };
  friend bool operator!=(const Iterator& a, const Iterator& b) {
    return a.current != b.current;
  };

 private:
  Value current;
  Container* container;
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
  using Iterator =
      detail::Iterator<std::optional<Cursor>,
                       shortest_path<Graph, Node, Distance, DistanceMap>>;

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

  Iterator begin() { return Iterator(this); }
  Iterator end() { return Iterator(); }

  inline DistanceMap run() {
    while (next()) {
    }
    return distance_map;
  }

  inline std::optional<Distance> run(const Node& to) {
    for (auto it = begin(); it != end(); it++) {
      auto [node, distance] = it->value();
      if (node == to) return distance;
    }
    return {};
  }
};

}  // namespace graph
}  // namespace aoc

#endif
