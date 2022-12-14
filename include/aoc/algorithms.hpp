#ifndef AOC_ALGORITHMS_HPP
#define AOC_ALGORITHMS_HPP

#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <queue>
#include <utility>

namespace aoc {

// MAGIC: from boost
// ref:
// https://stackoverflow.com/questions/2590677/how-do-i-combine-hash-values-in-c0x
inline void hash_combine(size_t&) {}
template <typename T, typename... Rest>
inline void hash_combine(size_t& seed, const T& v, Rest... rest) {
  std::hash<T> hasher;
  seed ^= hasher(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
  hash_combine(seed, rest...);
}

struct pair_hash {
  template <typename A, typename B>
  size_t operator()(const std::pair<A, B>& v) const {
    size_t h = 0;
    hash_combine(h, v.first, v.second);
    return h;
  }
};

template <typename I>
class range {
 public:
  class iterator {
    friend class range;

   public:
    long int operator*() const { return i_; }
    const iterator& operator++() {
      i_ += delta_;
      return *this;
    }
    iterator operator++(int) {
      iterator copy(*this);
      i_ += delta_;
      return copy;
    }

    bool operator==(const iterator& other) const { return i_ == other.i_; }
    bool operator!=(const iterator& other) const { return i_ != other.i_; }

   protected:
    iterator(I start, I delta) : i_(start), delta_(delta) {}

   private:
    I i_, delta_;
  };

  iterator begin() const { return begin_; }
  iterator end() const { return end_; }
  range(I begin, I end) : begin_(begin, I(1)), end_(end, I(1)) {}
  range(I begin, I end, I delta) : begin_(begin, delta), end_(end, delta) {}

 private:
  iterator begin_;
  iterator end_;
};

namespace detail {

template <typename V, typename D>
struct distance_cmp {
  constexpr bool operator()(const std::pair<V, D>& lhs,
                            const std::pair<V, D>& rhs) const {
    return lhs.second > rhs.second;  // Backwards
  }
};

template <typename Map, typename Node, typename Distance>
inline bool is_shorter(const Map& map, const Node& node,
                       const Distance& distance) {
  auto lookup = map.get(node);
  if (lookup) {
    return distance < lookup.value();
  }
  return true;
}

template <typename Graph, bool early_stop = false>
auto shortest_path(const Graph& graph, const typename Graph::node_type& start,
                   const std::optional<typename Graph::node_type>& end) {
  using Node = typename Graph::node_type;
  using Distance = typename Graph::distance_type;
  using S = std::pair<Node, Distance>;

  auto distance_map = graph.to_distance_map();
  distance_map.set(start, Distance(0));

  std::priority_queue<S, std::vector<S>, distance_cmp<Node, Distance>> queue;
  queue.push({start, Distance(0)});

  for (; !queue.empty(); queue.pop()) {
    const auto& next = queue.top();
    const auto& node = next.first;
    const auto& distance = next.second;
    if constexpr (early_stop) {
      if (node == end.value()) break;
    }
    for (const auto& neighbor : graph.neighbors(node)) {
      auto propose = std::make_pair(neighbor.first, neighbor.second + distance);
      if (is_shorter(distance_map, propose.first, propose.second)) {
        queue.push(propose);
        distance_map.set(propose.first, propose.second);
      }
    }
  }
  return distance_map;
}

}  // namespace detail

template <typename Graph>
std::optional<typename Graph::distance_type> shortest_path(
    const Graph& graph, const typename Graph::node_type& start,
    const typename Graph::node_type& end) {
  auto result = detail::shortest_path<Graph, true>(graph, start, end);
  return result.get(end);
}

template <typename Graph>
auto shortest_path(const Graph& graph, const typename Graph::node_type& start) {
  return detail::shortest_path<Graph, false>(graph, start, {});
}

}  // namespace aoc

#endif
