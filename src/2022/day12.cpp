#include <limits>
#include <queue>
#include <unordered_map>

#include "aoc/aoc.hpp"

namespace {

typedef std::uint64_t distance_t;
typedef std::pair<size_t, size_t> node_t;
typedef std::pair<node_t, distance_t> neighbor_t;

struct node_hash {
  size_t operator()(const node_t& v) const {
    size_t h = 0;
    aoc::hash_combine(h, v.first, v.second);
    return h;
  }
};

struct grid {
  size_t _width = 0;
  size_t _start = 0;
  size_t _end = 0;
  std::vector<int> _data;
  inline size_t width() const { return _width; }
  inline size_t height() const { return _data.size() / _width; }
  inline node_t start() const { return {_start % _width, _start / _width}; }
  inline node_t end() const { return {_end % _width, _end / _width}; }
  inline int value(size_t x, size_t y) const {
    if (x >= width() || y >= height()) return 100;
    return _data[_width * y + x];
  }

  std::vector<neighbor_t> neighbors(const node_t& current) const {
    std::vector<neighbor_t> neighbors;
    auto x0 = current.first;
    auto y0 = current.second;
    auto z0 = value(x0, y0);
    std::vector<std::pair<int, int>> deltas = {{x0, y0 + 1}, {x0 + 1, y0}};
    if (x0 > 0) deltas.push_back({x0 - 1, y0});
    if (y0 > 0) deltas.push_back({x0, y0 - 1});
    for (const auto& d : deltas) {
      if (value(d.first, d.second) <= z0 + 1) {
        neighbors.push_back(
            std::make_pair(std::make_pair(d.first, d.second), 1));
      }
    }
    return neighbors;
  };
};

template <typename V, typename D>
struct distance_cmp {
  constexpr bool operator()(const std::pair<V, D>& lhs,
                            const std::pair<V, D>& rhs) const {
    return lhs.second > rhs.second;  // Backwards
  }
};

template <typename Map, typename Node, typename Distance = std::uint64_t>
inline bool is_shorter(const Map& map, const Node& node,
                       const Distance& distance) {
  if (auto lookup = map.find(node); lookup != map.end()) {
    return distance < (*lookup).second;
  }
  return true;
}

template <typename Graph, typename Node, typename Distance = std::uint64_t,
          typename Hash = std::hash<Node>>
Distance shortest_path(const Graph& graph, const Node& start, const Node& end) {
  using S = std::pair<Node, Distance>;

  std::unordered_map<Node, Distance, Hash> distances;
  distances.insert({start, Distance(0)});

  std::priority_queue<S, std::vector<S>, distance_cmp<Node, Distance>> queue;
  queue.push({start, Distance(0)});

  for (; !queue.empty(); queue.pop()) {
    const auto& next = queue.top();
    const auto& node = next.first;
    const auto& distance = next.second;
    if (node == end) {
      return distance;
    }
    for (const auto& neighbor : graph.neighbors(node)) {
      auto propose = std::make_pair(neighbor.first, neighbor.second + distance);
      if (is_shorter(distances, propose.first, propose.second)) {
        queue.push(propose);
        distances.insert_or_assign(propose.first, propose.second);
      }
    }
  }
  return 0;
}

namespace grammar {

namespace dsl = lexy::dsl;

struct parser {
  static constexpr auto rule =
      dsl::identifier(dsl::ascii::alpha / dsl::ascii::newline);
  static constexpr auto value = lexy::callback<grid>([](auto lex) {
    grid g;
    size_t count = 0;
    for (const auto& c : lex) {
      if (c == '\n' || c == '\r') {
        if (g._width == 0) g._width = count;
        continue;
      } else if (c == 'S') {
        g._start = count;
        g._data.push_back(0);
      } else if (c == 'E') {
        g._end = count;
        g._data.push_back(static_cast<int>('z' - 'a'));
      } else {
        g._data.push_back(static_cast<int>(c - 'a'));
      }
      count++;
    }
    return g;
  });
};

}  // namespace grammar

AOC_IMPL(2022, 12) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto data) {
    auto start = data.start();
    auto end = data.end();
    return shortest_path<grid, node_t, distance_t, node_hash>(data, start, end);
  };
  static constexpr auto part2 = [](auto data) {
    distance_t result = std::numeric_limits<distance_t>::max();
    auto end = data.end();
    for (size_t y = 0; y < data.height(); ++y) {
      for (size_t x = 0; x < data.width(); ++x) {
        if (data.value(x, y) != 0) continue;
        auto len = shortest_path<grid, node_t, distance_t, node_hash>(
            data, {x, y}, end);
        if (len == 0) continue;
        result = std::min(result, len);
      }
    }
    return result;
  };
};

AOC_TEST_CASE(31, 29, R"(Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
)")

}  // namespace
