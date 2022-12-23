#include <limits>

#include "aoc/aoc.hpp"

namespace {
struct grid {
  size_t _width = 0;
  size_t _start = 0;
  size_t _end = 0;
  std::vector<int> _data;

  typedef std::uint64_t distance_type;
  typedef std::pair<size_t, size_t> node_type;
  typedef std::pair<node_type, distance_type> neighbor_type;

  inline size_t width() const { return _width; }
  inline size_t height() const { return _data.size() / _width; }
  inline node_type start() const { return {_start % _width, _start / _width}; }
  inline node_type end() const { return {_end % _width, _end / _width}; }
  inline int value(size_t x, size_t y) const {
    if (x >= width() || y >= height()) return -2;
    return _data[_width * y + x];
  }

  // distance_map<node_type, distance_type> to_distance_map() const {
  //   return {_width, std::vector<std::optional<distance_type>>(_data.size())};
  // }

  std::vector<neighbor_type> neighbors(const node_type& current) const {
    std::vector<neighbor_type> neighbors;
    auto x0 = current.first;
    auto y0 = current.second;
    auto z0 = value(x0, y0);
    std::vector<std::pair<int, int>> deltas = {{x0, y0 + 1}, {x0 + 1, y0}};
    if (x0 > 0) deltas.push_back({x0 - 1, y0});
    if (y0 > 0) deltas.push_back({x0, y0 - 1});
    for (const auto& d : deltas) {
      if (value(d.first, d.second) >= z0 - 1) {
        neighbors.push_back({{d.first, d.second}, 1});
      }
    }
    return neighbors;
  };
};

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
    return aoc::graph::shortest_path(data, end).run(start).value();
  };
  static constexpr auto part2 = [](auto data) {
    auto result = std::numeric_limits<grid::distance_type>::max();
    auto end = data.end();
    auto distances = aoc::graph::shortest_path(data, end).run();
    for (size_t y = 0; y < data.height(); ++y) {
      for (size_t x = 0; x < data.width(); ++x) {
        if (data.value(x, y) != 0) continue;
        auto value = distances[{x, y}];
        if (value > 0) {
          result = std::min(result, value);
        }
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
