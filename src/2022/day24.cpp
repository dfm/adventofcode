#include <array>
#include <iostream>
#include <range/v3/all.hpp>
#include <unordered_set>

#include "aoc/aoc.hpp"

namespace rv = ranges::views;

namespace {

using int_t = std::int64_t;
enum direction_t { up = 0, right, down, left };
using blizzard_t = std::tuple<int_t, int_t, direction_t>;

const std::array<std::pair<int_t, int_t>, 5> neighbors_to_try{
    std::make_pair(0, -1), std::make_pair(-1, 0), std::make_pair(0, 0),
    std::make_pair(1, 0), std::make_pair(0, 1)};

struct grid {
  int_t _x = 0;
  int_t _y = 0;
  int_t _width = 0;
  int_t _height = 0;
  std::vector<blizzard_t> _data;
  std::vector<std::unordered_set<std::pair<int_t, int_t>>> _cache;

  using distance_type = int_t;
  using node_type = std::tuple<size_t, int_t, int_t>;
  using neighbor_type = std::pair<node_type, distance_type>;

  void push_back(const char& c) {
    if (c == '^') {
      _data.push_back(std::make_tuple(_x, _y, direction_t::up));
    } else if (c == '>') {
      _data.push_back(std::make_tuple(_x, _y, direction_t::right));
    } else if (c == 'v') {
      _data.push_back(std::make_tuple(_x, _y, direction_t::down));
    } else if (c == '<') {
      _data.push_back(std::make_tuple(_x, _y, direction_t::left));
    }
    if (c == '\n') {
      if (_width == 0) {
        _width = _x;
      }
      _y++;
      _x = 0;
    } else {
      _x++;
    }
    _height = _y;
  }

  inline int_t period() const { return std::lcm(_width - 2, _height - 2); }
  std::pair<int_t, int_t> start() const { return {1, 0}; }
  std::pair<int_t, int_t> finish() const { return {_width - 2, _height - 1}; }

  inline blizzard_t move_blizzard(const blizzard_t& b) const {
    auto [x, y, d] = b;
    if (d == direction_t::up)
      y--;
    else if (d == direction_t::right)
      x++;
    else if (d == direction_t::down)
      y++;
    else
      x--;
    auto dx = _width - 2;
    auto dy = _height - 2;
    x = ((x - 1) % dx + dx) % dx + 1;
    y = ((y - 1) % dy + dy) % dy + 1;
    return std::make_tuple(x, y, d);
  }

  void update_cache() {
    std::unordered_set<std::pair<int_t, int_t>> open;
    open.insert(start());
    open.insert(finish());
    for (int_t y = 1; y < _height - 1; ++y) {
      for (int_t x = 1; x < _width - 1; ++x) {
        if (!ranges::any_of(_data, [&](const auto& b) {
              return x == std::get<0>(b) && y == std::get<1>(b);
            }))
          open.insert({x, y});
      }
    }
    _cache.push_back(open);
  }

  void step_blizzards() {
    for (auto& b : _data) {
      b = move_blizzard(b);
    }
    update_cache();
  }

  std::unordered_set<std::pair<int_t, int_t>> const& get_open(size_t time) {
    // time = time % static_cast<size_t>(period());
    if (_cache.size() == 0) {
      update_cache();
    }
    while (time >= _cache.size()) {
      step_blizzards();
    }
    return _cache[time];
  }

  std::vector<neighbor_type> neighbors(const node_type& current) {
    const auto& [time, x, y] = current;
    auto next_time = (time + 1) % static_cast<size_t>(period());
    const auto& open = get_open(next_time);
    std::vector<neighbor_type> results;
    for (const auto& [dx, dy] : neighbors_to_try) {
      auto xp = x + dx;
      auto yp = y + dy;
      if (xp <= 0 || xp >= _width - 1) continue;
      if (yp <= 0 || yp >= _height - 1) {
        if ((yp != 0 || xp != 1) && (yp != _height - 1 || xp != _width - 2))
          continue;
      }
      if (open.contains({xp, yp})) {
        results.push_back({{next_time, xp, yp}, 1});
      }
    }
    return results;
  }

  [[maybe_unused]] void show() const {
    for (int_t y = 0; y < _height; ++y) {
      for (int_t x = 0; x < _width; ++x) {
        if (ranges::any_of(_data, [&](const auto& b) {
              return x == std::get<0>(b) && y == std::get<1>(b);
            })) {
          std::cout << "*";
        } else {
          std::cout << ".";
        }
      }
      std::cout << std::endl;
    }
  }
};

namespace grammar {

namespace dsl = lexy::dsl;

struct character {
  static constexpr auto rule =
      dsl::capture(dsl::ascii::lower / dsl::ascii::punct / dsl::ascii::newline);
  static constexpr auto value = lexy::callback<char>(
      [](auto lex) { return static_cast<char>(*lex.begin()); });
};

struct parser {
  static constexpr auto rule =
      dsl::terminator(dsl::eof).opt_list(dsl::p<character>);
  static constexpr auto value = lexy::as_list<grid>;
};

}  // namespace grammar

size_t find_path(grid& g, size_t time, const std::pair<int_t, int_t>& from,
                 const std::pair<int_t, int_t>& to) {
  auto path = aoc::graph::shortest_path(g, {time, from.first, from.second});
  for (const auto& node : path) {
    if (!node) {
      throw std::runtime_error("Failed to find path");
    }
    const auto& [t, x, y] = node.value().first;
    if (x == to.first && y == to.second) return node.value().second;
  }
  return 0;
}

AOC_IMPL(2022, 24) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto grid) -> size_t {
    return find_path(grid, 0, grid.start(), grid.finish());
  };
  static constexpr auto part2 = [](auto grid) {
    auto time = find_path(grid, 0, grid.start(), grid.finish());
    time += find_path(grid, time, grid.finish(), grid.start());
    return time + find_path(grid, time, grid.start(), grid.finish());
    ;
  };
};

AOC_TEST_CASE(18, 54, R"(#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#
)")

}  // namespace
