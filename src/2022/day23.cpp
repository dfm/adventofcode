#include <array>
#include <iostream>
#include <range/v3/all.hpp>
#include <unordered_map>
#include <unordered_set>

#include "aoc/aoc.hpp"

namespace rv = ranges::views;

namespace {

using int_t = std::int64_t;
using coord_t = std::pair<int_t, int_t>;

const std::array<coord_t, 8> perimeter = {
    coord_t{-1, -1}, coord_t{0, -1}, coord_t{1, -1}, coord_t{-1, 0},
    coord_t{1, 0},   coord_t{-1, 1}, coord_t{0, 1},  coord_t{1, 1}};
const std::array<coord_t, 4> directions = {coord_t{0, -1}, coord_t{0, 1},
                                           coord_t{-1, 0}, coord_t{1, 0}};

struct grid {
  int_t _x = 0;
  int_t _y = 0;
  std::unordered_set<coord_t> _elves;

  void push_back(const char& c) {
    if (c == '#') {
      _elves.insert({_x, _y});
    }
    if (c == '\n') {
      _y++;
      _x = 0;
    } else {
      _x++;
    }
  }

  inline bool check(const coord_t& c) const { return _elves.contains(c); }

  coord_t propose(size_t round, const coord_t& from) const {
    if (!ranges::any_of(perimeter, [&](const coord_t& delta) {
          return check({from.first + delta.first, from.second + delta.second});
        }))
      return from;

    auto [x, y] = from;
    for (size_t d = 0; d < 4; ++d) {
      auto [dx, dy] = directions[(round + d) % 4];
      // std::cout << "dx = " << dx << "; dy = " << dy << std::endl;
      if (dx == 0 && !(check({x - 1, y + dy}) || check({x, y + dy}) ||
                       check({x + 1, y + dy}))) {
        return {x, y + dy};
      } else if (dy == 0 && !(check({x + dx, y - 1}) || check({x + dx, y}) ||
                              check({x + dx, y + 1}))) {
        return {x + dx, y};
      }
    }
    return {x, y};
  }

  size_t step(size_t round) {
    std::unordered_map<coord_t, size_t> counter;
    std::unordered_map<coord_t, coord_t> proposals;
    for (const auto& c : _elves) {
      auto p = propose(round % 4, c);
      proposals.insert_or_assign(c, p);
      auto q = counter.find(p);
      if (q == counter.end()) {
        counter.insert_or_assign(p, 1);
      } else {
        q->second++;
      }
    }

    size_t count = 0;
    _elves.clear();
    for (const auto& [c, p] : proposals) {
      if (counter[p] == 1) {
        _elves.insert(p);
        if (p != c) {
          count++;
        }
      } else {
        _elves.insert(c);
      }
    }

    return count;
  }

  size_t evaluate() const {
    size_t result = 0;
    auto to_x = rv::transform([](const auto& v) { return v.first; });
    auto to_y = rv::transform([](const auto& v) { return v.second; });
    auto [x_min, x_max] = ranges::minmax(_elves | to_x);
    auto [y_min, y_max] = ranges::minmax(_elves | to_y);
    for (int_t y = y_min; y <= y_max; ++y) {
      for (int_t x = x_min; x <= x_max; ++x) {
        if (!check({x, y})) {
          result++;
        }
      }
    }
    return result;
  }

  [[maybe_unused]] void show() const {
    auto to_x = rv::transform([](const auto& v) { return v.first; });
    auto to_y = rv::transform([](const auto& v) { return v.second; });
    auto [x_min, x_max] = ranges::minmax(_elves | to_x);
    auto [y_min, y_max] = ranges::minmax(_elves | to_y);
    for (int_t y = y_min; y <= y_max; ++y) {
      for (int_t x = x_min; x <= x_max; ++x) {
        if (check({x, y})) {
          std::cout << "#";
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
      dsl::capture(dsl::ascii::punct / dsl::ascii::newline);
  static constexpr auto value = lexy::callback<char>(
      [](auto lex) { return static_cast<char>(*lex.begin()); });
};

struct parser {
  static constexpr auto rule =
      dsl::terminator(dsl::eof).opt_list(dsl::p<character>);
  static constexpr auto value = lexy::as_list<grid>;
};

}  // namespace grammar

AOC_IMPL(2022, 23) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto grid) {
    for (auto round : rv::iota(0, 10)) {
      grid.step(round);
    }
    return grid.evaluate();
  };
  static constexpr auto part2 = [](auto grid) {
    size_t round = 0;
    while (grid.step(round++) != 0) {
    }
    return round;
  };
};

AOC_TEST_CASE(110, 20, R"(....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..
)")

}  // namespace
