#include "aoc/aoc.hpp"

namespace {

struct grid {
  size_t _width = 0;
  std::vector<int> _data;

  inline int width() const { return static_cast<int>(_width); }
  inline int height() const { return static_cast<int>(_data.size() / _width); }

  inline int value(int x, int y) const {
    if (x < 0 || x >= width()) return 0;
    if (y < 0 || y >= height()) return 0;
    auto idx = y * width() + x;
    return _data[idx];
  }

  template <typename I, typename F>
  inline std::pair<bool, size_t> search(I rng, F func) const {
    size_t count = 0;
    for (const auto &i : rng) {
      count++;
      if (func(i)) {
        return {false, count};
      }
    }
    return {true, count};
  }

  inline std::pair<bool, size_t> operator()(int x, int y) const {
    using rng = aoc::range<int>;
    auto h = value(x, y);
    auto fx = [&](int i) { return value(i, y) >= h; };
    auto fy = [&](int i) { return value(x, i) >= h; };
    auto left = search(rng(x - 1, -1, -1), fx);
    auto right = search(rng(x + 1, width()), fx);
    auto up = search(rng(y - 1, -1, -1), fy);
    auto down = search(rng(y + 1, height()), fy);
    return {left.first | right.first | up.first | down.first,
            left.second * right.second * up.second * down.second};
  }
};

namespace grammar {

namespace dsl = lexy::dsl;

struct parser {
  static constexpr auto rule =
      dsl::identifier(dsl::ascii::digit / dsl::ascii::newline);
  static constexpr auto value = lexy::callback<grid>([](auto lex) {
    grid g;
    size_t count = 0;
    for (const auto &c : lex) {
      if (c == '\n' || c == '\r') {
        if (g._width == 0) g._width = count;
      } else {
        g._data.push_back(static_cast<int>(c - '0'));
      }
      count++;
    }
    return g;
  });
};

}  // namespace grammar

AOC_IMPL(2022, 8) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto grid) {
    size_t count = 0;
    for (int y = 0; y < grid.height(); ++y) {
      for (int x = 0; x < grid.width(); ++x) {
        if (grid(x, y).first) count++;
      }
    }
    return count;
  };
  static constexpr auto part2 = [](auto grid) {
    size_t score = 0;
    for (int y = 0; y < grid.height(); ++y) {
      for (int x = 0; x < grid.width(); ++x) {
        score = std::max(score, grid(x, y).second);
      }
    }
    return score;
  };
};

AOC_TEST_CASE(21, 8, R"(30373
25512
65332
33549
35390
)")

}  // namespace
