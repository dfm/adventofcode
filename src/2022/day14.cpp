#include <range/v3/all.hpp>
#include <unordered_set>

#include "aoc/aoc.hpp"

namespace rv = ranges::views;

namespace {

using int_t = std::int64_t;
using coord_t = std::pair<int_t, int_t>;
using segment_t = std::vector<coord_t>;

namespace grammar {

namespace dsl = lexy::dsl;

struct number {
  static constexpr auto rule = dsl::integer<int_t>;
  static constexpr auto value = lexy::as_integer<int_t>;
};

struct coord {
  static constexpr auto rule = dsl::p<number> + dsl::comma + dsl::p<number>;
  static constexpr auto value = lexy::construct<coord_t>;
};

struct segment {
  static constexpr auto rule =
      dsl::terminator(dsl::newline)
          .opt_list(dsl::p<coord>, dsl::sep(LEXY_LIT(" -> ")));
  static constexpr auto value = lexy::as_list<segment_t>;
};

struct parser {
  static constexpr auto rule =
      dsl::terminator(dsl::eof).opt_list(dsl::p<segment>);
  static constexpr auto value = lexy::as_list<std::vector<segment_t>>;
};

}  // namespace grammar

template <typename T>
inline T sign(const T& a) {
  return T(a > T(0)) - T(a < T(0));
}

struct problem {
  std::unordered_set<coord_t> blocked;
  int_t min_x = std::numeric_limits<int_t>::max(),
        max_x = std::numeric_limits<int_t>::min(), min_y = 0,
        max_y = std::numeric_limits<int_t>::min();

  problem(auto data) {
    for (const auto& s : data) {
      for (size_t n = 0; n < s.size() - 1; ++n) {
        auto dx = sign(s[n + 1].first - s[n].first);
        auto dy = sign(s[n + 1].second - s[n].second);
        if (dx != 0) {
          auto rng = rv::iota(s[n].first, s[n + 1].first + dx) | rv::stride(dx);
          for (const auto& x : rng) {
            blocked.insert({x, s[n].second});
          }
        } else if (dy != 0) {
          auto rng =
              rv::iota(s[n].second, s[n + 1].second + dy) | rv::stride(dy);
          for (const auto& y : rng) {
            blocked.insert({s[n].first, y});
          }
        }
      }
    }
    for (const auto& c : blocked) {
      min_x = std::min(min_x, c.first);
      max_x = std::max(max_x, c.first);
      min_y = std::min(min_y, c.second);
      max_y = std::max(max_y, c.second);
    }
  }

  bool step(coord_t& c) const {
    if (c.second == max_y + 1) return false;
    c.second += 1;
    if (blocked.contains(c)) {
      c.first -= 1;
      if (blocked.contains(c)) {
        c.first += 2;
        if (blocked.contains(c)) {
          c.first -= 1;
          c.second -= 1;
          return false;
        }
        return true;
      }
      return true;
    }
    return true;
  }

  coord_t drop() {
    coord_t sand{500, 0};
    while (step(sand)) {
    }
    blocked.insert(sand);
    return sand;
  }
};

AOC_IMPL(2022, 14) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto data) {
    problem p(data);
    size_t count = 0;
    while (p.drop().second < p.max_y) {
      count++;
    }
    return count;
  };
  static constexpr auto part2 = [](auto data) {
    problem p(data);
    size_t count = 0;
    while (true) {
      count++;
      auto next = p.drop();
      if (next.first == 500 && next.second == 0) break;
    }
    return count;
  };
};

AOC_TEST_CASE(24, 93, R"(498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
)")

}  // namespace
