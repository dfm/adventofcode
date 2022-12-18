#include <unordered_set>

#include "aoc/aoc.hpp"

namespace {

using int_t = std::int64_t;
using coord_t = std::pair<int_t, int_t>;

inline int_t distance(const coord_t &a, const coord_t &b) {
  return std::abs(a.first - b.first) + std::abs(a.second - b.second);
}

struct sensor {
  coord_t pos;
  coord_t beacon;
  int_t min_dist;

  sensor(const coord_t &pos, const coord_t &beacon) : pos(pos), beacon(beacon) {
    min_dist = distance(pos, beacon);
  }
};

namespace grammar {

namespace dsl = lexy::dsl;

struct number {
  static constexpr auto rule = dsl::sign + dsl::integer<int_t>;
  static constexpr auto value = lexy::as_integer<int_t>;
};

struct coord {
  static constexpr auto rule =
      LEXY_LIT("x=") + dsl::p<number> + LEXY_LIT(", y=") + dsl::p<number>;
  static constexpr auto value = lexy::construct<coord_t>;
};

struct line {
  static constexpr auto rule = LEXY_LIT("Sensor at ") + dsl::p<coord> +
                               LEXY_LIT(": closest beacon is at ") +
                               dsl::p<coord> + dsl::newline;
  static constexpr auto value = lexy::construct<sensor>;
};

struct parser {
  static constexpr auto rule = dsl::terminator(dsl::eof).opt_list(dsl::p<line>);
  static constexpr auto value = lexy::as_list<std::vector<sensor>>;
};

}  // namespace grammar

AOC_IMPL(2022, 15) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto data) {
    int_t row = data.size() < 20 ? 10 : 2000000;
    std::unordered_set<coord_t> beacons;
    for (const auto &s : data) {
      beacons.insert(s.beacon);
    }

    int_t min_x = std::numeric_limits<int_t>::max();
    int_t max_x = std::numeric_limits<int_t>::min();
    for (const auto &s : data) {
      auto delta = s.min_dist - std::abs(s.pos.second - row);
      if (delta > 0) {
        min_x = std::min(s.pos.first - delta, min_x);
        max_x = std::max(s.pos.first + delta, max_x);
      }
    }

    size_t count = 0;
    for (int_t x = min_x; x <= max_x; ++x) {
      if (beacons.contains({x, row})) continue;

      for (const auto &s : data) {
        if (distance(s.pos, {x, row}) <= s.min_dist) {
          count++;
          break;
        }
      }
    }
    return count;
  };
  static constexpr auto part2 = [](auto data) {
    int_t max_val = data.size() < 20 ? 20 : 4000000;

    std::sort(data.begin(), data.end(),
              [](auto a, auto b) { return a.pos.first < b.pos.first; });

    for (int_t y = 0; y <= max_val; ++y) {
      int_t x = 0;
      for (const auto &s : data) {
        auto delta = s.min_dist - std::abs(s.pos.second - y);
        if (delta >= 0 && s.pos.first - delta <= x) {
          x = std::max(x, s.pos.first + delta);
        }
        if (x >= max_val) break;
      }
      if (x < max_val) {
        return (x + 1) * 4000000 + y;
      }
    }

    return int_t(0);
  };
};

AOC_TEST_CASE(26, 56000011,
              R"(Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3
)")

}  // namespace
