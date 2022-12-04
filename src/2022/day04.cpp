#include "aoc/aoc.hpp"

namespace {

typedef std::uint32_t int_t;
typedef std::pair<int_t, int_t> pair_t;
typedef std::pair<pair_t, pair_t> line_t;

namespace grammar {

namespace dsl = lexy::dsl;

struct number {
  static constexpr auto rule = dsl::integer<int_t>;
  static constexpr auto value = lexy::as_integer<int_t>;
};

struct pair {
  static constexpr auto rule =
      dsl::p<number> + dsl::lit_c<'-'> + dsl::p<number>;
  static constexpr auto value = lexy::construct<pair_t>;
};

struct line {
  static constexpr auto rule =
      dsl::p<pair> + dsl::lit_c<','> + dsl::p<pair> + dsl::newline;
  static constexpr auto value = lexy::construct<line_t>;
};

struct parser {
  static constexpr auto rule = dsl::terminator(dsl::eof).opt_list(dsl::p<line>);
  static constexpr auto value = lexy::as_list<std::vector<line_t>>;
};

}  // namespace grammar

inline bool contains(const pair_t& a, const pair_t& b) {
  return (a.first <= b.first) && (b.second <= a.second);
}

inline bool overlaps(const pair_t& a, const pair_t& b) {
  return ((a.first <= b.first) && (b.first <= a.second)) ||
         ((a.first <= b.second) && (b.second <= a.second));
}

auto part(auto func, auto data) {
  std::int64_t count = 0;
  for (const auto& line : data) {
    auto flag = func(line.first, line.second) || func(line.second, line.first);
    count += static_cast<std::int64_t>(flag);
  }
  return count;
}

AOC_IMPL(2022, 4) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto data) {
    return part(std::function(contains), data);
  };
  static constexpr auto part2 = [](auto data) {
    return part(std::function(overlaps), data);
  };
};

AOC_TEST_CASE(2, 4, R"(2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
)")

}  // namespace
