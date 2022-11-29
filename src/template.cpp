#include "aoc/aoc.hpp"

namespace {

namespace dsl = lexy::dsl;
typedef std::int64_t T;

struct line {
  static constexpr auto rule = dsl::sign + dsl::integer<T>;
  static constexpr auto value = lexy::as_integer<T>;
};

struct parser {
  static constexpr auto rule = dsl::terminator(dsl::eof).opt_list(dsl::p<line>);
  static constexpr auto value = lexy::as_list<std::vector<T>>;
};

auto part1 = [](auto) { return 0; };

auto part2 = [](auto) { return 0; };

}  // namespace

AOC_REGISTER({{year}}, {{day}}, parser, part1, part2);
AOC_TEST_CASE(part1, "0", 0)
AOC_TEST_CASE(part2, "0", 0)
