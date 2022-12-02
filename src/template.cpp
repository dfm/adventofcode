#include "aoc/aoc.hpp"

namespace {

namespace grammar {

namespace dsl = lexy::dsl;

struct number {
  static constexpr auto rule = dsl::sign + dsl::integer<std::int64_t>;
  static constexpr auto value = lexy::as_integer<std::int64_t>;
};

struct parser {
  static constexpr auto rule =
      dsl::terminator(dsl::eof).opt_list(dsl::p<number>);
  static constexpr auto value = lexy::as_list<std::vector<std::int64_t>>;
};

}  // namespace grammar

AOC_IMPL({{year}}, {{day}}) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto) { return 0; };
  static constexpr auto part2 = [](auto) { return 0; };
};

AOC_TEST_CASE(0, 0, R"()")

}  // namespace
