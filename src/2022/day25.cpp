#include <range/v3/all.hpp>

#include "aoc/aoc.hpp"

namespace rv = ranges::views;

namespace {

namespace grammar {

namespace dsl = lexy::dsl;

struct line {
  static constexpr auto rule =
      dsl::identifier(dsl::ascii::digit / dsl::ascii::punct) + dsl::newline;
  static constexpr auto value = lexy::as_string<std::string>;
};

struct parser {
  static constexpr auto rule = dsl::terminator(dsl::eof).opt_list(dsl::p<line>);
  static constexpr auto value = lexy::as_list<std::vector<std::string>>;
};

}  // namespace grammar

using int_t = std::uint64_t;

int_t from_snafu(const std::string& s) {
  int_t base = 1;
  int_t result = 0;
  for (const auto& c : s | rv::reverse) {
    if (c == '=') {
      result -= 2 * base;
    } else if (c == '-') {
      result -= base;
    } else if (c == '1') {
      result += base;
    } else if (c == '2') {
      result += 2 * base;
    }
    base *= 5;
  }
  return result;
}

std::string to_snafu(const std::uint64_t& n) {
  std::string result;
  if ((n + 2) / 5 != 0) {
    result = to_snafu((n + 2) / 5);
  }
  auto m = (n + 2) % 5;
  if (m == 0) {
    return result + "=";
  } else if (m == 1) {
    return result + "-";
  } else if (m == 2) {
    return result + "0";
  } else if (m == 3) {
    return result + "1";
  } else if (m == 4) {
    return result + "2";
  }
  throw std::runtime_error("Invalid number for snafu base");
}

AOC_IMPL(2022, 25) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto data) {
    auto sum = ranges::accumulate(data | rv::transform(from_snafu), int_t(0));
    return to_snafu(sum);
  };
  static constexpr auto part2 = [](auto) { return 50; };
};

AOC_TEST_CASE("2=-1=0", 50, R"(1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122
)")

}  // namespace
