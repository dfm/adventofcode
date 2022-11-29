#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest.h>

#include "aoc/io.hpp"

struct parser {
  static constexpr auto rule = lexy::dsl::sign + lexy::dsl::integer<int>;
  static constexpr auto value = lexy::as_integer<int>;
};

TEST_CASE("[io] lexy") {
  auto func = [](auto in) { return in; };
  auto wrapped = aoc::parse<parser>(func);
  std::istringstream in("1");
  REQUIRE(wrapped(in) == 1);
}
