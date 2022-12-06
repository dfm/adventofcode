#include <doctest.h>

#include <sstream>

#include "aoc/io.hpp"

struct parser {
  static constexpr auto rule = lexy::dsl::sign + lexy::dsl::integer<int>;
  static constexpr auto value = lexy::as_integer<int>;
};

TEST_CASE("[io] lexy") {
  auto func = [](auto in) { return in; };
  auto wrapped = aoc::parse_from_string<parser>(func);
  REQUIRE(wrapped("1") == 1);
}
