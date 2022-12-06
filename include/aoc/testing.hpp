#ifndef AOC_TESTING_HPP
#define AOC_TESTING_HPP

#include <doctest.h>

#include "io.hpp"

#define AOC_PART1_TEST_CASE(expect, data)                                      \
  TEST_CASE("Part 1") {                                                        \
    [[maybe_unused]] auto wrapped =                                            \
        aoc::parse_from_string<implementation::parser>(implementation::part1); \
    REQUIRE(wrapped(data) == expect);                                          \
  }

#define AOC_PART2_TEST_CASE(expect, data)                                      \
  TEST_CASE("Part 2") {                                                        \
    [[maybe_unused]] auto wrapped =                                            \
        aoc::parse_from_string<implementation::parser>(implementation::part2); \
    REQUIRE(wrapped(data) == expect);                                          \
  }

#define AOC_TEST_CASE(expect1, expect2, data) \
  AOC_PART1_TEST_CASE(expect1, data)          \
  AOC_PART2_TEST_CASE(expect2, data)

#endif
