#ifndef AOC_TESTING_HPP
#define AOC_TESTING_HPP

#include <doctest.h>

#include <sstream>

#include "./io.hpp"

#define AOC_TEST_CASE(expect1, expect2, data)                      \
  TEST_CASE("Part 1") {                                            \
    auto input = std::istringstream(data);                         \
    [[maybe_unused]] auto wrapped =                                \
        aoc::parse<implementation::parser>(implementation::part1); \
    REQUIRE(wrapped(input) == expect1);                            \
  }                                                                \
  TEST_CASE("Part 2") {                                            \
    auto input = std::istringstream(data);                         \
    [[maybe_unused]] auto wrapped =                                \
        aoc::parse<implementation::parser>(implementation::part2); \
    REQUIRE(wrapped(input) == expect2);                            \
  }

namespace aoc {
namespace testing {

template <typename Production>
auto execute_part(auto part, const std::string &data) {
  std::istringstream in(data);
  auto wrapped = parse<Production>(part);
  return wrapped(in);
}

}  // namespace testing
}  // namespace aoc

#endif
