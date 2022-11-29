#ifndef AOC_TESTING_HPP
#define AOC_TESTING_HPP

#include <doctest.h>

#include <sstream>

#include "./io.hpp"

#define AOC_TEST_CASE_IMPL(year, day, parser, func, data, expect)      \
  TEST_CASE("[" #year "-" #day "]") {                                  \
    REQUIRE(aoc::testing::execute_part<parser>(func, data) == expect); \
  }

#define AOC_TEST_CASE(func, data, expect) \
  AOC_TEST_CASE_IMPL(__aoc_year, __aoc_day, __aoc_parser, func, data, expect)

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
