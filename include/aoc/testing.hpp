#ifndef AOC_TESTING_HPP
#define AOC_TESTING_HPP

#include <doctest.h>

#include <sstream>

#include "./io.hpp"

#define AOC_TEST_CASE(year, day, func, data, expect)                         \
  TEST_CASE(#year "-" #day ": " #func " (" #expect ")") {                    \
    REQUIRE(aoc::testing::execute_part<__aoc_parser>(func, data) == expect); \
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
