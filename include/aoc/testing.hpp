#ifndef AOC_TESTING_HPP
#define AOC_TESTING_HPP

#include <sstream>

#include "./io.hpp"

#define AOC_TEST_CASE(year, day, part, func, data, expect)                    \
  TEST_CASE("[" #year "-" #day "-" #part "]") {                               \
    REQUIRE(aoc::testing::execute_part(std::function(func), data) == expect); \
  }

namespace aoc {
namespace testing {

template <typename I, typename O>
inline O execute_part(std::function<O(I)> part, const std::string &data) {
  std::istringstream in(data);
  auto wrapped = io::harness<I>::template wrap<O>(part);
  return wrapped(in);
}

}  // namespace testing
}  // namespace aoc

#endif
