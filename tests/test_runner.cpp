#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest.h>

#include <numeric>

#include "aoc/registry.hpp"
#include "aoc/runner.hpp"

TEST_CASE("[runner]") {
  using In = std::istream &;
  using Out = int;
  auto part1 = [](In in) -> Out { return 1; };
  auto part2 = [](In in) -> Out { return 2; };
  // aoc::registry::register_implementation(2021, 1, part1, part2);
  // std::istringstream in("hello");
  // std::ostringstream out;
  // aoc::run_with_data(2021, 1, in, out);
  // REQUIRE(out.str() ==
  //         std::string("* Dec 1, 2021 *\n=> Part 1: 1\n=> Part 2: 2\n"));
}
