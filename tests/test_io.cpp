#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest.h>

#include <numeric>

#include "aoc/io.hpp"

TEST_CASE("[io] istream") {
  using In = std::istream &;
  using Out = std::string;
  auto func = [](In in) -> Out {
    Out result;
    in >> result;
    return result;
  };
  auto wrapped = aoc::io::harness<In>::wrap<Out>(func);
  std::istringstream in("hello");
  REQUIRE(wrapped(in) == std::string("hello"));
}

TEST_CASE("[io] istream_iterator") {
  using In = std::istream_iterator<int>;
  using Out = int;
  auto func = [](In &in) -> Out { return std::reduce(in, In()); };
  auto wrapped = aoc::io::harness<In &>::wrap<Out>(func);
  std::istringstream in("0\n1\n3\n4\n");
  REQUIRE(wrapped(in) == 8);
}

TEST_CASE("[io] vector") {
  using In = const std::vector<int> &;
  using Out = int;
  auto func = [](In in) -> Out { return std::reduce(in.begin(), in.end()); };
  auto wrapped = aoc::io::harness<In>::wrap<Out>(func);
  std::istringstream in("0\n1\n3\n4\n");
  REQUIRE(wrapped(in) == 8);
}

TEST_CASE("[io] string") {
  using In = const std::string &;
  using Out = std::string;
  auto func = [](In in) -> Out { return in; };
  auto wrapped = aoc::io::harness<In>::wrap<Out>(func);
  std::istringstream in("hello");
  REQUIRE(wrapped(in) == std::string("hello"));
}
