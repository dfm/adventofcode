#ifndef AOC_REGISTRY_HPP
#define AOC_REGISTRY_HPP

#include <functional>
#include <iostream>
#include <map>

#include "./io.hpp"
#include "./types.hpp"

#define AOC_REGISTER(year, day, parser, part1, part2)                \
  static const aoc::year_t __aoc_year = year;                        \
  static const aoc::day_t __aoc_day = day;                           \
  using __aoc_parser = parser;                                       \
  static const int __dummy = aoc::registry::register_implementation( \
      __aoc_year, __aoc_day, aoc::parse<__aoc_parser>(part1),        \
      aoc::parse<__aoc_parser>(part2))

namespace aoc {
namespace registry {

struct implementation {
  std::function<void(std::istream &, std::ostream &)> part1;
  std::function<void(std::istream &, std::ostream &)> part2;
};

typedef std::map<std::pair<year_t, day_t>, implementation> registry_t;
registry_t &get_registry();

int register_implementation(year_t year, day_t day, auto part1, auto part2) {
  get_registry()[std::make_pair(year, day)] =
      implementation{to_runner(part1), to_runner(part2)};
  return 0;
}

}  // namespace registry
}  // namespace aoc

#endif