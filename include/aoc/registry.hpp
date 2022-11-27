#ifndef AOC_REGISTRY_HPP
#define AOC_REGISTRY_HPP

#include <functional>
#include <iostream>
#include <map>

#include "./io.hpp"
#include "./types.hpp"

#define AOC_REGISTER(year, day, part1, part2)                        \
  static const aoc::year_t __aoc_year = year;                        \
  static const aoc::day_t __aoc_day = day;                           \
  static const int __dummy = aoc::registry::register_implementation( \
      __aoc_year, __aoc_day, std::function(part1), std::function(part2))

namespace aoc {
namespace registry {

struct implementation {
  std::function<void(std::istream &, std::ostream &)> part1;
  std::function<void(std::istream &, std::ostream &)> part2;
};

typedef std::map<std::pair<year_t, day_t>, implementation> registry_t;
registry_t &get_registry();

template <typename I1, typename O1, typename I2, typename O2>
int register_implementation(year_t year, day_t day, std::function<O1(I1)> part1,
                            std::function<O2(I2)> part2) {
  get_registry()[std::make_pair(year, day)] = implementation{
      io::harness_to_runner(io::harness<I1>::template wrap<O1>(part1)),
      io::harness_to_runner(io::harness<I2>::template wrap<O2>(part2))};
  return 0;
}

template <typename T1, typename T2>
void register_implementation(year_t year, day_t day, T1 part1, T2 part2) {
  register_implementation(year, day, std::function(part1),
                          std::function(part2));
}

}  // namespace registry
}  // namespace aoc

#endif