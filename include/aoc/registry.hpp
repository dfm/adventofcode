#ifndef AOC_REGISTRY_HPP
#define AOC_REGISTRY_HPP

#include <chrono>
#include <filesystem>
#include <functional>
#include <map>
#include <ostream>
#include <utility>

#include "io.hpp"
#include "types.hpp"

#define AOC_IMPL(YEAR, DAY)                                              \
  struct implementation;                                                 \
  static const int __dummy =                                             \
      aoc::registry::register_implementation<implementation>(YEAR, DAY); \
  struct implementation

namespace aoc {
namespace registry {

using ticks_t = decltype(std::chrono::duration_cast<std::chrono::microseconds>(
                             std::chrono::steady_clock::now() -
                             std::chrono::steady_clock::now())
                             .count());

struct implementation {
  std::function<ticks_t(const std::filesystem::path &, std::ostream &)> part1;
  std::function<ticks_t(const std::filesystem::path &, std::ostream &)> part2;
};

typedef std::map<std::pair<year_t, day_t>, implementation> registry_t;
registry_t &get_registry();

std::function<ticks_t(const std::filesystem::path &, std::ostream &)> to_runner(
    auto func) {
  return [func](const std::filesystem::path &in, std::ostream &out) -> ticks_t {
    auto begin = std::chrono::steady_clock::now();
    auto result = func(in);
    auto end = std::chrono::steady_clock::now();
    out << result;
    return std::chrono::duration_cast<std::chrono::microseconds>(end - begin)
        .count();
  };
}

template <typename T>
int register_implementation(year_t year, day_t day) {
  get_registry()[std::make_pair(year, day)] =
      implementation{to_runner(parse_from_file<typename T::parser>(T::part1)),
                     to_runner(parse_from_file<typename T::parser>(T::part2))};
  return 0;
}

}  // namespace registry
}  // namespace aoc

#endif