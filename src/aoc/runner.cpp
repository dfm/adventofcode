#include "aoc/runner.hpp"

#include <exception>
#include <sstream>

#include "aoc/io.hpp"
#include "aoc/registry.hpp"
#include "aoc/remote.hpp"

namespace aoc {

registry::implementation get_implementation(year_t year, day_t day) {
  auto& registry = registry::get_registry();
  auto implementation = registry.find(std::make_pair(year, day));
  if (implementation == registry.end()) {
    std::ostringstream msg;
    msg << "No implementation registered for Dec " << day << ", " << year;
    throw std::runtime_error(msg.str());
  }
  return implementation->second;
}

void run(year_t year, day_t day, std::ostream& out) {
  auto implementation = get_implementation(year, day);
  out << "* Dec " << day << ", " << year << " *" << std::endl;

  auto in1 = remote::data(year, day).get();
  out << "=> Part 1: ";
  implementation.part1(in1, out);
  out << std::endl;

  auto in2 = remote::data(year, day).get();
  out << "=> Part 2: ";
  implementation.part2(in2, out);
  out << std::endl;
}

void run_all(year_t year, std::ostream& out) {
  std::size_t count = 0;
  for (const auto& impl : registry::get_registry()) {
    if (impl.first.first == year) {
      run(year, impl.first.second, out);
      count++;
    }
  }

  if (count == 0) {
    std::ostringstream msg;
    msg << "No implementations registered for Dec " << year;
    throw std::runtime_error(msg.str());
  }
}

}  // namespace aoc
