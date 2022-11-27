#include "aoc/runner.hpp"

#include <exception>
#include <sstream>

#include "aoc/io.hpp"
#include "aoc/registry.hpp"
#include "aoc/remote.hpp"

namespace aoc {

void run_with_data(year_t year, day_t day, std::istream& in,
                   std::ostream& out) {
  auto& registry = registry::get_registry();
  auto implementation = registry.find(std::make_pair(year, day));
  if (implementation == registry.end()) {
    std::ostringstream msg;
    msg << "No implementation registered for Dec " << day << ", " << year;
    throw std::runtime_error(msg.str());
  }

  auto part1 = implementation->second.part1;
  auto part2 = implementation->second.part2;

  out << "* Dec " << day << ", " << year << " *" << std::endl;
  out << "=> Part 1: ";
  part1(in, out);
  out << std::endl;

  out << "=> Part 2: ";
  part2(in, out);
  out << std::endl;
}

void run(year_t year, day_t day, std::ostream& out) {
  auto in = remote::data(year, day).get();
  run_with_data(year, day, in, out);
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
