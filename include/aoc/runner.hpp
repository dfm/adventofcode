#ifndef AOC_RUNNER_HPP
#define AOC_RUNNER_HPP

#include <ostream>

#include "types.hpp"

namespace aoc {

void run(year_t year, day_t day, std::ostream& out);
void run_all(year_t year, std::ostream& out);

}  // namespace aoc

#endif
