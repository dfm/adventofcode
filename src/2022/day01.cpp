#include <algorithm>
#include <numeric>

#include "aoc/aoc.hpp"

namespace {

namespace dsl = lexy::dsl;
typedef std::int64_t T;
typedef std::vector<T> elf_t;
typedef std::vector<elf_t> elves_t;

struct elf {
  static constexpr auto rule =
      dsl::list(dsl::integer<T>, dsl::trailing_sep(dsl::newline));
  static constexpr auto value = lexy::as_list<elf_t>;
};

struct parser {
  static constexpr auto rule = dsl::list(dsl::p<elf>, dsl::sep(dsl::newline));
  static constexpr auto value = lexy::as_list<elves_t>;
};

auto part1 = [](auto data) {
  T max = 0;
  for (const auto &elf : data) {
    auto value = std::reduce(elf.begin(), elf.end());
    max = std::max(max, value);
  }
  return max;
};

auto part2 = [](auto data) {
  std::vector<T> counts(data.size());
  for (std::size_t n = 0; n < data.size(); ++n) {
    counts[n] = std::reduce(data[n].begin(), data[n].end());
  }
  std::sort(counts.begin(), counts.end());
  return counts[counts.size() - 3] + counts[counts.size() - 2] +
         counts[counts.size() - 1];
};

const static std::string test_data = R"(1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
)";

}  // namespace

AOC_REGISTER(2022, 1, parser, part1, part2);
AOC_TEST_CASE(2022, 1, part1, test_data, 24000)
AOC_TEST_CASE(2022, 1, part2, test_data, 45000)
