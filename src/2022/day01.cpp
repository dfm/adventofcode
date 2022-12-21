#include <algorithm>
#include <numeric>
#include <range/v3/all.hpp>

#include "aoc/aoc.hpp"

namespace rv = ranges::views;

namespace {
typedef std::int64_t T;

namespace grammar {
namespace dsl = lexy::dsl;
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
}  // namespace grammar

AOC_IMPL(2022, 1) {
  using parser = grammar::parser;

  static constexpr auto part1 = [](auto data) {
    auto counts = data | rv::transform([](const auto &elf) {
                    return std::reduce(elf.begin(), elf.end());
                  });
    return ranges::max(counts);
  };

  static constexpr auto part2 = [](auto data) {
    auto counts = data | rv::transform([](const auto &elf) {
                    return std::reduce(elf.begin(), elf.end());
                  });
    std::vector<T> sorted(counts.begin(), counts.end());
    std::sort(sorted.begin(), sorted.end(), std::greater());
    return ranges::accumulate(sorted | rv::take(3), 0);
  };
};

AOC_TEST_CASE(24000, 45000, R"(1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
)")

}  // namespace
