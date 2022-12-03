#include "aoc/aoc.hpp"

namespace {

namespace grammar {

namespace dsl = lexy::dsl;

struct rucksack {
  static constexpr auto rule =
      dsl::identifier(dsl::ascii::alpha) + dsl::newline;
  static constexpr auto value = lexy::as_string<std::string>;
};

struct parser {
  static constexpr auto rule =
      dsl::terminator(dsl::eof).opt_list(dsl::p<rucksack>);
  static constexpr auto value = lexy::as_list<std::vector<std::string>>;
};

}  // namespace grammar

inline auto score(std::string& s) {
  std::int64_t score = 0;
  auto idx = std::unique(s.begin(), s.end());
  for (auto c = s.begin(); c != idx; ++c) {
    if (*c >= 'a' && *c <= 'z') {
      score += *c - 'a' + 1;
    } else {
      score += *c - 'A' + 27;
    }
  }
  return score;
}

AOC_IMPL(2022, 3) {
  using parser = grammar::parser;

  static constexpr auto part1 = [](auto data) {
    std::int64_t total = 0;
    for (const auto& line : data) {
      auto middle = (line.end() - line.begin()) / 2 + line.begin();
      auto first = std::string(line.begin(), middle);
      auto second = std::string(middle, line.end());
      std::sort(first.begin(), first.end());
      std::sort(second.begin(), second.end());
      std::string inter;
      std::set_intersection(first.begin(), first.end(), second.begin(),
                            second.end(), std::back_inserter(inter));
      total += score(inter);
    }
    return total;
  };

  static constexpr auto part2 = [](auto data) {
    std::int64_t total = 0;
    for (auto idx = data.begin(); idx < data.end(); idx += 3) {
      auto r1 = *idx;
      auto r2 = *(idx + 1);
      auto r3 = *(idx + 2);
      std::sort(r1.begin(), r1.end());
      std::sort(r2.begin(), r2.end());
      std::sort(r3.begin(), r3.end());
      std::string tmp, result;
      std::set_intersection(r1.begin(), r1.end(), r2.begin(), r2.end(),
                            std::back_inserter(tmp));
      std::set_intersection(r3.begin(), r3.end(), tmp.begin(), tmp.end(),
                            std::back_inserter(result));
      total += score(result);
    }
    return total;
  };
};

AOC_TEST_CASE(157, 70, R"(vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
)")

}  // namespace
