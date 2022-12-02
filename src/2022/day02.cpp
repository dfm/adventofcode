#include "aoc/aoc.hpp"

namespace {

namespace grammar {
namespace dsl = lexy::dsl;

struct round {
  static constexpr auto whitespace = dsl::ascii::space;
  static constexpr auto rule =
      dsl::p<aoc::dsl::character> + dsl::p<aoc::dsl::character>;
  static constexpr auto value = lexy::construct<std::pair<char, char>>;
};

struct parser {
  static constexpr auto rule =
      dsl::terminator(dsl::eof).opt_list(dsl::p<round>);
  static constexpr auto value =
      lexy::as_list<std::vector<std::pair<char, char>>>;
};

}  // namespace grammar

inline std::int64_t score(const char a, const char b) {
  auto score = static_cast<std::int64_t>(b - 'A') + 1;
  if (a == b) {
    return score + 3;
  }
  if ((a == 'A' && b == 'B') || (a == 'B' && b == 'C') ||
      (a == 'C' && b == 'A')) {
    return score + 6;
  }
  return score;
}

inline std::int64_t play(const char a, const char b) {
  char choice;
  if (b == 'X') {
    choice =
        static_cast<char>((3 + (static_cast<int>(a - 'A') - 1) % 3) % 3) + 'A';
  } else if (b == 'Y') {
    choice = a;
  } else {
    choice =
        static_cast<char>((3 + (static_cast<int>(a - 'A') + 1) % 3) % 3) + 'A';
  }
  return score(a, choice);
}

AOC_IMPL(2022, 2) {
  using parser = grammar::parser;

  static constexpr auto part1 = [](auto data) {
    auto total = 0;
    for (const auto& round : data) {
      total += score(round.first, round.second - 'X' + 'A');
    }
    return total;
  };

  static constexpr auto part2 = [](auto data) {
    auto total = 0;
    for (const auto& round : data) {
      total += play(round.first, round.second);
    }
    return total;
  };
};

AOC_TEST_CASE(15, 12, R"(A Y
B X
C Z
)")

}  // namespace
