#include "aoc/aoc.hpp"

namespace {

enum class choice {
  rock = 0,
  paper,
  scissors,
};

namespace grammar {
namespace dsl = lexy::dsl;

struct character {
  static constexpr auto rule = dsl::capture(dsl::ascii::upper);
  static constexpr auto value = lexy::callback<char>(
      [](auto lex) { return static_cast<char>(*lex.begin()); });
};

struct round {
  static constexpr auto whitespace = dsl::ascii::space;
  static constexpr auto rule = dsl::p<character> + dsl::p<character>;
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

auto part1 = [](auto data) {
  auto total = 0;
  for (const auto& round : data) {
    total += score(round.first, round.second - 'X' + 'A');
  }
  return total;
};

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

auto part2 = [](auto data) {
  auto total = 0;
  for (const auto& round : data) {
    total += play(round.first, round.second);
  }
  return total;
};

const static std::string test_data = R"(A Y
B X
C Z
)";

}  // namespace

AOC_REGISTER(2022, 2, grammar::parser, part1, part2);
AOC_TEST_CASE(2022, 2, part1, test_data, 15)
AOC_TEST_CASE(2022, 2, part2, test_data, 12)
