#include "aoc/aoc.hpp"

namespace {

namespace grammar {
namespace dsl = lexy::dsl;
struct parser {
  static constexpr auto rule = dsl::identifier(dsl::ascii::alpha);
  static constexpr auto value = lexy::as_string<std::string>;
};
}  // namespace grammar

template <size_t width>
size_t find_first_unique(const std::string &msg) {
  for (size_t n = width - 1; n < msg.size(); ++n) {
    bool unique = true;
    for (size_t i = 0; unique && (i < width); ++i) {
      for (size_t j = i + 1; unique && (j < width); ++j) {
        if (msg[n - j] == msg[n - i]) {
          unique = false;
        }
      }
    }
    if (unique) {
      return n + 1;
    }
  }
  return 0;
}

AOC_IMPL(2022, 6) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto data) {
    return find_first_unique<4>(data);
  };
  static constexpr auto part2 = [](auto data) {
    return find_first_unique<14>(data);
  };
};

AOC_TEST_CASE(7, 19, R"(mjqjpqmgbljsphdztnvjfqwrcgsmlb
)")

}  // namespace
