#include <type_traits>

#include "aoc/aoc.hpp"

namespace {
namespace grammar {
namespace dsl = lexy::dsl;
struct parser {
  static constexpr auto rule = dsl::identifier(dsl::ascii::alpha);
  static constexpr auto value = lexy::as_string<std::string>;
};
}  // namespace grammar

template <size_t dx, size_t dy, std::enable_if_t<(dx + 1 == dy), bool> = true>
inline bool match_impl(size_t n, const std::string &msg) {
  return (msg[n + dx] == msg[n + dy]);
}

template <size_t dx, size_t dy, std::enable_if_t<(dx + 1 < dy), bool> = true>
inline bool match_impl(size_t n, const std::string &msg) {
  return (msg[n + dx] == msg[n + dy]) | match_impl<dx, dy - 1>(n, msg);
}

template <size_t dx, size_t width, std::enable_if_t<(dx == 0), bool> = true>
inline bool match(size_t n, const std::string &msg) {
  return match_impl<dx, width>(n, msg);
}

template <size_t dx, size_t width, std::enable_if_t<(dx != 0), bool> = true>
inline bool match(size_t n, const std::string &msg) {
  return match_impl<dx, width>(n, msg) | match<dx - 1, width>(n, msg);
}

template <size_t width>
size_t find_first_unique(const std::string &msg) {
  for (size_t n = 0; n < msg.size() - width; ++n) {
    if (!match<width - 2, width - 1>(n, msg)) return n + width;
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
