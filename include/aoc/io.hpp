#ifndef AOC_IO_HPP
#define AOC_IO_HPP

#include <cstdint>
#include <functional>
#include <istream>
#include <lexy/action/parse.hpp>
#include <lexy/callback.hpp>
#include <lexy/dsl.hpp>
#include <lexy/input/string_input.hpp>
#include <lexy_ext/report_error.hpp>
#include <ostream>
#include <vector>

namespace aoc {

template <typename Parser>
auto parse(auto func) {
  return [func](std::istream &in) {
    std::string s(std::istreambuf_iterator<char>(in), {});
    auto input = lexy::string_input(s);
    auto result = lexy::parse<Parser>(input, lexy_ext::report_error);
    if (result.is_error()) {
      throw std::runtime_error("Failed to parse input");
    }
    return func(result.value());
  };
}

std::function<void(std::istream &, std::ostream &)> to_runner(auto func) {
  return [func](std::istream &in, std::ostream &out) { out << func(in); };
}

namespace dsl {

template <typename Int = std::int64_t>
struct signed_integer {
  static constexpr auto rule = lexy::dsl::sign + lexy::dsl::integer<Int>;
  static constexpr auto value = lexy::as_integer<Int>;
};

template <typename Int = std::int64_t>
struct vector_of_signed_integers {
  static constexpr auto rule = lexy::dsl::terminator(lexy::dsl::eof)
                                   .opt_list(lexy::dsl::p<signed_integer<Int>>);
  static constexpr auto value = lexy::as_list<std::vector<Int>>;
};

struct character {
  static constexpr auto rule = lexy::dsl::capture(lexy::dsl::ascii::upper);
  static constexpr auto value = lexy::callback<char>(
      [](auto lex) { return static_cast<char>(*lex.begin()); });
};

}  // namespace dsl
}  // namespace aoc

#endif