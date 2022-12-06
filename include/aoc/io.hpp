#ifndef AOC_IO_HPP
#define AOC_IO_HPP

#include <filesystem>
#include <lexy/action/parse.hpp>
#include <lexy/callback.hpp>
#include <lexy/dsl.hpp>
#include <lexy/input/file.hpp>
#include <lexy/input/string_input.hpp>
#include <lexy_ext/report_error.hpp>
#include <ostream>
#include <sstream>
#include <vector>

namespace aoc {

template <typename Parser>
auto parse(auto input) {
  auto result = lexy::parse<Parser>(input, lexy_ext::report_error);
  if (result.is_error()) {
    throw std::runtime_error("Failed to parse input");
  }
  return result.value();
}

template <typename Parser>
auto parse_from_file(auto func) {
  return [func](const std::filesystem::path &path) {
    auto file = lexy::read_file<lexy::ascii_encoding>(path.string().c_str());
    if (!file) {
      std::ostringstream msg;
      msg << "Failed to open " << path << " for reading";
      throw std::runtime_error(msg.str());
    }
    auto input = file.buffer();
    auto value = parse<Parser>(input);
    return func(value);
  };
}

template <typename Parser>
auto parse_from_string(auto func) {
  return [func](const std::string &data) {
    auto input = lexy::string_input(data);
    auto value = parse<Parser>(input);
    return func(value);
  };
}

namespace dsl {

template <typename Int = int>
struct signed_integer {
  static constexpr auto rule = lexy::dsl::sign + lexy::dsl::integer<Int>;
  static constexpr auto value = lexy::as_integer<Int>;
};

template <typename Int = int>
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