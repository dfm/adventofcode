#include <variant>

#include "aoc/aoc.hpp"

namespace {

namespace packet {

struct packet_value;
using literal = std::int64_t;
using array = std::vector<packet_value>;
struct packet_value {
  std::variant<literal, array> v;

  template <typename T>
  packet_value(T t) : v(std::move(t)) {}

  friend bool operator<(const packet_value& a, const packet_value& b);
};

}  // namespace packet

namespace grammar {

namespace dsl = lexy::dsl;

struct token;

struct literal : lexy::token_production {
  struct number : lexy::transparent_production {
    static constexpr auto rule = dsl::integer<packet::literal>;
    static constexpr auto value = lexy::as_integer<packet::literal>;
  };

  static constexpr auto rule = dsl::p<number>;
  static constexpr auto value = lexy::construct<packet::packet_value>;
};

struct array {
  static constexpr auto rule =
      dsl::square_bracketed.opt_list(dsl::recurse<token>, dsl::sep(dsl::comma));
  static constexpr auto value = lexy::as_list<packet::array>;
};

struct token : lexy::transparent_production {
  static constexpr auto rule = dsl::p<literal> | dsl::p<array>;
  static constexpr auto value = lexy::construct<packet::packet_value>;
};

struct packet_value {
  static constexpr auto whitespace = dsl::ascii::space;
  [[maybe_unused]] static constexpr auto max_recursion_depth = 19;
  static constexpr auto rule = dsl::p<token>;
  static constexpr auto value = lexy::forward<packet::packet_value>;
};

struct parser {
  static constexpr auto rule =
      dsl::terminator(dsl::eof).opt_list(dsl::p<packet_value>);
  static constexpr auto value =
      lexy::as_list<std::vector<packet::packet_value>>;
};

}  // namespace grammar

enum comp_t { wrong = 0, right, same };

comp_t compare(const packet::packet_value& a, const packet::packet_value& b);

comp_t compare(const packet::literal& a, const packet::literal& b) {
  if (a < b) return comp_t::right;
  if (a == b) return comp_t::same;
  return comp_t::wrong;
}

comp_t compare(const packet::array& a, const packet::array& b) {
  auto size = std::min(a.size(), b.size());
  for (size_t n = 0; n < size; ++n) {
    auto flag = compare(a[n], b[n]);
    if (flag != comp_t::same) return flag;
  }
  if (a.size() < b.size()) return comp_t::right;
  if (a.size() == b.size()) return comp_t::same;
  return comp_t::wrong;
}

comp_t compare(const packet::literal& a, const packet::array& b) {
  return compare(packet::array{packet::packet_value(a)}, b);
}

comp_t compare(const packet::array& a, const packet::literal& b) {
  return compare(a, packet::array{packet::packet_value(b)});
}

comp_t compare(const packet::packet_value& a, const packet::packet_value& b) {
  return std::visit(
      [&](const auto& v1) {
        return std::visit([&](const auto& v2) { return compare(v1, v2); }, b.v);
      },
      a.v);
}

namespace packet {
bool operator<(const packet_value& a, const packet_value& b) {
  return compare(a, b) == comp_t::right;
}
}  // namespace packet

AOC_IMPL(2022, 13) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto data) {
    size_t result = 0;
    for (size_t n = 0; n < data.size(); n += 2) {
      auto flag = compare(data[n], data[n + 1]);
      if (flag == comp_t::right) {
        result += n / 2 + 1;
      }
    }
    return result;
  };
  static constexpr auto part2 = [](auto packets) {
    auto spacers =
        lexy::parse<grammar::parser>(lexy::zstring_input("[[2]]\n[[6]]\n"),
                                     lexy_ext::report_error)
            .value();
    packets.insert(packets.end(), spacers.begin(), spacers.end());
    std::sort(packets.begin(), packets.end());

    size_t result = 1;
    size_t n = 1;
    for (const auto& p : packets) {
      if (compare(p, spacers[0]) == comp_t::same) result *= n;
      if (compare(p, spacers[1]) == comp_t::same) result *= n;
      ++n;
    }
    return result;
  };
};

AOC_TEST_CASE(13, 140, R"([1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
)")

}  // namespace
