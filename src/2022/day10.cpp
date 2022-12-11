#include "aoc/aoc.hpp"

namespace {

typedef std::int64_t int_t;
typedef std::pair<int_t, int_t> instr_t;

namespace grammar {

namespace dsl = lexy::dsl;

struct number {
  static constexpr auto rule = dsl::sign + dsl::integer<int_t>;
  static constexpr auto value = lexy::as_integer<int_t>;
};

struct addx {
  static constexpr auto rule = LEXY_LIT("addx ") >> dsl::p<number>;
  static constexpr auto value = lexy::callback<instr_t>([](auto val) {
    return instr_t{2, val};
  });
};

struct noop {
  static constexpr auto rule = LEXY_LIT("noop");
  static constexpr auto value = lexy::callback<instr_t>([]() {
    return instr_t{1, 0};
  });
};

struct instr {
  static constexpr auto rule = (dsl::p<addx> | dsl::p<noop>)+dsl::newline;
  static constexpr auto value = lexy::forward<instr_t>;
};

struct parser {
  static constexpr auto rule =
      dsl::terminator(dsl::eof).opt_list(dsl::p<instr>);
  static constexpr auto value = lexy::as_list<std::vector<instr_t>>;
};

}  // namespace grammar

AOC_IMPL(2022, 10) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto data) {
    int_t result = 0;
    int_t cycle = 0;
    int_t mag = 1;
    for (const auto& i : data) {
      for (int_t n = 0; n < i.first; ++n) {
        cycle++;
        if ((cycle - 20) % 40 == 0) {
          result += cycle * mag;
        }
      }
      mag += i.second;
    }
    return result;
  };
  static constexpr auto part2 = [](auto data) {
    int_t cycle = 0;
    int_t x = 1;
    std::ostringstream s;
    for (const auto& i : data) {
      for (int_t n = 0; n < i.first; ++n) {
        auto x0 = cycle % 40;
        if (x0 == 0) s << std::endl;
        if ((x - 1 <= x0) && (x0 <= x + 1))
          s << "#";
        else
          s << ".";
        cycle++;
      }
      x += i.second;
    }
    return s.str();
  };
};

AOC_TEST_CASE(13140, std::string(R"(
##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....)"),
              R"(addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
)")

}  // namespace
