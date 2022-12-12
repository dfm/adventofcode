#include <numeric>
#include <optional>

#include "aoc/aoc.hpp"

namespace {

enum op_t { add = 0, mul };
typedef std::uint64_t int_t;
typedef std::tuple<std::optional<int_t>, op_t, std::optional<int_t>> op_args_t;

struct monkey_t {
  int_t id;
  std::vector<int_t> items;
  op_args_t op;
  int_t mod;
  int_t pass_true;
  int_t pass_false;
};

namespace grammar {

namespace dsl = lexy::dsl;

struct number {
  static constexpr auto rule = dsl::integer<int_t>;
  static constexpr auto value = lexy::as_integer<int_t>;
};

struct monkey_id {
  static constexpr auto rule = LEXY_LIT("Monkey ") >>
                               (dsl::p<number> + LEXY_LIT(":"));
  static constexpr auto value = lexy::forward<int_t>;
};

struct starting_items {
  static constexpr auto rule = LEXY_LIT("Starting items: ") >>
                               dsl::list(dsl::p<number>,
                                         dsl::sep(LEXY_LIT(", ")));
  static constexpr auto value = lexy::as_list<std::vector<int_t>>;
};

struct operation {
  struct op {
    static constexpr auto whitespace = dsl::ascii::space;
    static constexpr auto ops =
        lexy::symbol_table<op_t>.map<LEXY_SYMBOL("+")>(op_t::add).map<LEXY_SYMBOL("*")>(
            op_t::mul);
    static constexpr auto rule = [] {
      auto name = dsl::identifier(dsl::ascii::alpha);
      auto reference = dsl::symbol<ops>(name);
      return reference;
    }();
    static constexpr auto value = lexy::forward<op_t>;
  };

  struct arg {
    struct old {
      static constexpr auto rule = LEXY_LIT("old");
      static constexpr auto value = lexy::construct<std::optional<int_t>>;
    };

    struct num {
      static constexpr auto rule = dsl::else_ >> dsl::integer<int_t>;
      static constexpr auto value = lexy::callback<std::optional<int_t>>(
          [](auto v) -> std::optional<int_t> { return v; });
    };

    static constexpr auto rule = dsl::p<old> | dsl::p<num>;
    static constexpr auto value = lexy::forward<std::optional<int_t>>;
  };

  static constexpr auto rule = LEXY_LIT("Operation: new = ") >>
                               (dsl::p<arg> + dsl::p<op> + dsl::p<arg>);
  static constexpr auto value = lexy::construct<op_args_t>;
};

struct test {
  static constexpr auto rule =
      LEXY_LIT("Test: divisible by ") >> dsl::p<number>;
  static constexpr auto value = lexy::forward<int_t>;
};

struct true_case {
  static constexpr auto rule =
      LEXY_LIT("If true: throw to monkey ") >> dsl::p<number>;
  static constexpr auto value = lexy::forward<int_t>;
};

struct false_case {
  static constexpr auto rule =
      LEXY_LIT("If false: throw to monkey ") >> dsl::p<number>;
  static constexpr auto value = lexy::forward<int_t>;
};

struct monkey {
  static constexpr auto whitespace = dsl::ascii::newline / dsl::ascii::blank;
  static constexpr auto rule = dsl::p<monkey_id> + dsl::p<starting_items> +
                               dsl::p<operation> + dsl::p<test> +
                               dsl::p<true_case> + dsl::p<false_case>;
  static constexpr auto value = lexy::construct<monkey_t>;
};

struct parser {
  static constexpr auto rule =
      dsl::terminator(dsl::eof).opt_list(dsl::p<monkey>);
  static constexpr auto value = lexy::as_list<std::vector<monkey_t>>;
};

}  // namespace grammar

inline int_t exec_op(const op_args_t& op_args, const int_t& value) {
  int_t a = std::get<0>(op_args).value_or(value);
  int_t b = std::get<2>(op_args).value_or(value);
  if (std::get<1>(op_args) == op_t::add) return a + b;
  return a * b;
}

template <int_t max_iter, int_t norm>
inline int_t run(std::vector<monkey_t>& data) {
  auto mod = std::accumulate(data.begin(), data.end(), int_t(1),
                             [](auto a, auto b) { return a * b.mod; });
  std::vector<int_t> counts(data.size());
  for (int_t i = 0; i < max_iter; ++i) {
    int_t n = 0;
    for (auto& m : data) {
      counts[n++] += m.items.size();
      for (const auto& i : m.items) {
        auto level = (exec_op(m.op, i) / norm) % mod;
        if (level % m.mod == 0) {
          data[m.pass_true].items.push_back(level);
        } else {
          data[m.pass_false].items.push_back(level);
        }
      }
      m.items.clear();
    }
  }
  std::sort(counts.begin(), counts.end());
  return counts[counts.size() - 1] * counts[counts.size() - 2];
}

AOC_IMPL(2022, 11) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto data) { return run<20, 3>(data); };
  static constexpr auto part2 = [](auto data) { return run<10000, 1>(data); };
};

AOC_TEST_CASE(10605, 2713310158, R"(Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
)")

}  // namespace
