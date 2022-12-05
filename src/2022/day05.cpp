#include "aoc/aoc.hpp"

namespace {

typedef std::vector<std::vector<char>> layout_t;

struct instruction_t {
  size_t amount;
  size_t from;
  size_t to;
};

struct problem_t {
  layout_t layout;
  std::vector<instruction_t> instructions;

  problem_t(const std::vector<std::string> &input_layout,
            const std::vector<instruction_t> &instructions)
      : instructions(instructions) {
    size_t num_columns =
        (input_layout[input_layout.size() - 1].length() + 1) / 4;
    layout.resize(num_columns);
    for (size_t idx = input_layout.size() - 1; idx > 0; --idx) {
      auto &line = input_layout[idx - 1];
      for (size_t col = 0; col < num_columns; ++col) {
        char c = line[col * 4 + 1];
        if (c != ' ') {
          layout[col].push_back(c);
        }
      }
    }
  }

  void execute_part1(const instruction_t &step) {
    auto &from = layout[step.from - 1];
    auto &to = layout[step.to - 1];
    for (size_t idx = 0; idx < step.amount; ++idx) {
      to.push_back(from.back());
      from.pop_back();
    }
  }

  void execute_part2(const instruction_t &step) {
    auto &from = layout[step.from - 1];
    auto &to = layout[step.to - 1];
    std::vector<char> tmp(step.amount);
    for (size_t idx = 0; idx < step.amount; ++idx) {
      tmp.push_back(from.back());
      from.pop_back();
    }
    for (size_t idx = 0; idx < step.amount; ++idx) {
      to.push_back(tmp.back());
      tmp.pop_back();
    }
  }

  std::string report() const {
    std::ostringstream oss;
    for (const auto &col : layout) {
      if (!col.empty()) {
        oss << col.back();
      } else {
        oss << ' ';
      }
    }
    return oss.str();
  }

  friend std::ostream &operator<<(std::ostream &, const problem_t &);
};

[[maybe_unused]] std::ostream &operator<<(std::ostream &os,
                                          const problem_t &problem) {
  size_t ind = 1;
  for (const auto &col : problem.layout) {
    os << ind++ << ": ";
    for (const auto &row : col) {
      os << row;
    }
    os << std::endl;
  }
  return os;
}

namespace grammar {

namespace dsl = lexy::dsl;

struct line {
  static constexpr auto rule = [] {
    auto allowed = dsl::ascii::alpha / dsl::ascii::digit / dsl::ascii::space /
                       LEXY_LIT("[") / LEXY_LIT("]") -
                   dsl::ascii::newline;
    return dsl::peek(allowed) >> dsl::list(dsl::capture(allowed));
  }();
  static constexpr auto value =
      lexy::as_string<std::string, lexy::ascii_encoding>;
};

struct layout {
  static constexpr auto rule = [] {
    return dsl::list(dsl::p<line>, dsl::trailing_sep(dsl::newline));
  }();
  static constexpr auto value = lexy::as_list<std::vector<std::string>>;
};

struct instruction {
  static constexpr auto rule = LEXY_LIT("move ") + dsl::integer<size_t> +
                               LEXY_LIT(" from ") + dsl::integer<size_t> +
                               LEXY_LIT(" to ") + dsl::integer<size_t> +
                               dsl::newline;
  static constexpr auto value = lexy::construct<instruction_t>;
};

struct instruction_set {
  static constexpr auto rule =
      dsl::terminator(dsl::eof).opt_list(dsl::p<instruction>);
  static constexpr auto value = lexy::as_list<std::vector<instruction_t>>;
};

struct parser {
  static constexpr auto rule =
      dsl::p<layout> + dsl::newline + dsl::p<instruction_set>;
  static constexpr auto value = lexy::construct<problem_t>;
};

}  // namespace grammar

AOC_IMPL(2022, 5) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto data) {
    for (const auto &step : data.instructions) {
      data.execute_part1(step);
    }
    return data.report();
  };
  static constexpr auto part2 = [](auto data) {
    for (const auto &step : data.instructions) {
      data.execute_part2(step);
    }
    return data.report();
  };
};

AOC_TEST_CASE(std::string("CMZ"), std::string("MCD"), R"(    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
)")

}  // namespace
