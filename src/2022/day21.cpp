#include <optional>
#include <stack>
#include <unordered_map>
#include <unordered_set>

#include "aoc/aoc.hpp"

namespace {

using int_t = std::int64_t;
enum op_t { add = 0, sub, mul, div };
using expr_t = std::tuple<op_t, std::string, std::string>;
using instr_t = std::pair<std::optional<int_t>, std::optional<expr_t>>;

struct context {
  std::unordered_map<std::string, int_t> _values;
  std::unordered_map<std::string, expr_t> _expressions;

  void push_back(std::pair<std::string, instr_t>&& v) {
    auto [name, instr] = v;
    if (instr.first) {
      _values.insert_or_assign(name, instr.first.value());
    } else {
      _expressions.insert_or_assign(name, instr.second.value());
    }
  }

  int_t get(const std::string& key) {
    auto q = _values.find(key);
    if (q != _values.end()) {
      return q->second;
    }
    int_t value = 0;
    auto [op, a_key, b_key] = _expressions[key];
    auto a = get(a_key);
    auto b = get(b_key);
    if (op == op_t::add) {
      value = a + b;
    } else if (op == op_t::sub) {
      value = a - b;
    } else if (op == op_t::mul) {
      value = a * b;
    } else {
      value = a / b;
    }
    _values.insert_or_assign(key, value);
    return value;
  }

  std::vector<std::string> find_path_to_human() const {
    std::unordered_map<std::string, std::string> parents;
    std::unordered_set<std::string> visited;
    std::stack<std::string> todo;
    todo.push("root");
    while (!todo.empty()) {
      auto next = todo.top();
      todo.pop();
      if (next == "humn") {
        break;
      }

      if (!visited.contains(next)) {
        visited.insert(next);
        auto q = _expressions.find(next);
        if (q != _expressions.end()) {
          auto [_, a, b] = q->second;
          parents.insert_or_assign(a, next);
          parents.insert_or_assign(b, next);
          todo.push(a);
          todo.push(b);
        }
      }
    }

    std::vector<std::string> path;
    std::string current = "humn";
    while (current != "root") {
      path.push_back(current);
      current = parents[current];
    }
    std::reverse(path.begin(), path.end());
    return path;
  }

  int_t part2() {
    auto path = find_path_to_human();
    auto [_, a_root, b_root] = _expressions["root"];
    int_t carry = a_root == path[0] ? get(b_root) : get(a_root);

    for (size_t n = 0; n < path.size() - 1; ++n) {
      auto [op, a_key, b_key] = _expressions[path[n]];
      auto other = a_key == path[n + 1] ? get(b_key) : get(a_key);
      if (op == op_t::add) {
        carry -= other;
      } else if (op == op_t::sub) {
        if (a_key == path[n + 1]) {
          carry += other;
        } else {
          carry = other - carry;
        }
      } else if (op == op_t::mul) {
        carry /= other;
      } else {
        if (a_key == path[n + 1]) {
          carry *= other;
        } else {
          carry = other / carry;
        }
      }
    }

    return carry;
  }
};

namespace grammar {

namespace dsl = lexy::dsl;

struct number {
  static constexpr auto rule = dsl::integer<int_t>;
  static constexpr auto value = lexy::callback<instr_t>([](auto v) -> instr_t {
    return {lexy::as_integer<int_t>(v), {}};
  });
};

struct op {
  static constexpr auto ops =
        lexy::symbol_table<op_t>.map<LEXY_SYMBOL("+")>(op_t::add).map<LEXY_SYMBOL("-")>(
            op_t::sub).map<LEXY_SYMBOL("*")>(
            op_t::mul).map<LEXY_SYMBOL("/")>(
            op_t::div);
  static constexpr auto rule = [] {
    auto name = dsl::identifier(dsl::ascii::alpha);
    auto reference = dsl::symbol<ops>(name);
    return reference;
  }();
  static constexpr auto value = lexy::forward<op_t>;
};

struct expr {
  static constexpr auto whitespace = dsl::ascii::space;
  static constexpr auto rule = dsl::identifier(dsl::ascii::alpha) + dsl::p<op> +
                               dsl::identifier(dsl::ascii::alpha);
  static constexpr auto value =
      lexy::callback<instr_t>([](auto a, auto o, auto b) -> instr_t {
        expr_t v{o, lexy::as_string<std::string>(a),
                 lexy::as_string<std::string>(b)};
        return instr_t{{}, v};
      });
};

struct instr {
  static constexpr auto rule = [] {
    auto a = dsl::peek(dsl::digit<>) >> dsl::p<number>;
    auto b = dsl::else_ >> dsl::p<expr>;
    return dsl::identifier(dsl::ascii::alpha) + LEXY_LIT(": ") + (a | b);
  }();
  static constexpr auto value =
      lexy::callback<std::pair<std::string, instr_t>>([](auto key, auto value) {
        return std::make_pair(lexy::as_string<std::string>(key), value);
      });
};

struct parser {
  static constexpr auto whitespace = dsl::newline;
  static constexpr auto rule =
      dsl::terminator(dsl::eof).opt_list(dsl::p<instr>);
  static constexpr auto value = lexy::as_list<context>;
};

}  // namespace grammar

AOC_IMPL(2022, 21) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto data) { return data.get("root"); };
  static constexpr auto part2 = [](auto data) { return data.part2(); };
};

AOC_TEST_CASE(152, 301, R"(root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32
)")

}  // namespace
