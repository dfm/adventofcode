#include <array>
#include <unordered_set>

#include "aoc/aoc.hpp"

namespace {

typedef std::pair<std::int32_t, std::int32_t> vec_t;
struct instruction_t {
  vec_t dir;
  std::int32_t distance;
};

// MAGIC: from boost
// ref:
// https://stackoverflow.com/questions/2590677/how-do-i-combine-hash-values-in-c0x
inline void hash_combine(size_t&) {}
template <typename T, typename... Rest>
inline void hash_combine(size_t& seed, const T& v, Rest... rest) {
  std::hash<T> hasher;
  seed ^= hasher(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
  hash_combine(seed, rest...);
}

struct vec_hash {
  size_t operator()(const vec_t& v) const {
    size_t h = 0;
    hash_combine(h, v.first, v.second);
    return h;
  }
};

namespace grammar {

namespace dsl = lexy::dsl;

struct direction {
  static constexpr auto directions =
      lexy::symbol_table<vec_t>
        .map<LEXY_SYMBOL("L")>(vec_t{-1, 0})
        .map<LEXY_SYMBOL("R")>(vec_t{1, 0})
        .map<LEXY_SYMBOL("U")>(vec_t{0, 1})
        .map<LEXY_SYMBOL("D")>(vec_t{0, -1});
  static constexpr auto rule = [] {
    auto name = dsl::identifier(dsl::ascii::alpha);
    auto reference = dsl::symbol<directions>(name);
    return reference;
  }();
  static constexpr auto value = lexy::forward<vec_t>;
};

struct instruction {
  static constexpr auto rule = dsl::p<direction> + dsl::ascii::space +
                               dsl::integer<std::int32_t> + dsl::ascii::newline;
  static constexpr auto value = lexy::construct<instruction_t>;
};

struct parser {
  static constexpr auto rule =
      dsl::terminator(dsl::eof).opt_list(dsl::p<instruction>);
  static constexpr auto value = lexy::as_list<std::vector<instruction_t>>;
};

}  // namespace grammar

inline std::int32_t sign(std::int32_t x) { return (x > 0) - (x < 0); }

inline vec_t step(const vec_t& step, vec_t& head, vec_t& tail) {
  vec_t delta{head.first - tail.first, head.second - tail.second};
  head.first += step.first;
  head.second += step.second;

  auto dx = head.first - tail.first, dy = head.second - tail.second;
  if ((dx == -1 || dx == 0 || dx == 1) && (dy == -1 || dy == 0 || dy == 1))
    return vec_t{0, 0};

  return vec_t{sign(dx), sign(dy)};
}

template <size_t count>
struct problem_t {
  std::array<vec_t, count> knots;
  const vec_t& tail() const { return knots[count - 1]; }
  void move(const vec_t& v) {
    vec_t delta = v;
    for (size_t k = 0; k < count - 1; ++k) {
      delta = step(delta, knots[k], knots[k + 1]);
      if (delta.first == 0 && delta.second == 0) return;
    }
    knots[count - 1].first += delta.first;
    knots[count - 1].second += delta.second;
  }
  size_t run(const std::vector<instruction_t>& data) {
    std::unordered_set<vec_t, vec_hash> visited;
    for (const auto& instruction : data) {
      for (std::int64_t i = 0; i < instruction.distance; ++i) {
        move(instruction.dir);
        visited.insert(tail());
      }
    }
    return visited.size();
  }
};

AOC_IMPL(2022, 9) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto data) {
    problem_t<2> p;
    return p.run(data);
  };
  static constexpr auto part2 = [](auto data) {
    problem_t<10> p;
    return p.run(data);
  };
};

AOC_TEST_CASE(13, 1, R"(R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
)")

AOC_TEST_CASE(88, 36, R"(R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
)")

}  // namespace
