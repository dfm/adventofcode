#include "aoc/aoc.hpp"

namespace {

using idx_t = std::size_t;
using value_t = std::int64_t;
using node_t = std::tuple<value_t, idx_t, idx_t>;

namespace grammar {

namespace dsl = lexy::dsl;

struct number {
  static constexpr auto rule = dsl::sign + dsl::integer<value_t>;
  static constexpr auto value = lexy::as_integer<value_t>;
};

struct parser {
  static constexpr auto whitespace = dsl::newline;
  static constexpr auto rule =
      dsl::terminator(dsl::eof).opt_list(dsl::p<number>);
  static constexpr auto value = lexy::as_list<std::vector<value_t>>;
};

}  // namespace grammar

template <typename T>
inline T sign(const T& x) {
  return T(x > 0) - T(x < 0);
}

struct linked_list {
  idx_t root;
  std::vector<node_t> _data;

  linked_list(const std::vector<value_t>& v) {
    root = idx_t(0);
    _data.reserve(v.size());
    _data.push_back({v[0], v.size() - 1, 1});
    for (idx_t n = 1; n < v.size() - 1; ++n) {
      _data.push_back({v[n], n - 1, n + 1});
    }
    _data.push_back({v[v.size() - 1], v.size() - 2, 0});
  }

  std::vector<value_t> to_vector() const {
    std::vector<value_t> result;

    idx_t start = root;
    for (idx_t i = 0; i < _data.size(); ++i) {
      if (std::get<0>(_data[i]) == 0) {
        start = i;
        break;
      }
    }

    idx_t n = start;
    do {
      result.push_back(std::get<0>(_data[n]));
      n = std::get<2>(_data[n]);
    } while (n != start);
    return result;
  }

  void move(idx_t n) {
    auto [delta, left, right] = _data[n];
    if (delta == 0) return;
    if (n == root) root = right;

    // Remove the node from its old position
    std::get<2>(_data[left]) = right;
    std::get<1>(_data[right]) = left;

    // Find its new coordinates
    idx_t new_idx = n;
    idx_t num_moves = static_cast<idx_t>(std::abs(delta) % (_data.size() - 1));
    if (delta < 0) {
      for (idx_t d = 0; d <= num_moves; ++d) {
        new_idx = std::get<1>(_data[new_idx]);
      }
    } else {
      for (idx_t d = 0; d < num_moves; ++d) {
        new_idx = std::get<2>(_data[new_idx]);
      }
    }

    // Insert it in the new position
    auto new_right = std::get<2>(_data[new_idx]);
    std::get<2>(_data[new_idx]) = n;
    std::get<1>(_data[n]) = new_idx;
    std::get<2>(_data[n]) = new_right;
    std::get<1>(_data[new_right]) = n;
  }

  void mix(idx_t count = 1) {
    for (idx_t i = 0; i < count; ++i) {
      for (idx_t n = 0; n < _data.size(); ++n) {
        move(n);
      }
    }
  }

  value_t coords() const {
    auto v = to_vector();
    return v[1000 % v.size()] + v[2000 % v.size()] + v[3000 % v.size()];
  }
};

AOC_IMPL(2022, 20) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto data) {
    linked_list list(data);
    list.mix();
    return list.coords();
  };
  static constexpr auto part2 = [](auto data) {
    for (auto& v : data) {
      v *= 811589153;
    }
    linked_list list(data);
    list.mix(10);
    return list.coords();
  };
};

AOC_TEST_CASE(3, 1623178306, R"(1
2
-3
3
-2
0
4
)")

}  // namespace
