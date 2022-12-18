#include <array>
#include <iostream>
#include <unordered_set>

#include "aoc/aoc.hpp"

namespace {

enum direction_t { left = 0, right };
using int_t = std::int64_t;
using coord_t = std::pair<int_t, int_t>;
using board_t = std::unordered_set<coord_t, aoc::pair_hash>;

struct piece_t {
  size_t width;
  size_t height;
  std::vector<coord_t> _data;
  piece_t(std::vector<coord_t>&& data) : _data(data) {
    width = std::max_element(
                _data.begin(), _data.end(),
                [](const auto& a, const auto& b) { return a.first < b.first; })
                ->first +
            1;
    height = std::max_element(_data.begin(), _data.end(),
                              [](const auto& a, const auto& b) {
                                return a.second < b.second;
                              })
                 ->second +
             1;
  }

  inline bool check_collides(const board_t& map, const coord_t& origin) const {
    auto [x0, y0] = origin;
    if (x0 < 0 || y0 < 0) return true;
    if (x0 + static_cast<int_t>(width) > 7) return true;
    for (auto [x, y] : _data) {
      if (map.contains({x0 + x, y0 + y})) return true;
    }
    return false;
  }
};

static const std::array<piece_t, 5> pieces{
    piece_t({{0, 0}, {1, 0}, {2, 0}, {3, 0}}),
    piece_t({{1, 0}, {0, 1}, {1, 1}, {2, 1}, {1, 2}}),
    piece_t({{0, 0}, {1, 0}, {2, 0}, {2, 1}, {2, 2}}),
    piece_t({{0, 0}, {0, 1}, {0, 2}, {0, 3}}),
    piece_t({{0, 0}, {1, 0}, {0, 1}, {1, 1}})};

struct game_t {
  size_t tick;
  size_t piece_idx;
  int_t top;
  const std::vector<direction_t> directions;
  board_t board;

  game_t(std::vector<direction_t>& data)
      : tick(0), piece_idx(0), top(0), directions(data) {}

  inline void drop() {
    int_t x = 2;
    int_t y = top + 3;
    const auto size = directions.size();
    const auto& piece = pieces[piece_idx];
    for (;; ++tick) {
      // std::cout << piece_idx << " -> " << x << ", " << y << std::endl;
      auto d = directions[tick % size];
      auto xp = d == direction_t::left ? x - 1 : x + 1;
      x = piece.check_collides(board, {xp, y}) ? x : xp;
      if (piece.check_collides(board, {x, y - 1})) {
        piece_idx = (piece_idx + 1) % pieces.size();
        top = std::max(top, y + static_cast<int_t>(piece.height));
        // std::cout << "top = " << top << std::endl;
        tick++;
        for (auto [x_, y_] : piece._data) {
          board.insert({x + x_, y + y_});
        }
        break;
      }
      y--;
    }
  }

  void show() const {
    for (int_t y = top - 1; y >= 0; --y) {
      for (int_t x = 0; x < 7; ++x) {
        if (board.contains({x, y})) {
          std::cout << "#";
        } else {
          std::cout << ".";
        }
      }
      std::cout << std::endl;
    }
    std::cout << std::endl;
  }
};

namespace grammar {

namespace dsl = lexy::dsl;

struct direction {
  struct _left {
    static constexpr auto rule = dsl::lit_c<'<'>;
    static constexpr auto value =
        lexy::callback<direction_t>([]() { return direction_t::left; });
  };

  struct _right {
    static constexpr auto rule = dsl::lit_c<'>'>;
    static constexpr auto value =
        lexy::callback<direction_t>([]() { return direction_t::right; });
  };
  static constexpr auto rule = dsl::p<_left> | dsl::p<_right>;
  static constexpr auto value = lexy::forward<direction_t>;
};

struct parser {
  static constexpr auto whitespace = dsl::ascii::newline;
  static constexpr auto rule =
      dsl::terminator(dsl::eof).opt_list(dsl::p<direction>);
  static constexpr auto value = lexy::as_list<std::vector<direction_t>>;
};

}  // namespace grammar

AOC_IMPL(2022, 17) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto data) {
    game_t game(data);
    for (size_t n = 0; n < 2022; ++n) {
      game.drop();
    }
    return game.top;
  };
  static constexpr auto part2 = [](auto data) {
    game_t game(data);
    std::unordered_set<std::pair<int_t, int_t>, aoc::pair_hash> seen;
    int_t start = 0;
    int_t start_value = 0;
    int_t period = 0;
    std::vector<int_t> trace;
    for (size_t n = 0;; ++n) {
      std::pair<int_t, int_t> q{game.piece_idx,
                                game.tick % game.directions.size()};
      if (seen.contains(q)) {
        if (start == 0) {
          start = n;
          seen.clear();
          start_value = game.top;
        } else {
          period = n - start;
          break;
        }
      }
      seen.insert(q);
      game.drop();
      if (start != 0) {
        trace.push_back(game.top - start_value);
      }
    }
    int_t iters = (1000000000000 - start) / period;
    int_t remain = (1000000000000 - start) % period;
    int_t result = start_value + iters * trace[trace.size() - 1];
    if (remain > 0) {
      result += trace[remain - 1];
    }
    return result;
  };
};

AOC_TEST_CASE(3068, 1514285714288, R"(>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>
)")

}  // namespace
