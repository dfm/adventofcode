#include <iostream>
#include <range/v3/all.hpp>

#include "aoc/aoc.hpp"

namespace rv = ranges::views;

namespace {

using int_t = std::int64_t;
enum square { space = 0, open, wall };
enum turn { none = 0, left, right };
using step = std::pair<turn, int_t>;

struct coord_t {
  int_t x;
  int_t y;
  coord_t operator+(const coord_t& other) const {
    return {x + other.x, y + other.y};
  }

  coord_t& operator+=(const coord_t& other) {
    this->x += other.x;
    this->y += other.y;
    return *this;
  }
};
struct position_t {
  coord_t location;
  coord_t pointing;
};

struct grid {
  int_t _sector_size;
  int_t _width;
  int_t _height;
  std::vector<square> _data;
  std::vector<step> _path;
  position_t _pos;

  grid(const std::vector<std::vector<square>>& data,
       const std::vector<step>& path)
      : _path(path) {
    _width = ranges::max(data |
                         rv::transform([](const auto& x) { return x.size(); }));
    _height = data.size();
    _sector_size = _width / 4;
    for (const auto& row : data) {
      for (const auto& el : row) {
        _data.push_back(el);
      }
      for (int_t n = static_cast<int_t>(row.size()); n < _width; ++n) {
        _data.push_back(square::space);
      }
    }

    int_t x = 0;
    for (const auto& v : _data) {
      if (v != square::space) break;
      x++;
    }
    _pos = {{x, 0}, {1, 0}};
  }

  square get(const coord_t& c) const {
    const auto& [x, y] = c;
    if (x < 0 || _width <= x) return square::space;
    if (y < 0 || _height <= y) return square::space;
    return _data[y * _width + x];
  }

  coord_t wrap_part1(const position_t& pos) const {
    coord_t proposed = pos.location;
    coord_t delta = {-pos.pointing.x, -pos.pointing.y};
    while (get(proposed + delta) != square::space) {
      proposed += delta;
    }
    return proposed;
  }

  std::pair<int_t, coord_t> sector_coords(const coord_t& c) const {
    auto [x, y] = c;
    auto xs = x / _sector_size;
    auto ys = y / _sector_size;
    int_t sector = 0;
    if (xs == 2 && ys == 0) {
      sector = 1;
    } else if (xs == 0 && ys == 1) {
      sector = 2;
    } else if (xs == 1 && ys == 1) {
      sector = 3;
    } else if (xs == 2 && ys == 1) {
      sector = 4;
    } else if (xs == 2 && ys == 2) {
      sector = 5;
    } else if (xs == 3 && ys == 2) {
      sector = 5;
    }
    return {sector, {x % _sector_size, y % _sector_size}};
  }

  // coord_t wrap_part2(const position_t& pos) const {
  // }

  void move(const step& instr, auto wrapper) {
    const auto& [direction, amount] = instr;
    if (direction == turn::left) {
      _pos.pointing = {_pos.pointing.y, -_pos.pointing.x};
    } else if (direction == turn::right) {
      _pos.pointing = {-_pos.pointing.y, _pos.pointing.x};
    } else {
      for (int_t n = 0; n < amount; ++n) {
        auto [x, y] = _pos.location + _pos.pointing;
        const auto& value = get({x, y});
        if (value == square::wall) {
          return;
        } else if (value == square::space) {
          auto proposed = wrapper(_pos);
          if (get(proposed) != square::wall) {
            _pos.location = proposed;
          }
        } else {
          _pos.location = {x, y};
        }
      }
    }
  }

  int_t run(auto wrapper) {
    for (const auto& s : _path) {
      // std::cout << "Step: " << s.first << " " << s.second << std::endl;
      move(s, wrapper);
      // std::cout << _pos.location.x << ", " << _pos.location.y << std::endl;
      // std::cout << _pos.pointing.x << ", " << _pos.pointing.y << std::endl;
    }
    int_t result = 1000 * (_pos.location.y + 1) + 4 * (_pos.location.x + 1);
    if (_pos.pointing.y == 1) {
      result += 1;
    } else if (_pos.pointing.x == -1) {
      result += 2;
    } else if (_pos.pointing.y == -1) {
      result += 3;
    }

    return result;
  }

  void show() const {
    for (int_t y = 0; y < _height; ++y) {
      for (int_t x = 0; x < _width; ++x) {
        std::cout << _data[y * _width + x];
      }
      std::cout << std::endl;
    }
    std::cout << std::endl;

    for (const auto& [t, a] : _path) {
      std::cout << t << ": " << a << std::endl;
    }
  }
};

namespace grammar {

namespace dsl = lexy::dsl;

struct map_square {
  static constexpr auto rule =
      dsl::capture(dsl::ascii::punct / dsl::ascii::space);
  static constexpr auto value = lexy::callback<square>([](auto lex) {
    auto c = static_cast<char>(*lex.begin());
    if (c == ' ') {
      return square::space;
    } else if (c == '.') {
      return square::open;
    } else if (c == '#') {
      return square::wall;
    }
    throw std::runtime_error("Invalid token");
  });
};

struct map_row : lexy::token_production {
  static constexpr auto rule =
      dsl::terminator(dsl::newline).opt_list(dsl::p<map_square>);
  static constexpr auto value = lexy::as_list<std::vector<square>>;
};

struct map {
  static constexpr auto rule =
      dsl::terminator(dsl::newline).opt_list(dsl::p<map_row>);
  static constexpr auto value = lexy::as_list<std::vector<std::vector<square>>>;
};

struct path_amount {
  static constexpr auto rule = dsl::integer<int_t>;
  static constexpr auto value = lexy::callback<step>([](const auto& v) -> step {
    return {turn::none, lexy::as_integer<int_t>(v)};
  });
};

struct path_turn {
  static constexpr auto direction =
      lexy::symbol_table<turn>.map<LEXY_SYMBOL("L")>(turn::left).map<LEXY_SYMBOL("R")>(turn::right);
  static constexpr auto rule = [] {
    auto cond = dsl::peek(dsl::lit_c<'L'>) | dsl::peek(dsl::lit_c<'R'>);
    auto value = dsl::symbol<direction>(dsl::identifier(dsl::ascii::alpha));
    return cond >> value;
  }();
  static constexpr auto value = lexy::callback<step>([](const auto& v) -> step {
    return {v, 0};
  });
};

struct path_step {
  static constexpr auto rule =
      dsl::p<path_turn> | (dsl::else_ >> dsl::p<path_amount>);
  static constexpr auto value = lexy::forward<step>;
};

struct path {
  static constexpr auto rule =
      dsl::terminator(dsl::newline).opt_list(dsl::p<path_step>);
  static constexpr auto value = lexy::as_list<std::vector<step>>;
};

struct parser {
  static constexpr auto rule = dsl::p<map> + dsl::p<path>;
  static constexpr auto value = lexy::construct<grid>;
};

}  // namespace grammar

AOC_IMPL(2022, 22) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto grid) {
    return grid.run([&grid](const auto& pos) { return grid.wrap_part1(pos); });
  };
  static constexpr auto part2 = [](auto /* grid */) {
    return 0;
    // return grid.run([&grid](const auto& pos) { return grid.wrap_part2(pos);
    // });
  };
};

AOC_TEST_CASE(6032, 0 /*5031*/, R"(        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5
)")

}  // namespace
