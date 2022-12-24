#include <array>
#include <iostream>
#include <range/v3/all.hpp>
#include <unordered_map>

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

  bool operator==(const coord_t& other) const {
    return x == other.x && y == other.y;
  }
};

struct hash_coord {
  std::size_t operator()(const coord_t& key) const {
    std::size_t h = 0;
    aoc::hash_combine(h, key.x, key.y);
    return h;
  }
};

struct position_t {
  coord_t location;
  coord_t pointing;
};

enum direction_t { north = 0, east, south, west };
using edge_t = std::vector<std::pair<coord_t, direction_t>>;
using edges_t = std::unordered_map<coord_t, edge_t, hash_coord>;

inline std::pair<coord_t, edge_t> make_edge(auto x, auto y, auto x1, auto y1,
                                            auto d1, auto x2, auto y2, auto d2,
                                            auto x3, auto y3, auto d3, auto x4,
                                            auto y4, auto d4) {
  return {{static_cast<int_t>(x), static_cast<int_t>(y)},
          {{{static_cast<int_t>(x1), static_cast<int_t>(y1)}, d1},
           {{static_cast<int_t>(x2), static_cast<int_t>(y2)}, d2},
           {{static_cast<int_t>(x3), static_cast<int_t>(y3)}, d3},
           {{static_cast<int_t>(x4), static_cast<int_t>(y4)}, d4}}};
}

struct test_data {
  static edges_t edges() {
    edges_t result{
        make_edge(2, 0, 0, 1, north, 3, 2, east, 2, 1, north, 1, 1, north),
        make_edge(0, 1, 2, 0, north, 1, 1, east, 2, 2, south, 3, 2, south),
        make_edge(1, 1, 2, 0, west, 2, 1, west, 2, 2, west, 0, 1, east),
        make_edge(2, 1, 2, 0, south, 3, 2, north, 2, 2, north, 1, 1, east),
        make_edge(2, 2, 2, 1, south, 3, 2, west, 0, 1, south, 1, 1, south),
        make_edge(3, 2, 2, 1, east, 2, 0, east, 0, 1, east, 2, 2, east),
    };
    return result;
  }
};

struct real_data {
  static edges_t edges() {
    edges_t result{
        make_edge(1, 0, 0, 3, west, 2, 0, west, 1, 1, north, 0, 2, west),
        make_edge(2, 0, 0, 3, south, 1, 2, east, 1, 1, east, 1, 0, east),
        make_edge(1, 1, 1, 0, south, 2, 0, south, 1, 2, north, 0, 2, north),
        make_edge(0, 2, 1, 1, west, 1, 2, west, 0, 3, north, 1, 0, west),
        make_edge(1, 2, 1, 1, south, 2, 0, east, 0, 3, east, 0, 2, east),
        make_edge(0, 3, 0, 2, south, 1, 2, south, 2, 0, north, 1, 0, north),
    };
    return result;
  }
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
    _sector_size = std::max(_width, _height) / 4;
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

  position_t wrap_part1(const position_t& pos) const {
    coord_t proposed = pos.location;
    coord_t delta = {-pos.pointing.x, -pos.pointing.y};
    while (get(proposed + delta) != square::space) {
      proposed += delta;
    }
    return {proposed, pos.pointing};
  }

  // ***** Part 2: *****
  inline int_t wrap_into_range(int_t n) const {
    while (n < 0) n += 4;
    return n % 4;
  }

  inline coord_t rotate(const coord_t& c, int_t n) const {
    auto [x, y] = c;
    n = wrap_into_range(n);
    int_t size = _sector_size - 1;
    if (n == 0) {
      return {x, y};
    } else if (n == 1) {
      return {y, size - x};
    } else if (n == 2) {
      return {size - x, size - y};
    }
    return {size - y, x};
  }

  inline coord_t transform_location(const coord_t& c, int_t from,
                                    int_t to) const {
    auto [x, y] = rotate(c, from);
    to = wrap_into_range(to - from);

    // We've rotated so that we're always leaving from north now...
    coord_t result;
    int_t size = _sector_size - 1;
    if (to == static_cast<int_t>(direction_t::north)) {
      result = {size - x, y};
    } else if (to == static_cast<int_t>(direction_t::east)) {
      result = {size - y, size - x};
    } else if (to == static_cast<int_t>(direction_t::south)) {
      result = {x, size - y};
    } else {
      result = {y, x};
    }

    // Rotate back
    return rotate(result, -from);
  }

  position_t wrap_part2(const edges_t& edge_map, const position_t& pos) const {
    auto [x, y] = pos.location;
    auto [px, py] = pos.pointing;
    coord_t from_sector_id{x / _sector_size, y / _sector_size};
    coord_t from_sector_coords{x % _sector_size, y % _sector_size};

    direction_t from;
    if (py == -1)
      from = north;
    else if (px == 1)
      from = east;
    else if (py == 1)
      from = south;
    else
      from = west;

    auto q = edge_map.find(from_sector_id);
    if (q == edge_map.end()) throw std::runtime_error("Invalid sector id");
    const auto& [to_sector_id, to] = q->second[static_cast<size_t>(from)];
    const auto& [x2, y2] = transform_location(from_sector_coords, from, to);

    coord_t final_pointing;
    if (to == north)
      final_pointing = {0, 1};
    else if (to == east)
      final_pointing = {-1, 0};
    else if (to == south)
      final_pointing = {0, -1};
    else
      final_pointing = {1, 0};

    return {{to_sector_id.x * _sector_size + x2,
             to_sector_id.y * _sector_size + y2},
            final_pointing};
  }

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
          if (get(proposed.location) != square::wall) {
            _pos = proposed;
          } else {
            return;
          }
        } else {
          _pos.location = {x, y};
        }
      }
    }
  }

  int_t run(auto wrapper) {
    for (const auto& s : _path) {
      move(s, wrapper);
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

  [[maybe_unused]] void show() const {
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
  static constexpr auto part2 = [](auto grid) {
    const auto& edges =
        grid._sector_size == 4 ? test_data::edges() : real_data::edges();
    return grid.run(
        [&](const auto& pos) { return grid.wrap_part2(edges, pos); });
  };
};

AOC_TEST_CASE(6032, 5031, R"(        ...#
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
