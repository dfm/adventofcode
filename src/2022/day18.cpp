#include <numeric>
#include <stack>
#include <unordered_map>
#include <unordered_set>

#include "aoc/aoc.hpp"

namespace {

using int_t = std::int64_t;
using coord_t = std::tuple<int_t, int_t, int_t>;

namespace grammar {

namespace dsl = lexy::dsl;

struct number {
  static constexpr auto rule = dsl::integer<int_t>;
  static constexpr auto value = lexy::as_integer<int_t>;
};

struct coord {
  static constexpr auto rule = dsl::p<number> + dsl::lit_c<','> +
                               dsl::p<number> + dsl::lit_c<','> +
                               dsl::p<number> + dsl::newline;
  static constexpr auto value = lexy::construct<coord_t>;
};

struct parser {
  static constexpr auto rule =
      dsl::terminator(dsl::eof).opt_list(dsl::p<coord>);
  static constexpr auto value = lexy::as_list<std::vector<coord_t>>;
};

}  // namespace grammar

template <size_t dim>
inline int_t surface_area(const std::unordered_set<coord_t>& points,
                          const coord_t& target) {
  int_t result = 0;
  coord_t query = target;
  std::get<dim>(query) += 1;
  if (!points.contains(query)) result++;
  std::get<dim>(query) -= 2;
  if (!points.contains(query)) result++;
  return result;
}

template <size_t dim>
inline int_t min_element(const std::vector<coord_t>& data) {
  const auto cmp = [](const auto& a, const auto& b) {
    return std::get<dim>(a) < std::get<dim>(b);
  };
  return std::get<dim>(*std::min_element(data.begin(), data.end(), cmp));
}

template <size_t dim>
inline int_t max_element(const std::vector<coord_t>& data) {
  const auto cmp = [](const auto& a, const auto& b) {
    return std::get<dim>(a) < std::get<dim>(b);
  };
  return std::get<dim>(*std::max_element(data.begin(), data.end(), cmp));
}

inline bool is_outside(const coord_t& min, const coord_t& max,
                       const coord_t& target) {
  auto [xmn, ymn, zmn] = min;
  auto [xmx, ymx, zmx] = max;
  auto [x, y, z] = target;
  return x < xmn || x > xmx || y < ymn || y > ymx || z < zmn || z > zmx;
}

inline bool can_reach_outside(std::unordered_map<coord_t, bool>& cache,
                              const std::unordered_set<coord_t>& points,
                              const coord_t& min, const coord_t& max,
                              const coord_t& target) {
  auto q = cache.find(target);
  if (q != cache.end()) return q->second;
  bool result = false;
  std::unordered_set<coord_t> seen;
  std::stack<coord_t> todo;
  todo.push(target);
  while (!todo.empty()) {
    auto c = todo.top();
    todo.pop();
    if (points.contains(c) || seen.contains(c)) continue;
    if (is_outside(min, max, c)) {
      result = true;
      break;
    }
    seen.insert(c);

    auto [x, y, z] = c;
    todo.push({x - 1, y, z});
    todo.push({x + 1, y, z});
    todo.push({x, y - 1, z});
    todo.push({x, y + 1, z});
    todo.push({x, y, z - 1});
    todo.push({x, y, z + 1});
  }
  cache.insert_or_assign(target, result);
  return result;
}

template <size_t dim>
inline int_t surface_area(std::unordered_map<coord_t, bool>& cache,
                          const std::unordered_set<coord_t>& points,
                          const coord_t& min, const coord_t& max,
                          const coord_t& target) {
  int_t result = 0;
  coord_t query = target;
  std::get<dim>(query) += 1;
  if (can_reach_outside(cache, points, min, max, query)) result++;
  std::get<dim>(query) -= 2;
  if (can_reach_outside(cache, points, min, max, query)) result++;
  return result;
}

AOC_IMPL(2022, 18) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto data) {
    std::unordered_set<coord_t> points(data.begin(), data.end());
    return std::accumulate(data.begin(), data.end(), int_t(0),
                           [&points](auto carry, const auto& c) {
                             return carry + surface_area<0>(points, c) +
                                    surface_area<1>(points, c) +
                                    surface_area<2>(points, c);
                           });
  };
  static constexpr auto part2 = [](auto data) {
    std::unordered_set<coord_t> points(data.begin(), data.end());
    std::unordered_map<coord_t, bool> cache;
    coord_t min{min_element<0>(data), min_element<1>(data),
                min_element<2>(data)};
    coord_t max{max_element<0>(data), max_element<1>(data),
                max_element<2>(data)};
    return std::accumulate(
        data.begin(), data.end(), int_t(0), [&](auto carry, const auto& c) {
          return carry + surface_area<0>(cache, points, min, max, c) +
                 surface_area<1>(cache, points, min, max, c) +
                 surface_area<2>(cache, points, min, max, c);
        });
  };
};

AOC_TEST_CASE(64, 58, R"(2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5
)")

}  // namespace
