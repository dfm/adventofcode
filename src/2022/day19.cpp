#include <array>
#include <iostream>
#include <queue>
#include <unordered_map>
#include <unordered_set>

#include "aoc/aoc.hpp"

namespace {

using int_t = std::int64_t;
using cost_t = std::array<int_t, 3>;
struct blueprint {
  int_t id;
  int_t ore_bot_cost;
  int_t clay_bot_cost;
  std::pair<int_t, int_t> obsidian_bot_cost;
  std::pair<int_t, int_t> geode_bot_cost;
  std::tuple<int_t, int_t, int_t> max_cost;

  blueprint(int_t id, int_t ore_bot_cost, int_t clay_bot_cost,
            std::pair<int_t, int_t> obsidian_bot_cost,
            std::pair<int_t, int_t> geode_bot_cost)
      : id(id),
        ore_bot_cost(ore_bot_cost),
        clay_bot_cost(clay_bot_cost),
        obsidian_bot_cost(obsidian_bot_cost),
        geode_bot_cost(geode_bot_cost) {
    max_cost =
        std::make_tuple(std::max(std::max(std::max(ore_bot_cost, clay_bot_cost),
                                          obsidian_bot_cost.first),
                                 geode_bot_cost.first),
                        obsidian_bot_cost.second, geode_bot_cost.second);
  }
};

namespace grammar {

namespace dsl = lexy::dsl;

struct number {
  static constexpr auto rule = dsl::integer<int_t>;
  static constexpr auto value = lexy::as_integer<int_t>;
};

struct _blueprint {
  struct bot1 {
    static constexpr auto whitespace = dsl::ascii::space;
    static constexpr auto rule =
        LEXY_LIT("Each") + dsl::until(dsl::ascii::space) +
        LEXY_LIT("robot costs") + dsl::p<number> + dsl::until(dsl::lit_c<'.'>);
    static constexpr auto value = lexy::forward<int_t>;
  };

  struct bot2 {
    static constexpr auto whitespace = dsl::ascii::space;
    static constexpr auto rule =
        LEXY_LIT("Each") + dsl::until(dsl::ascii::space) +
        LEXY_LIT("robot costs") + dsl::p<number> +
        dsl::until(dsl::ascii::space) + LEXY_LIT("and") + dsl::p<number> +
        dsl::until(dsl::lit_c<'.'>);
    static constexpr auto value = lexy::construct<std::pair<int_t, int_t>>;
  };

  static constexpr auto whitespace = dsl::ascii::space;
  static constexpr auto rule = [] {
    auto id = LEXY_LIT("Blueprint") + dsl::p<number> + LEXY_LIT(":");
    return id + dsl::p<bot1> + dsl::p<bot1> + dsl::p<bot2> + dsl::p<bot2>;
  }();
  static constexpr auto value = lexy::construct<blueprint>;
};

struct parser {
  static constexpr auto rule =
      dsl::terminator(dsl::eof).opt_list(dsl::p<_blueprint>);
  static constexpr auto value = lexy::as_list<std::vector<blueprint>>;
};

}  // namespace grammar

struct graph {
  blueprint bp;

  using resource_type = std::tuple<int_t, int_t, int_t, int_t>;
  using distance_type = int_t;
  using node_type = std::tuple<int_t, resource_type, resource_type>;
  using neighbor_type = std::pair<node_type, distance_type>;

  graph(const blueprint& bp) : bp(bp) {}

  node_type root() const { return {0, {0, 0, 0, 0}, {1, 0, 0, 0}}; }

  template <int_t max_time>
  std::vector<neighbor_type> neighbors(const node_type& current) const {
    const auto& [time, materials, bots] = current;
    if (time >= max_time) {
      return {};
    }

    std::vector<neighbor_type> results;

    auto time_left = max_time - time;
    auto [r, c, b, g] = materials;
    auto [b_r, b_c, b_b, b_g] = bots;
    auto out_r = r + b_r;
    auto out_c = c + b_c;
    auto out_b = b + b_b;
    auto out_g = g + b_g;
    auto [cost_b_r, cost_b_c] = bp.obsidian_bot_cost;
    auto [cost_g_r, cost_g_b] = bp.geode_bot_cost;
    auto [max_r, max_c, max_b] = bp.max_cost;

    // Can we build geode bot? Let's do it if we can...
    if (r >= cost_g_r && b >= cost_g_b) {
      results.push_back({{time + 1,
                          {out_r - cost_g_r, out_c, out_b - cost_g_b, out_g},
                          {b_r, b_c, b_b, b_g + 1}},
                         1});
    } else {
      results.push_back({{time + 1, {out_r, out_c, out_b, out_g}, bots}, 1});

      if (r >= cost_b_r && c >= cost_b_c && b_b < max_b &&
          b_b * time_left + out_b < time_left * max_b) {
        results.push_back({{time + 1,
                            {out_r - cost_b_r, out_c - cost_b_c, out_b, out_g},
                            {b_r, b_c, b_b + 1, b_g}},
                           1});
      }

      if (r >= bp.clay_bot_cost && b_c < max_c &&
          b_c * time_left + out_c < time_left * max_c) {
        results.push_back({{time + 1,
                            {out_r - bp.clay_bot_cost, out_c, out_b, out_g},
                            {b_r, b_c + 1, b_b, b_g}},
                           1});
      }

      if (r >= bp.ore_bot_cost && b_r < max_r &&
          b_r * time_left + out_c < time_left * max_r) {
        results.push_back({{time + 1,
                            {out_r - bp.ore_bot_cost, out_c, out_b, out_g},
                            {b_r + 1, b_c, b_b, b_g}},
                           1});
      }
    }
    return results;
  }
};

template <int_t max_time>
int_t bfs(const graph& g) {
  std::unordered_set<graph::node_type> visited;
  std::queue<graph::node_type> todo;

  int_t best = 0;
  std::vector<int_t> cache(max_time + 1);
  std::fill(cache.begin(), cache.end(), 0);
  auto root = g.root();
  visited.insert(root);
  todo.push(root);
  while (!todo.empty()) {
    auto next = todo.front();
    todo.pop();
    auto num_geodes = std::get<3>(std::get<1>(next));
    if (num_geodes < cache[std::get<0>(next)]) continue;

    // auto [time, materials, bots] = next;
    // if (num_geodes > 0) {
    //   std::cout << best << " / " << time << ": ";
    //   std::cout << std::get<0>(materials) << " ";
    //   std::cout << std::get<1>(materials) << " ";
    //   std::cout << std::get<2>(materials) << " ";
    //   std::cout << std::get<3>(materials) << " / ";
    //   std::cout << std::get<0>(bots) << " ";
    //   std::cout << std::get<1>(bots) << " ";
    //   std::cout << std::get<2>(bots) << " ";
    //   std::cout << std::get<3>(bots) << std::endl;
    // }

    best = std::max(best, num_geodes);
    cache[std::get<0>(next)] = best;
    for (const auto& [node, _] : g.neighbors<max_time>(next)) {
      if (visited.contains(node)) continue;
      visited.insert(node);
      todo.push(node);
    }
  }

  return best;
}

AOC_IMPL(2022, 19) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto data) {
    int_t result = 0;
    for (const auto& bp : data) {
      graph g(bp);
      auto quality = bfs<24>(g);
      result += bp.id * quality;
    }
    return result;
  };
  static constexpr auto part2 = [](auto data) {
    int_t result = 1;
    for (size_t n = 0; n < std::min(data.size(), size_t(3)); ++n) {
      graph g(data[n]);
      auto quality = bfs<32>(g);
      result *= quality;
    }
    return result;
  };
};

AOC_TEST_CASE(
    33, 3348,
    R"(Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
)")

}  // namespace
