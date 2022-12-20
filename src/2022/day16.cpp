#include <unordered_map>
#include <unordered_set>

#include "aoc/aoc.hpp"

namespace {

using int_t = std::uint64_t;
using edge_t = std::pair<std::string, int_t>;
struct node {
  std::string name;
  int_t rate;
  std::vector<edge_t> edges;
};

namespace grammar {

namespace dsl = lexy::dsl;

struct name {
  static constexpr auto rule = dsl::identifier(dsl::ascii::alpha);
  static constexpr auto value = lexy::as_string<std::string>;
};

struct number {
  static constexpr auto rule = dsl::integer<int_t>;
  static constexpr auto value = lexy::as_integer<int_t>;
};

struct edge {
  static constexpr auto rule = dsl::identifier(dsl::ascii::alpha);
  static constexpr auto value = lexy::callback<edge_t>([](auto v) {
    return edge_t{lexy::as_string<std::string>(v), 1};
  });
};

struct edges {
  static constexpr auto rule =
      dsl::terminator(dsl::newline)
          .opt_list(dsl::p<edge>, dsl::sep(LEXY_LIT(", ")));
  static constexpr auto value = lexy::as_list<std::vector<edge_t>>;
};

struct line {
  static constexpr auto rule = [] {
    auto name_ = LEXY_LIT("Valve ") + dsl::p<name>;
    auto rate_ = LEXY_LIT(" has flow rate=") + dsl::p<number> + LEXY_LIT("; ");
    auto edges_ = ((LEXY_LIT("tunnels") >> LEXY_LIT(" lead to valves ")) |
                   (LEXY_LIT("tunnel") >> LEXY_LIT(" leads to valve "))) +
                  dsl::p<edges>;
    return name_ + rate_ + edges_;
  }();
  static constexpr auto value = lexy::construct<node>;
};

struct parser {
  static constexpr auto rule = dsl::terminator(dsl::eof).opt_list(dsl::p<line>);
  static constexpr auto value = lexy::as_list<std::vector<node>>;
};

}  // namespace grammar

struct graph {
  std::unordered_map<std::string, node> _nodes;
  std::vector<int_t> _rates;
  std::vector<int_t> _distances;
  size_t _num_nodes;

  using distance_type = int_t;
  using node_type = std::string;
  using neighbor_type = edge_t;

  std::vector<neighbor_type> neighbors(const node_type& current) const {
    std::vector<neighbor_type> neighbors;
    auto search = _nodes.find(current);
    if (search == _nodes.end()) return neighbors;
    auto edges = search->second.edges;
    std::copy(edges.begin(), edges.end(), std::back_inserter(neighbors));
    return neighbors;
  }

  graph(auto data) {
    std::sort(data.begin(), data.end(),
              [](const auto& a, const auto& b) { return a.name < b.name; });

    for (const auto& n : data) {
      _nodes.insert({n.name, n});
    }

    // Refactor the graph to remove the zero rate nodes - we never stop there
    for (const auto& initial : _nodes) {
      auto& n = initial.second;
      if (n.rate > 0 || n.name == "AA") continue;
      for (const auto& other : n.edges) {
        auto& node = _nodes[other.first];
        auto& edges = node.edges;
        auto current_edge =
            *std::find_if(edges.begin(), edges.end(),
                          [&](const auto& v) { return v.first == n.name; });
        edges.erase(std::remove(edges.begin(), edges.end(), current_edge),
                    edges.end());
        for (const auto& new_edge : n.edges) {
          if (new_edge.first == other.first) continue;
          edges.push_back(
              {new_edge.first, new_edge.second + current_edge.second});
        }
      }
    }

    for (const auto& n : data) {
      if (n.rate > 0 || n.name == "AA") continue;
      _nodes.erase(n.name);
    }

    // Map the node names to integers
    size_t idx = 0;
    std::unordered_map<std::string, size_t> node_map;
    for (const auto& n : data) {
      if (n.rate > 0 || n.name == "AA") node_map.insert({n.name, idx++});
    }
    _num_nodes = idx;
    _rates.resize(_num_nodes);
    _distances.resize(_num_nodes * _num_nodes);
    std::fill(_distances.begin(), _distances.end(), int_t(0));

    for (const auto& n1 : _nodes) {
      _rates[node_map[n1.second.name]] = n1.second.rate;
      auto values = aoc::graph::shortest_path(*this, n1.second.name).run();
      for (const auto& n2 : values) {
        if (n1.second.name != n2.first) {
          auto from = node_map[n1.second.name];
          auto to = node_map[n2.first];
          _distances[from * _num_nodes + to] = n2.second;
        }
      }
    }
  }
};

template <int_t max_time>
std::vector<std::pair<int_t, std::uint32_t>> search(
    const std::vector<int_t>& rates, const std::vector<int_t>& distances,
    int_t time, int_t value, int_t flow, size_t current, std::uint32_t open) {
  if (time > max_time) {
    return {};
  }

  const size_t size = rates.size();
  std::vector<std::pair<int_t, std::uint32_t>> result;
  result.push_back({value + (max_time - time) * flow, open});
  for (size_t n = 0; n < size; ++n) {
    auto q = 1 << n;
    if (open & q) continue;
    auto dt = distances[current * size + n] + 1;
    auto df = rates[n];
    auto next = search<max_time>(rates, distances, time + dt, value + dt * flow,
                                 flow + df, n, open | q);
    result.insert(result.end(), next.begin(), next.end());
  }
  return result;
}

int_t find_best(const std::vector<std::pair<int_t, std::uint32_t>>& v) {
  return std::max_element(
             v.begin(), v.end(),
             [](const auto& a, const auto& b) { return a.first < b.first; })
      ->first;
}

template <int_t max_time>
int_t search(const std::vector<int_t>& rates,
             const std::vector<int_t>& distances) {
  auto result = search<max_time>(rates, distances, 0, 0, 0, 0, 0);
  return find_best(result);
}

template <int_t max_time>
int_t search2(const std::vector<int_t>& rates,
              const std::vector<int_t>& distances) {
  int_t result = 0;
  auto paths = search<max_time>(rates, distances, 0, 0, 0, 0, 0);
  std::sort(paths.begin(), paths.end(),
            [](const auto& a, const auto& b) { return a.first > b.first; });
  auto num_paths = paths.size();
  for (size_t i = 0; i < num_paths; ++i) {
    auto [v1, p1] = paths[i];
    for (size_t j = i + 1; j < num_paths; ++j) {
      auto [v2, p2] = paths[j];
      if (v1 + v2 <= result) break;
      if (p1 & p2) continue;
      result = std::max(result, v1 + v2);
    }
  }
  return result;
}

AOC_IMPL(2022, 16) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto data) {
    auto g = graph(data);
    return search<30>(g._rates, g._distances);
  };
  static constexpr auto part2 = [](auto data) {
    auto g = graph(data);
    return search2<26>(g._rates, g._distances);
  };
};

AOC_TEST_CASE(1651, 1707,
              R"(Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
)")

}  // namespace
