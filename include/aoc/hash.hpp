#ifndef AOC_HASH_HPP
#define AOC_HASH_HPP

#include <cstddef>
#include <tuple>
#include <utility>

namespace aoc {

// MAGIC: from boost
// ref:
// https://stackoverflow.com/questions/2590677/how-do-i-combine-hash-values-in-c0x
inline void hash_combine(std::size_t&) {}

template <typename T, typename... Rest>
inline void hash_combine(std::size_t& seed, const T& v, Rest... rest) {
  std::hash<T> hasher;
  seed ^= hasher(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
  hash_combine(seed, rest...);
}

namespace detail {

template <typename... T, std::size_t... Is>
inline void hash_combine_impl(std::size_t& seed, const std::tuple<T...>& v,
                              std::index_sequence<Is...>) {
  hash_combine(seed, std::get<Is>(v)...);
}

}  // namespace detail

template <typename... T>
inline void hash_combine(std::size_t& seed, const std::tuple<T...>& v) {
  detail::hash_combine_impl(
      seed, v,
      std::make_index_sequence<std::tuple_size<std::tuple<T...>>::value>{});
}

struct pair_hash {
  template <typename A, typename B>
  std::size_t operator()(const std::pair<A, B>& v) const {
    std::size_t h = 0;
    hash_combine(h, v.first, v.second);
    return h;
  }
};

}  // namespace aoc

//
template <typename A, typename B>
struct std::hash<std::pair<A, B>> {
  std::size_t operator()(const std::pair<A, B>& key) const {
    std::size_t h = 0;
    aoc::hash_combine(h, key.first, key.second);
    return h;
  }
};

template <typename... Args>
struct std::hash<std::tuple<Args...>> {
  std::size_t operator()(const std::tuple<Args...>& key) const {
    std::size_t h = 0;
    aoc::hash_combine(h, key);
    return h;
  }
};

#endif
