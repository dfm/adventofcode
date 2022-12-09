#ifndef AOC_ALGORITHMS_HPP
#define AOC_ALGORITHMS_HPP

#include <cstddef>
#include <cstdint>
#include <functional>

namespace aoc {

template <typename I>
class range {
 public:
  class iterator {
    friend class range;

   public:
    long int operator*() const { return i_; }
    const iterator &operator++() {
      i_ += delta_;
      return *this;
    }
    iterator operator++(int) {
      iterator copy(*this);
      i_ += delta_;
      return copy;
    }

    bool operator==(const iterator &other) const { return i_ == other.i_; }
    bool operator!=(const iterator &other) const { return i_ != other.i_; }

   protected:
    iterator(I start, I delta) : i_(start), delta_(delta) {}

   private:
    I i_, delta_;
  };

  iterator begin() const { return begin_; }
  iterator end() const { return end_; }
  range(I begin, I end) : begin_(begin, I(1)), end_(end, I(1)) {}
  range(I begin, I end, I delta) : begin_(begin, delta), end_(end, delta) {}

 private:
  iterator begin_;
  iterator end_;
};

// MAGIC: from boost
// ref:
// https://stackoverflow.com/questions/2590677/how-do-i-combine-hash-values-in-c0x
inline void hash_combine(size_t &) {}
template <typename T, typename... Rest>
inline void hash_combine(size_t &seed, const T &v, Rest... rest) {
  std::hash<T> hasher;
  seed ^= hasher(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
  hash_combine(seed, rest...);
}

}  // namespace aoc

#endif
