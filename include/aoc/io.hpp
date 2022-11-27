#ifndef AOC_IO_HPP
#define AOC_IO_HPP

#include <algorithm>
#include <functional>
#include <istream>
#include <iterator>
#include <ostream>
#include <string>
#include <vector>

namespace aoc {

template <typename T>
struct stream_iterator {
  stream_iterator(std::istream &in) : d_in(in) {}
  std::istream &d_in;
  std::istream_iterator<T> begin() { return std::istream_iterator<T>(d_in); }
  std::istream_iterator<T> end() { return std::istream_iterator<T>(); }
};

template <typename T>
std::istream_iterator<T> begin(stream_iterator<T> r) {
  return r.begin();
}

template <typename T>
std::istream_iterator<T> end(stream_iterator<T> r) {
  return r.end();
}

namespace io {

template <typename T>
struct wrapped {
  typedef std::function<T(std::istream &)> type;
};

template <typename In>
struct harness {
  typedef In input_t;

  template <typename Out>
  inline static typename wrapped<Out>::type wrap(std::function<Out(input_t)>);
};

template <>
struct harness<std::istream &> {
  template <typename Out>
  inline static typename wrapped<Out>::type wrap(
      std::function<Out(std::istream &)> func) {
    return [func](std::istream &in) { return func(in); };
  }
};

template <typename T>
struct harness<std::istream_iterator<T> &> {
  typedef std::istream_iterator<T> &input_t;

  template <typename Out>
  inline static typename wrapped<Out>::type wrap(
      std::function<Out(input_t)> func) {
    return [func](std::istream &in) {
      std::istream_iterator<T> stream(in);
      return func(stream);
    };
  }
};

template <typename T>
struct harness<stream_iterator<T> &> {
  typedef stream_iterator<T> &input_t;

  template <typename Out>
  inline static typename wrapped<Out>::type wrap(
      std::function<Out(input_t)> func) {
    return [func](std::istream &in) {
      stream_iterator<T> stream(in);
      return func(stream);
    };
  }
};

template <typename T>
struct harness<const std::vector<T> &> {
  typedef const std::vector<T> &input_t;

  template <typename Out>
  inline static typename wrapped<Out>::type wrap(
      std::function<Out(input_t)> func) {
    return [func](std::istream &in) {
      std::vector<T> vec;
      std::istream_iterator<T> begin(in), end;
      std::copy(begin, end, std::back_inserter(vec));
      return func(vec);
    };
  }
};

template <>
struct harness<const std::string &> {
  typedef const std::string &input_t;

  template <typename Out>
  inline static typename wrapped<Out>::type wrap(
      std::function<Out(input_t)> func) {
    return [func](std::istream &in) {
      std::string str;
      in >> str;
      return func(str);
    };
  }
};

template <typename Out>
inline std::function<void(std::istream &, std::ostream &)> harness_to_runner(
    std::function<Out(std::istream &)> func) {
  return [func](std::istream &in, std::ostream &out) { out << func(in); };
}

}  // namespace io
}  // namespace aoc

#endif