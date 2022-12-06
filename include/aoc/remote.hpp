#ifndef AOC_REMOTE_HPP
#define AOC_REMOTE_HPP

#include <filesystem>
#include <string>

#include "types.hpp"

namespace aoc {
namespace remote {

std::string get_session_key();

struct data {
  year_t year;
  day_t day;
  data(year_t year, day_t day) : year(year), day(day) {}
  void download() const;
  std::filesystem::path path() const;
  std::filesystem::path get() const;
};

}  // namespace remote
}  // namespace aoc

#endif