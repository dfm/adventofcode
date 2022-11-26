#ifndef AOC_REMOTE_HPP
#define AOC_REMOTE_HPP

#ifndef AOC_OFFLINE_MODE
#include <cpr/cpr.h>
#endif

#include <cstdlib>
#include <exception>
#include <filesystem>
#include <fstream>
#include <sstream>
#include <string>

#include "./types.hpp"

namespace fs = std::filesystem;

namespace aoc {
namespace remote {

std::string get_session_key() {
  fs::path config_dir(std::getenv("HOME"));
  config_dir /= ".config";
  config_dir /= "aoc";
  auto key_path = config_dir / "key";

  std::string key;
  std::ifstream file(key_path);
  if (!file.is_open()) {
    std::ostringstream msg;
    msg << "Session key file " << key_path << " not found; please create it";
    throw std::runtime_error(msg.str());
  }

  file >> key;
  if (key.empty()) {
    std::ostringstream msg;
    msg << "Session key file " << key_path
        << " is empty; please add your key to it";
    throw std::runtime_error(msg.str());
  }

  return key;
}

struct data {
  year_t year;
  day_t day;
  data(year_t year, day_t day) : year(year), day(day) {}

  fs::path path() const {
    auto path = fs::current_path();
    path /= "data";
    path /= std::to_string(year);
    path /= std::to_string(day);
    return path;
  }

  void download() const {
#ifndef AOC_OFFLINE_MODE
    std::ostringstream url;
    url << "https://adventofcode.com/" << year << "/day/" << day << "/input";
    auto r = cpr::Get(cpr::Url{url.str()},
                      cpr::Cookies{{"session", get_session_key()}});

    if (r.status_code != 200) {
      std::ostringstream msg;
      msg << "Request to " << url.str();
      msg << " failed with status code " << r.status_code;
      msg << " and reason:\n" << r.reason;
      throw std::runtime_error(msg.str());
    }

    auto filename = path();
    fs::create_directories(filename.parent_path());
    std::ofstream file(filename);
    if (!file.is_open()) {
      std::ostringstream msg;
      msg << "Failed to open file " << filename << " for writing";
      throw std::runtime_error(msg.str());
    }

    file << r.text;
#else
    throw std::runtime_error("Please compile with AOC_BUILD_REMOTE=ON");
#endif
  }

  std::ifstream get() const {
    auto filename = path();
    if (!fs::exists(filename)) {
      download();
    }

    std::ifstream file(filename);
    if (!file.is_open()) {
      std::ostringstream msg;
      msg << "Failed to open file " << filename << " for reading";
      throw std::runtime_error(msg.str());
    }

    return file;
  }
};

}  // namespace remote
}  // namespace aoc

#endif