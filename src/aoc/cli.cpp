#include <fmt/core.h>

#include <CLI/CLI.hpp>
#include <chrono>
#include <ctime>
#include <filesystem>
#include <format>
#include <fstream>
#include <inja/inja.hpp>
#include <iostream>
#include <regex>

#include "aoc/aoc.hpp"

namespace fs = std::filesystem;

int main(int argc, char** argv) {
  CLI::App app{"Holiday hacking"};
  bool start = false;
  aoc::year_t year = 0;
  aoc::day_t day = 0;
  app.add_option("-y,--year", year, "What year is it?");
  app.add_option("-d,--day", day, "Pick a day, any day");
  app.add_flag("-s,--start", start, "Start a new day");
  CLI11_PARSE(app, argc, argv);

  auto now =
      std::chrono::system_clock::to_time_t(std::chrono::system_clock::now());
  auto tm = std::localtime(&now);
  auto month = tm->tm_mon + 1;
  if (year == 0) {
    if (month < 12) {
      year = tm->tm_year + 1899;
    } else {
      year = tm->tm_year + 1900;
    }
  }
  if (day == 0 && month == 12) {
    day = tm->tm_mday;
  }

  std::cout << "Year: " << year << std::endl;
  if (day == 0) {
    std::cout << "Day: all" << std::endl;
  } else {
    std::cout << "Day: " << day << std::endl;
  }

  if (start) {
    if (day == 0) {
      std::cerr << "Please specify a day." << std::endl;
      return 1;
    }

    auto path = fs::current_path();
    path /= "src";
    auto template_path = path / "template.cpp";
    std::ifstream template_file(template_path);
    if (!template_file.is_open()) {
      std::cerr << "Failed to open " << template_path << " for reading"
                << std::endl;
      return 1;
    }

    path /= std::to_string(year);
    path /= fmt::format("day{:02d}.cpp", day);
    if (fs::exists(path)) {
      std::cerr << "An implementation already exists at: " << path << std::endl;
      return 1;
    }
    fs::create_directories(path.parent_path());
    std::ofstream target_file(path);
    if (!target_file.is_open()) {
      std::cerr << "Failed to open " << path << " for writing" << std::endl;
      return 1;
    }

    inja::Environment env;
    inja::json data;
    data["year"] = year;
    data["day"] = day;
    env.write(template_path, data, path);

    std::cout << "Today is a new day..." << std::endl;
    return 0;
  }

  try {
    if (day == 0) {
      aoc::run_all(year, std::cout);
    } else {
      aoc::run(year, day, std::cout);
    }
  } catch (const std::runtime_error& e) {
    std::cerr << "Error: " << e.what() << std::endl;
    return 1;
  }

  return 0;
}