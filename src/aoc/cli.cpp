#include "aoc/aoc.hpp"

#include <CLI/CLI.hpp>
#include <iostream>

int main(int argc, char** argv) {
  CLI::App app{"Holiday hacking"};
  aoc::year_t year = 2022;
  aoc::day_t day = 0;
  app.add_option("-y,--year", year, "What year is it?");
  app.add_option("-d,--day", day, "Pick a day, any day");
  CLI11_PARSE(app, argc, argv);

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