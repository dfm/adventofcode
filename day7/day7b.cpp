#include "day7.hpp"
#include <sstream>
#include <algorithm>

int main() {
  std::string line, token;
  std::vector<long> data;
  std::getline(std::cin, line);
  std::stringstream stream(line);
  while (std::getline(stream, token, ',')) {
    try {
      data.push_back(std::stol(token));
    } catch (const std::invalid_argument &exception) { std::cout << "invalid program" << std::endl; };
  }

  std::vector<long> phases = {5, 6, 7, 8, 9};
  size_t nphase            = phases.size();

  long max_power = 0;

  do {
    std::vector<IntcodeProgram<long>> programs(nphase);
    std::list<long> inputs = {phases[0], 0};
    for (int i = 1; i < nphase; ++i) {
      programs[i].set_data(data);
      programs[i - 1].output()->push_back(phases[i]);
      programs[i].set_input(programs[i - 1].output());
    }
    programs[0].set_data(data);
    programs[nphase - 1].output()->push_back(phases[0]);
    programs[nphase - 1].output()->push_back(0);
    programs[0].set_input(programs[nphase - 1].output());

    bool finished = false;
    while (!finished) {
      for (int i = 0; i < nphase; ++i) { finished = programs[i].run(); }
    }
    long power = programs[nphase - 1].output()->front();
    if (power > max_power) {
      max_power = power;
      for (int j = 0; j < nphase; ++j) { std::cout << phases[j]; }
      std::cout << " -> " << max_power << std::endl;
    }
  } while (std::next_permutation(phases.begin(), phases.end()));

  std::cout << max_power << std::endl;

  // std::vector<long> phases = {9, 8, 7, 6, 5};
  // size_t nphase            = phases.size();
  // std::vector<IntcodeProgram<long>> programs(nphase);

  // long max_power = 0;

  // // do {
  // bool first = true;
  // for (int i = 0; i < nphase; ++i) {
  //   programs[i].set_data(data);
  //   std::cout << programs[i] << "\n";
  // }
  // std::vector<long> inputs = {0, 0};
  // long power               = 0;
  // while (inputs.size() > 1) {
  //   for (int i = 0; i < nphase; ++i) {
  //     if (first) { inputs.insert(inputs.begin(), phases[i]); }
  //     std::vector<long> tmp = programs[i].run(inputs);
  //     std::cout << programs[i] << "\n";

  //     inputs = tmp;
  //     if (tmp.size() <= 1) break;
  //     if (i == nphase - 1) { power = tmp[0]; }
  //     std::cout << inputs.size() << std::endl;
  //   }
  //   first = false;
  // }
  // std::cout << power << "\n";
  // if (power > max_power) {
  //   max_power = power;
  //   for (int j = 0; j < nphase; ++j) { std::cout << phases[j]; }
  //   std::cout << " -> " << max_power << std::endl;
  // }
  // // } while (std::next_permutation(phases.begin(), phases.end()));

  // std::cout << max_power << std::endl;
}