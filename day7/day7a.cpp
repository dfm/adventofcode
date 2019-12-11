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

  std::vector<long> phases = {0, 1, 2, 3, 4};
  size_t nphase            = phases.size();

  long max_power = 0;

  do {
    std::vector<IntcodeProgram<long>> programs(nphase);
    std::list<long> inputs = {phases[0], 0};
    for (int i = 0; i < nphase; ++i) {
      programs[i].set_data(data);
      if (i == 0) {
        programs[i].set_input(&inputs);
      } else {
        programs[i - 1].output()->push_back(phases[i]);
        programs[i].set_input(programs[i - 1].output());
      }
    }
    for (int i = 0; i < nphase; ++i) { programs[i].run(); }
    long power = programs[nphase - 1].output()->front();
    if (power > max_power) {
      max_power = power;
      for (int j = 0; j < nphase; ++j) { std::cout << phases[j]; }
      std::cout << " -> " << max_power << std::endl;
    }
  } while (std::next_permutation(phases.begin(), phases.end()));

  std::cout << max_power << std::endl;
}