#include "day2.hpp"

int main() {
  std::string line;
  std::vector<size_t> data;
  while (std::getline(std::cin, line, ',')) {
    try {
      data.push_back(std::stoi(line));
    } catch (const std::invalid_argument &exception) { std::cout << "invalid program" << std::endl; };
  }
  IntcodeProgram<size_t> program(data);
  for (size_t noun = 0; noun < 100; ++noun) {
    for (size_t verb = 0; verb < 100; ++verb) {
      program(1)    = noun;
      program(2)    = verb;
      size_t result = run_intcode_program(program);
      if (result == 19690720) { std::cout << noun << " " << verb << " " << (100 * noun + verb) << std::endl; }
    }
  }
}