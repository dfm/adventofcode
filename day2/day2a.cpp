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
  size_t result = run_intcode_program(program);
  std::cout << result << std::endl;
}