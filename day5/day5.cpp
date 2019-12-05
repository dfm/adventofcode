#include "day5.hpp"
#include <sstream>

int main() {
  std::string line, token;
  std::vector<int> data;
  std::getline(std::cin, line);
  std::stringstream stream(line);
  while (std::getline(stream, token, ',')) {
    try {
      data.push_back(std::stoi(token));
    } catch (const std::invalid_argument &exception) { std::cout << "invalid program" << std::endl; };
  }
  IntcodeProgram<int> program(data);
  run_intcode_program(program);
}