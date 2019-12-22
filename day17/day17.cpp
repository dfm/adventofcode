#include <iostream>
#include "day17.hpp"

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr << "Must provide a filename" << std::endl;
    return 1;
  }

  // Read the map
  std::queue<long> input;
  IntcodeProgram<long> program(argv[1], input);
  program.run();
  auto &output = program.output();

  Map<int> map(output);
  std::cout << "Intersection parameter: " << map.count_intersections() << std::endl;

  // Find the set of instructions
  input = map.build_instruction_set();

  IntcodeProgram<long> program2(argv[1], input);
  program2.get(0, 1) = 2;
  program2.run();
  output = program2.output();
  std::cout << "Dust collected: " << output.back() << std::endl;

  return 0;
}