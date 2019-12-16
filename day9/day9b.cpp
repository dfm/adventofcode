#include <iostream>
#include "day9.hpp"

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr << "Must provide a filename" << std::endl;
    return 1;
  }
  IntcodeProgram<long> program(argv[1]);

  std::list<long> input = {2};

  program.set_input(&input);
  std::cout << program.run() << std::endl;

  return 0;
}