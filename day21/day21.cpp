#include <iostream>
#include "day21.hpp"

int main(int argc, char *argv[]) {
  if (argc != 3) {
    std::cerr << "Must provide a filename" << std::endl;
    return 1;
  }
  int part = std::stoi(argv[1]);

  std::queue<long> input;

  std::string instructions;
  if (part == 1) {
    instructions = R"(NOT D T
OR C T
NOT T J
NOT A T
OR T J
WALK
)";
  } else {
    instructions = R"(NOT B T
NOT C J
OR T J
AND D J
NOT H T
NOT T T
OR E T
AND T J
NOT A T
OR T J
RUN
)";
  }
  for (const char &c : instructions) { input.push(long(c)); }

  IntcodeProgram<long> program(argv[2], input);
  std::cout << program.run() << std::endl;
  std::queue<long> &output = program.output();

  long last = 0;
  while (!output.empty()) {
    last = output.front();
    std::cout << static_cast<char>(last);
    output.pop();
  }
  std::cout << std::endl;
  std::cout << last << std::endl;

  return 0;
}