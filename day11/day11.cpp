#include <iostream>
#include "day11.hpp"

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr << "Must provide a filename" << std::endl;
    return 1;
  }

  // Part 1
  Robot<long> robot1(argv[1]);
  std::cout << "PART 1:\n\n" << robot1.run(0) << std::endl;

  // Part 2
  Robot<long> robot2(argv[1]);
  robot2.run(1);
  std::cout << "\nPART 2:\n\n" << robot2 << std::endl;

  return 0;
}