#include <iostream>
#include "day15.hpp"

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr << "Must provide a filename" << std::endl;
    return 1;
  }

  Robot<long> robot(argv[1]);
  size_t fill_time = robot.run();
  std::cout << "fill time: " << fill_time << std::endl;

  return 0;
}