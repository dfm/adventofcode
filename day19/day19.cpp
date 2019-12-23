#include <iostream>
#include "day19.hpp"

struct Drone {
  char *filename;
  Drone(char *filename) : filename(filename){};
  long operator()(long x, long y) const {
    std::queue<long> input;
    IntcodeProgram<long> program(filename, input);
    input.push(x);
    input.push(y);
    program.run();
    return program.output().front();
  }
};

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr << "Must provide a filename" << std::endl;
    return 1;
  }
  Drone drone(argv[1]);

  // Count the squares
  long count = 0;
  for (long y = 0; y < 50; ++y) {
    for (long x = 0; x < 50; ++x) { count += drone(x, y); }
  }
  std::cout << "Part 1: " << count << std::endl;

  // Follow the light
  long x_min = 0;
  for (long y = 100;; ++y) {
    for (long x = x_min;; ++x) {
      long result = drone(x, y);
      if (result) {
        x_min = x;
        // Check the upper corner
        if (drone(x + 99, y - 99)) {
          std::cout << "Part 2: " << (x * 10000 + y - 99) << std::endl;
          return 0;
        }
        break;
      }
    }
  }

  return 0;
}