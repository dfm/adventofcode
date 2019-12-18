#include <iostream>
#include "day12.hpp"

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr << "Must provide a filename" << std::endl;
    return 1;
  }

  System system(argv[1]);
  std::cout << system << std::endl;

  long energy = system.steps(1000);
  std::cout << system << std::endl;
  std::cout << "total energy: " << energy << std::endl;

  return 0;
}