#include <iostream>
#include "day12.hpp"

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr << "Must provide a filename" << std::endl;
    return 1;
  }

  System system(argv[1]);
  size_t x_period     = system.x.find_period();
  size_t y_period     = system.y.find_period();
  size_t z_period     = system.z.find_period();
  size_t total_period = lcm(lcm(x_period, y_period), z_period);

  std::cout << "x period: " << x_period << std::endl;
  std::cout << "y period: " << y_period << std::endl;
  std::cout << "z period: " << z_period << std::endl;
  std::cout << "total period: " << total_period << std::endl;

  return 0;
}