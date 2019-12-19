#include <iostream>
#include "day13.hpp"

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr << "Must provide a filename" << std::endl;
    return 1;
  }

  Game<long> game(argv[1]);
  game.run();
  std::cout << game.count(2) << std::endl;

  return 0;
}