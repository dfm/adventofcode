#include <iostream>
#include "day13.hpp"

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr << "Must provide a filename" << std::endl;
    return 1;
  }

  Game<long, false> game(argv[1]);
  long score = game.play(2);
  std::cout << score << std::endl;

  return 0;
}