#include <iostream>
#include <vector>
#include <string>
#include <cstdlib>

int main() {
  // Read the input
  std::string line;
  if (!std::getline(std::cin, line)) return 1;
  size_t size = line.size();
  std::vector<int> numbers(size);
  for (size_t n = 0; n < size; ++n) {
    numbers[n] = line[n] - '0';
  }

  size_t factor = 10000;
  size_t offset = std::stol(line.substr(0, 7));
  size_t full_size = size * factor - offset;
  std::vector<int> cache(full_size);
  for (size_t n = 0; n < full_size; ++n) cache[n] = numbers[(offset + n) % size];

  for (size_t pass = 0; pass < 100; ++pass) {
    int prev = cache[full_size - 1];
    for (size_t n = 2; n <= full_size; ++n) {
      size_t ind = full_size - n;
      prev = (cache[ind] + prev) % 10;
      cache[ind] = prev;
    }
  }
  for (size_t n = 0; n < 8; ++n) std::cout << cache[n];
  std::cout << std::endl;

  return 0;
}