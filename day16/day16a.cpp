#include <iostream>
#include <vector>
#include <string>
#include <cstdlib>

int main() {
  // Read the input
  std::string line;
  if (!std::getline(std::cin, line)) return 1;
  size_t size = line.size();
  std::vector<long> numbers(size);
  for (size_t n = 0; n < size; ++n) {
    numbers[n] = line[n] - '0';
  }

  long pattern[4] = {0, 1, 0, -1};
  std::vector<long> result(size);
  for (size_t pass = 0; pass < 100; ++pass) {
    for (size_t order = 1; order <= size; ++order) {
      result[order - 1] = 0;
      for (size_t n = 1; n <= size; ++n) {
        result[order - 1] += (numbers[n - 1] * pattern[(n / order) % 4]) % 10;
      }
      result[order - 1] = std::abs(result[order - 1] % 10);
    }
    numbers = result;
  }
  for (size_t n = 0; n < 8; ++n) std::cout << numbers[n];
  std::cout << std::endl;

  return 0;
}