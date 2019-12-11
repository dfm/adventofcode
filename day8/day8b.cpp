#include <iostream>
#include <fstream>
#include <vector>

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr << "Must provide a filename" << std::endl;
    return 1;
  }
  std::ifstream stream(argv[1]);
  if (!stream.is_open()) {
    std::cerr << "Unable to open: " << argv[1] << std::endl;
    return 2;
  }
  std::string data;
  if (!std::getline(stream, data)) {
    stream.close();
    std::cerr << "Unable to read: " << argv[1] << std::endl;
    return 2;
  }
  stream.close();

  const size_t width = 25, height = 6;
  int image[width * height];
  for (size_t i = 0; i < width * height; ++i) image[i] = 2;

  size_t full_index = 0;
  while (full_index < data.size()) {
    for (size_t y = 0, index = 0; y < height; ++y) {
      for (size_t x = 0; x < width; ++x, ++full_index, ++index) {
        if (image[index] == 2) { image[index] = data[full_index] - '0'; }
      }
    }
  }

  for (size_t y = 0, index = 0; y < height; ++y) {
    for (size_t x = 0; x < width; ++x, ++index) {
      if (image[index])
        std::cout << "O";
      else
        std::cout << " ";
    }
    std::cout << std::endl;
  }

  return 0;
}