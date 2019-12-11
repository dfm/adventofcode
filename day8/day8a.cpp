#include <iostream>
#include <fstream>
#include <vector>

struct Counter {
  size_t num_zeros;
  size_t num_ones;
  size_t num_twos;
};

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
  size_t index = 0;

  size_t layer = 0, best_layer = -1;
  std::vector<Counter> layers;
  while (index < data.size()) {
    layers.push_back(Counter({0, 0, 0}));
    for (size_t y = 0; y < height; ++y) {
      for (size_t x = 0; x < width; ++x) {
        int datum = data[index] - '0';
        switch (datum) {
          case 0: layers[layer].num_zeros++; break;
          case 1: layers[layer].num_ones++; break;
          case 2: layers[layer].num_twos++; break;
        }
        index++;
      }
    }
    if (layers[best_layer].num_zeros >= layers[layer].num_zeros) { best_layer = layer; }
    layer++;
  }

  std::cout << layers[best_layer].num_zeros << " " << (layers[best_layer].num_ones * layers[best_layer].num_twos) << std::endl;

  return 0;
}