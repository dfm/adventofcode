#include <iostream>
#include <fstream>
#include <algorithm>

class State {
  public:
  State(const char *filename) {
    std::ifstream stream(filename);
    if (!stream.is_open()) { throw std::runtime_error("unable to open file"); }
    std::string line;
    for (int i = 0; i < 5; ++i) {
      std::getline(stream, line);
      for (int j = 0; j < 5; ++j) { state[i * 5 + j] = line[j] == '#'; }
    }
    stream.close();
  }

  void update() {
    int adj[25];
    std::fill_n(adj, 25, 0);
    int i, j, ind;
    adj[0] = state[1];
    for (j = 1; j < 4; ++j) { adj[j] = state[j - 1] + state[j + 1]; }
    adj[4] = state[3];

    for (i = 1; i < 4; ++i) {
      ind      = 5 * i;
      adj[ind] = state[ind - 5] + state[ind + 1] + state[ind + 5];
      ++ind;
      for (j = 1; j < 4; ++j, ++ind) { adj[ind] = state[ind - 5] + state[ind - 1] + state[ind + 1] + state[ind + 5]; }
      adj[ind] = state[ind - 5] + state[ind - 1] + state[ind + 5];
    }
  }

  private:
  bool state[25];
};

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr << "Must provide a filename" << std::endl;
    return 1;
  }

  State state(argv[1]);
  // std::cout << maze.traverse() << std::endl;

  return 0;
}