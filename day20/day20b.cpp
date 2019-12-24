#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <queue>
#include <set>

struct Coord {
  int x;
  int y;
  int distance = 0;
  int level    = 0;
};

bool operator<(const Coord &a, const Coord &b) { return (a.y < b.y) || ((a.y == b.y) && (a.x < b.x)); }
bool operator==(const Coord &a, const Coord &b) { return (a.x == b.x) && (a.y == b.y) && (a.level == b.level); }

Coord operator+(const Coord &a, const Coord &b) { return {a.x + b.x, a.y + b.y, a.distance + b.distance, a.level}; }

inline bool is_letter(const char &val) { return ('A' <= val) && (val <= 'Z'); }

class Maze {
  public:
  Maze(int inner_size, const char *filename) : inner_size(inner_size) {
    std::ifstream stream(filename);
    if (!stream.is_open()) { throw std::runtime_error("unable to open file"); }
    std::string line;
    height = 0;
    while (std::getline(stream, line)) {
      for (const char &c : line) grid.push_back(c);
      width = line.size();
      height++;
    }
    stream.close();
    find_tunnels();

    // for (int y = 0; y < height; ++y) {
    //   for (int x = 0; x < width; ++x) {
    //     if (is_inside(x, y))
    //       std::cout << "I";
    //     else
    //       std::cout << get(x, y);
    //   }
    //   std::cout << std::endl;
    // }
  }

  void find_tunnels() {
    std::map<std::pair<const char, const char>, std::vector<std::pair<Coord, Coord>>> map;
    for (int y = 0; y < height - 1; ++y) {
      for (int x = 0; x < width - 1; ++x) {
        int level      = is_inside(x, y) ? -1 : 1;
        const char &x1 = get(x, y);
        const char &x2 = get(x + 1, y);
        if (is_letter(x1) && is_letter(x2)) {
          if (x - 1 >= 0 && get(x - 1, y) == '.') {
            map[std::make_pair(x1, x2)].push_back({{x, y}, {x - 1, y, 0, level}});
          } else {
            map[std::make_pair(x1, x2)].push_back({{x + 1, y}, {x + 2, y, 0, level}});
          }
          x++;
        }

        const char &y1 = get(x, y);
        const char &y2 = get(x, y + 1);
        if (is_letter(y1) && is_letter(y2)) {
          if (y - 1 >= 0 && get(x, y - 1) == '.') {
            map[std::make_pair(y1, y2)].push_back({{x, y}, {x, y - 1, 0, level}});
          } else {
            map[std::make_pair(y1, y2)].push_back({{x, y + 1}, {x, y + 2, 0, level}});
          }
        }
      }
    }

    for (const auto &val : map) {
      if (val.first.first == 'A' && val.first.second == 'A') {
        start       = val.second[0].second;
        start.level = 0;
      } else if (val.first.first == 'Z' && val.first.second == 'Z') {
        finish       = val.second[0].second;
        finish.level = 0;
      } else {
        tunnels[val.second[0].first] = val.second[1].second;
        tunnels[val.second[1].first] = val.second[0].second;
      }
    }
  }

  int traverse() const {
    std::vector<std::set<int>> visited(grid.size());
    visited[start.y * width + start.x].insert(start.level);
    std::queue<Coord> queue;
    queue.push(start);
    while (!queue.empty()) {
      Coord pos = queue.front();
      queue.pop();
      if (pos == finish) {
        std::cout << pos.x << " " << pos.y << " " << pos.level << std::endl;
        return pos.distance;
      }

      for (int n = 0; n < 4; ++n) {
        Coord new_pos = pos + delta[n];
        resolve(new_pos);
        int index = new_pos.y * width + new_pos.x;
        if (visited[index].find(new_pos.level) == visited[index].end() && grid[index] == '.') {
          visited[index].insert(new_pos.level);
          queue.push(new_pos);
        }
      }
    }
    return -1;
  }

  void resolve(Coord &pos) const {
    try {
      const Coord &new_pos = tunnels.at(pos);
      if (pos.level + new_pos.level >= 0) {
        pos.x = new_pos.x;
        pos.y = new_pos.y;
        pos.level += new_pos.level;
      }
    } catch (const std::out_of_range &e) { return; }
  }

  bool is_inside(int x, int y) const {
    return (2 + inner_size <= x) && (x < width - inner_size - 3) && (2 + inner_size <= y) && (y < height - inner_size - 2);
  }
  char &get(int x, int y) { return grid[y * width + x]; }
  const char &get(int x, int y) const { return grid[y * width + x]; }

  private:
  int width, height, inner_size;
  std::vector<char> grid;
  std::map<Coord, Coord> tunnels;
  Coord start, finish;
  Coord delta[4] = {{-1, 0, 1}, {1, 0, 1}, {0, -1, 1}, {0, 1, 1}};
};

int main(int argc, char *argv[]) {
  if (argc != 3) {
    std::cerr << "Must provide a filename" << std::endl;
    return 1;
  }

  Maze maze(std::stoi(argv[1]), argv[2]);
  std::cout << maze.traverse() << std::endl;

  return 0;
}