#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <queue>
#include <algorithm>
#include <set>
#include <unordered_map>

struct Coord {
  int x;
  int y;
  int dist;
};

struct Key {
  char name;
  Coord pos;
};

bool is_lower_letter(const char &c) { return ('a' <= c) && (c <= 'z'); }

class Grid {
  public:
  Grid(const Grid &grid)
     : width_(grid.width_), height_(grid.height_), data_(grid.data_), visited_(grid.data_.size(), false), locks_(grid.locks_), cache_(grid.cache_){};
  Grid(int width, int height, std::vector<int> data, std::unordered_map<std::string, int> &cache)
     : width_(width), height_(height), data_(data), visited_(data.size(), false), cache_(cache) {
    for (const char &value : data_) {
      if (is_lower_letter(value)) locks_.insert(value);
    }
  };

  std::string repr(const char &key) {
    std::stringstream stream;
    stream << key << "+";
    for (const char &lock : locks_) stream << lock;
    return stream.str();
  }

  int get_index(int x, int y) const { return y * width_ + x; }
  inline bool is_valid(int x, int y) const {
    if ((0 > x) || (x >= width_) || (0 > y) || (y >= height_)) return false;
    int index = get_index(x, y);
    if (visited_[index]) return 0;
    char value = data_[index];
    return (value == '.') || is_lower_letter(value);
  }

  int find_keys(const std::vector<int> &x, const std::vector<int> &y) {
    std::vector<int> distances;
    for (size_t robot_id = 0; robot_id < x.size(); ++robot_id) {
      std::queue<Coord> queue;
      queue.push({x[robot_id], y[robot_id], 0});
      std::vector<Key> keys;
      while (!queue.empty()) {
        Coord pos = queue.front();
        queue.pop();
        int index  = get_index(pos.x, pos.y);
        char value = data_[index];
        if (is_lower_letter(value)) {
          keys.push_back({value, pos});
        } else {
          for (int i = 0; i < 4; ++i) {
            int new_x = pos.x + dx[i], new_y = pos.y + dy[i];
            if (is_valid(new_x, new_y)) {
              visited_[get_index(new_x, new_y)] = true;
              queue.push({new_x, new_y, pos.dist + 1});
            }
          }
        }
      }

      int count = 0;
      for (const auto &key : keys) {
        Grid new_grid(*this);
        new_grid.unlock_door(key.name);
        std::string id = repr(key.name);
        auto found     = cache_.find(id);
        if (found != cache_.end()) {
          // Cache hit
          distances.push_back(key.pos.dist + found->second);
        } else {
          std::vector<int> new_x(x), new_y(y);
          new_x[robot_id]    = key.pos.x;
          new_y[robot_id]    = key.pos.y;
          int inner_distance = new_grid.find_keys(new_x, new_y);
          cache_.insert({id, inner_distance});
          distances.push_back(key.pos.dist + inner_distance);
        }
      }
    }
    if (distances.empty()) return 0;
    return *std::min_element(distances.begin(), distances.end());
  }

  void unlock_door(const char &key) {
    locks_.erase(key);
    for (size_t n = 0; n < data_.size(); ++n) {
      if ((data_[n] == '@') || (data_[n] == key) || (data_[n] == key - ('a' - 'A'))) { data_[n] = int('.'); }
    }
  }

  private:
  int width_, height_;
  std::vector<int> data_;
  std::vector<bool> visited_;
  std::set<char> locks_;
  std::unordered_map<std::string, int> &cache_;
  int dx[4] = {0, 0, -1, 1};
  int dy[4] = {-1, 1, 0, 0};
};

int main(int argc, char *argv[]) {
  std::ifstream stream(argv[1]);
  if (!stream.is_open()) { throw std::runtime_error("unable to open file"); }

  std::string line;
  int width = 0, height = 0;
  std::vector<int> x, y;
  std::vector<int> data;
  while (std::getline(stream, line)) {
    if (!line.size()) break;
    width = line.size();
    height++;
    for (size_t n = 0; n < width; ++n) {
      data.push_back(line[n]);
      if (line[n] == '@') {
        x.push_back(n);
        y.push_back(height - 1);
      }
    }
  }
  stream.close();

  std::unordered_map<std::string, int> cache;
  Grid grid(width, height, data, cache);
  std::cout << grid.find_keys(x, y) << std::endl;

  return 0;
}