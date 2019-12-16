#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>
#include <algorithm>

struct Point {
  size_t x;
  size_t y;
};

double compute_angle(const Point &ref, const Point &other) { return std::atan2(double(other.y) - double(ref.y), double(other.x) - double(ref.x)); }

bool compare_angles(const double &a, const double &b) { return std::abs(a - b) < 1e-12; }

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

  std::string line;
  size_t y = 0;
  std::vector<Point> asteroids;
  while (std::getline(stream, line)) {
    for (size_t x = 0; x < line.size(); ++x) {
      if (line[x] == '#') { asteroids.push_back({x, y}); }
    }
    y++;
  }
  stream.close();

  size_t best_index = 0, best_count = 0;
  size_t n_total = asteroids.size();
  for (size_t n = 0; n < n_total; ++n) {
    Point ref = asteroids[n];
    std::vector<double> angles(n_total - 1);
    for (size_t m = 0; m < n_total; ++m) {
      if (m == n) continue;
      angles[m - (m > n)] = compute_angle(ref, asteroids[m]);
    }
    std::sort(angles.begin(), angles.end());
    auto last = std::unique(angles.begin(), angles.end(), &compare_angles);
    // angles.erase(last, angles.end());
    auto count = std::distance(angles.begin(), last);
    if (count > best_count) {
      best_index = n;
      best_count = count;
    }
  }
  std::cout << asteroids[best_index].x << " " << asteroids[best_index].y << " " << best_count << std::endl;

  return 0;
}