#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>
#include <algorithm>

struct Point {
  size_t x;
  size_t y;
};

struct Pointing {
  Point point;
  double angle;
  double distance;
  bool flag;
};

double compute_angle(const Point &ref, const Point &other) { return std::atan2(double(other.y) - double(ref.y), double(other.x) - double(ref.x)); }
Pointing compute_pointing(const Point &ref, const Point &other) {
  double x     = double(other.x) - double(ref.x);
  double y     = double(other.y) - double(ref.y);
  double angle = std::atan2(y, x) + 0.5 * M_PI;
  if (angle < 0) angle += 2 * M_PI;
  double distance = std::sqrt(x * x + y * y);
  return {other, angle, distance, false};
}

struct compare_angles {
  inline bool operator()(const double &a, const double &b) { return std::abs(a - b) < 1e-12; }
};

struct compare_pointings {
  inline bool operator()(const Pointing &a, const Pointing &b) {
    bool same_angle = std::abs(a.angle - b.angle) < 1e-12;
    if (same_angle) return a.distance < b.distance;
    return a.angle < b.angle;
  }
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
    auto last  = std::unique(angles.begin(), angles.end(), compare_angles());
    auto count = std::distance(angles.begin(), last);
    if (count > best_count) {
      best_index = n;
      best_count = count;
    }
  }
  std::cout << asteroids[best_index].x << " " << asteroids[best_index].y << " " << best_count << std::endl;

  Point ref = asteroids[best_index];
  asteroids.erase(asteroids.begin() + best_index);
  std::vector<Pointing> pointings(n_total - 1);
  for (size_t n = 0; n < n_total - 1; ++n) { pointings[n] = compute_pointing(ref, asteroids[n]); }
  std::sort(pointings.begin(), pointings.end(), compare_pointings());

  // std::cout << ref.x << " " << ref.y << std::endl;
  // for (size_t n = 0; n < n_total - 1; ++n) {
  //   std::cout << pointings[n].point.x << " " << pointings[n].point.y << " " << pointings[n].angle << " " << pointings[n].distance << std::endl;
  // }

  size_t index = 0;
  size_t count = 0;
  while (pointings.size()) {
    count++;
    if (count == 200)
      std::cout << count << " " << pointings[index].point.x << " " << pointings[index].point.y << " "
                << (100 * pointings[index].point.x + pointings[index].point.y) << std::endl;
    double angle = pointings[index].angle;
    pointings.erase(pointings.begin() + index);
    for (; index < pointings.size(); ++index) {
      if (pointings[index].angle > angle + 1e-12) break;
    }
    if (index == pointings.size() - 1) index = 0;
  }

  return 0;
}