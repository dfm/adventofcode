#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <algorithm>
#include <iterator>

struct Segment {
  char direction;
  size_t distance;
};

struct Point {
  long int x;
  long int y;
  size_t distance;
};

Point operator+(const Point &a, const Point &b) { return {a.x + b.x, a.y + b.y}; }
bool operator<(const Point &a, const Point &b) { return (a.x < b.x) || ((a.x == b.x) && (a.y < b.y)); }
bool operator==(const Point &a, const Point &b) { return (a.x == b.x) && (a.y == b.y); }

class Path {
  public:
  Path() : current({0, 0}){};

  void move(const std::string &token) {
    if (token.size() <= 1) { throw std::invalid_argument("invalid token"); }
    const char direction  = token[0];
    const size_t distance = std::stoul(token.substr(1));

    Point delta({0, 0});
    if (direction == 'U') {
      delta.y = 1;
    } else if (direction == 'D') {
      delta.y = -1;
    } else if (direction == 'L') {
      delta.x = -1;
    } else if (direction == 'R') {
      delta.x = 1;
    } else {
      throw std::runtime_error("invalid direction");
    }

    size_t current_distance = current.distance;
    for (size_t i = 0; i < distance; ++i) {
      current_distance++;
      current          = current + delta;
      current.distance = current_distance;
      points.push_back(current);
    }
  }

  friend std::vector<Point> find_overlap(const Path &path1, const Path &path2) {
    std::vector<Point> results;
    for (const auto &point1 : path1.points) {
      for (const auto &point2 : path2.points) {
        if (point1 == point2) {
          Point point(point1);
          point.distance += point2.distance;
          results.push_back(point);
        }
      }
    }
    return results;
  }

  private:
  Point current;
  std::vector<Point> points;
};

size_t find_shortest_distance(std::vector<Point> points) {
  struct {
    bool operator()(const Point &a, const Point &b) const { return a.distance < b.distance; }
  } comp;
  std::sort(points.begin(), points.end(), comp);
  return points[0].distance;
}

std::vector<Path> parse_paths(std::istream &stream) {
  std::vector<Path> paths;
  std::string line, token;
  while (std::getline(stream, line)) {
    Path path;
    auto stream = std::stringstream(line);
    while (std::getline(stream, token, ',')) { path.move(token); }
    paths.push_back(path);
  }
  return paths;
}

int main() {
  std::string line, token;
  auto paths = parse_paths(std::cin);
  if (paths.size() != 2) { throw std::runtime_error("invalid spec"); }
  auto matches = find_overlap(paths[0], paths[1]);
  if (matches.size() < 1) {
    std::cout << "no matches\n";
    return 1;
  }
  std::cout << find_shortest_distance(matches) << std::endl;
  return 0;
}