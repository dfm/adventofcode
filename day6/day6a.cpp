#include <iostream>
#include <sstream>
#include <vector>
#include <map>
#include <set>

int main() {
  std::set<std::string> nodes;
  std::map<std::string, std::string> parents;
  std::string line, parent, child;
  while (std::getline(std::cin, line)) {
    std::stringstream stream(line);
    std::getline(stream, parent, ')');
    std::getline(stream, child, ')');
    parents[child] = parent;
    nodes.insert(parent);
    nodes.insert(child);
  }

  size_t count = 0;
  for (auto node : nodes) {
    while (parents.count(node)) {
      node = parents[node];
      count++;
    }
  }
  std::cout << count << std::endl;
}
