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

  std::string node = "YOU";
  std::vector<std::string> you_tree;
  while (parents.count(node)) {
    node = parents[node];
    you_tree.push_back(node);
  }

  node         = "SAN";
  size_t count = 0, best_count = -1;
  while (parents.count(node)) {
    node = parents[node];
    for (size_t i = 0; i < you_tree.size(); ++i) {
      if (you_tree[i] == node) {
        size_t new_count = count + i;
        if (new_count < best_count) { best_count = new_count; }
      }
    }
    count++;
  }

  std::cout << best_count << std::endl;
}
