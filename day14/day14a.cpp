#include <fstream>
#include <sstream>
#include <iostream>
#include <regex>
#include <vector>
#include <unordered_map>

struct Token {
  size_t amount;
  std::string name;
  Token() : amount(0){};
  Token(const std::string &token) {
    std::smatch m;
    std::regex re("([0-9]+) ([A-Z]+)");
    if (!std::regex_search(token, m, re) || m.size() != 3) throw std::runtime_error("token error");
    amount = std::stol(m[1]);
    name   = m[2];
  };
  friend std::ostream &operator<<(std::ostream &os, const Token &token) {
    os << token.amount << " " << token.name;
    return os;
  }
};

struct Reaction {
  Token output;
  std::vector<Token> input;
  Reaction(){};
  Reaction(Token token) { output = token; }
  friend std::ostream &operator<<(std::ostream &os, const Reaction &reaction) {
    for (size_t n = 0; n < reaction.input.size(); ++n) {
      os << reaction.input[n];
      if (n != reaction.input.size() - 1) os << ", ";
    }
    os << " => " << reaction.output;
    return os;
  }
};

class System {
  public:
  System(size_t initial_ore) : initial_ore_(initial_ore) { resources_["ORE"] = initial_ore_; }
  void add_reaction(Reaction reaction) { reactions_[reaction.output.name] = reaction; }

  bool build_tree(size_t factor, const Reaction &reaction) {
    std::string ore = "ORE";
    resources_[reaction.output.name] += factor * reaction.output.amount;
    for (const Token &token : reaction.input) {
      if (resources_[token.name] < factor * token.amount) {
        if (token.name == ore) return false;
        const Reaction &reaction = reactions_[token.name];
        size_t delta             = factor * token.amount - resources_[token.name];
        size_t yield             = reaction.output.amount;
        size_t new_factor        = (delta + yield - 1) / yield; // "delta / yield" rounded up
        if (!build_tree(new_factor, reaction)) return false;
      }
      resources_[token.name] -= factor * token.amount;
    }
    return true;
  }

  size_t make_fuel(size_t factor = 1) {
    const Reaction &reaction = reactions_["FUEL"];
    build_tree(factor, reaction);
    return resources_["ORE"];
  }

  void reset() {
    resources_.erase(resources_.begin(), resources_.end());
    resources_["ORE"] = initial_ore_;
  }

  size_t search(size_t lower) {
    const Reaction &reaction = reactions_["FUEL"];

    // Doubling to find upper limit
    bool flag    = true;
    size_t upper = lower;
    reset();
    do {
      lower = upper;
      upper = 2 * lower;
      reset();
      flag = build_tree(upper, reaction);
    } while (flag);

    // Binary search
    while (lower < upper - 1) {
      size_t middle = (lower + upper) / 2;
      reset();
      flag = build_tree(upper, reaction);
      if (flag)
        lower = middle;
      else
        upper = middle;
    }

    // Refine
    reset();
    build_tree(lower, reaction);
    while (build_tree(1, reaction)) { lower++; }
    return lower;
  }

  private:
  size_t initial_ore_;
  std::unordered_map<std::string, size_t> resources_;
  std::unordered_map<std::string, Reaction> reactions_;
};

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr << "Must provide a filename" << std::endl;
    return 1;
  }

  size_t initial_ore = 1000000000000;
  System system(initial_ore);
  std::smatch m;
  std::regex re("([0-9A-Z, ]+) => ([0-9]+ [A-Z]+)");
  char *filename = argv[1];
  std::ifstream stream(filename);
  if (!stream.is_open()) { throw std::runtime_error("unable to open file"); }
  std::string line;
  while (std::getline(stream, line)) {
    if (std::regex_search(line, m, re)) {
      if (m.size() != 3) throw std::runtime_error("parse error");
    }

    Token token(m[2]);
    Reaction reaction(token);

    std::string arg;
    std::stringstream left(m[1]);
    while (std::getline(left, arg, ',')) { reaction.input.push_back(Token(arg)); }

    system.add_reaction(reaction);
  }
  stream.close();

  size_t total_ore    = system.make_fuel();
  size_t ore_per_fuel = initial_ore - total_ore;

  std::cout << ore_per_fuel << " " << (initial_ore / ore_per_fuel) << std::endl;

  std::cout << system.search(initial_ore / ore_per_fuel) << std::endl;

  // total_ore = system.make_fuel(total_ore / ore_per_fuel);
  // std::cout << (total_ore / ore_per_fuel) << std::endl;

  // size_t count = 0;
  // while (size_t n = system.make_fuel()) {
  //   std::cout << n << std::endl;
  //   count++;
  // }
  // std::cout << count << std::endl;

  return 0;
}