#include <unordered_map>

#include "aoc/aoc.hpp"

namespace {

namespace grammar {

namespace dsl = lexy::dsl;

enum cmd_t { _output = 0, _cd, _ls };

struct instr_t {
  cmd_t cmd = cmd;
  std::string arg;
  bool is_dir = false;
  size_t size = 0;

  instr_t(cmd_t cmd) : cmd(cmd) {}
  instr_t(cmd_t cmd, std::string arg, bool is_dir = false)
      : cmd(cmd), arg(std::move(arg)), is_dir(is_dir) {}
  instr_t(cmd_t cmd, size_t size, std::string arg)
      : cmd(cmd), arg(std::move(arg)), size(size) {}
};

struct str {
  static constexpr auto rule = dsl::identifier(
      dsl::ascii::alnum / dsl::ascii::punct - dsl::ascii::newline);
  static constexpr auto value = lexy::as_string<std::string>;
};

struct cd {
  static constexpr auto rule = LEXY_LIT("cd ") >> dsl::p<str>;
  static constexpr auto value =
      lexy::callback<instr_t>([](auto s) { return instr_t(cmd_t::_cd, s); });
};

struct ls {
  static constexpr auto rule = LEXY_LIT("ls");
  static constexpr auto value =
      lexy::callback<instr_t>([]() { return instr_t(cmd_t::_ls); });
};

struct cmd {
  static constexpr auto rule = LEXY_LIT("$ ") >> (dsl::p<cd> | dsl::p<ls>);
  static constexpr auto value = lexy::forward<instr_t>;
};

struct dir {
  static constexpr auto rule = LEXY_LIT("dir ") >> dsl::p<str>;
  static constexpr auto value = lexy::callback<instr_t>(
      [](auto s) { return instr_t(cmd_t::_output, s, true); });
};

struct file {
  static constexpr auto rule =
      dsl::else_ >> dsl::integer<size_t> + dsl::ascii::space + dsl::p<str>;
  static constexpr auto value = lexy::callback<instr_t>(
      [](auto n, auto s) { return instr_t(cmd_t::_output, n, s); });
};

struct instr {
  static constexpr auto rule =
      (dsl::p<cmd> | dsl::p<dir> | dsl::p<file>)+dsl::newline;
  static constexpr auto value = lexy::forward<instr_t>;
};

struct parser {
  static constexpr auto rule =
      dsl::terminator(dsl::eof).opt_list(dsl::p<instr>);
  static constexpr auto value = lexy::as_list<std::vector<instr_t>>;
};

}  // namespace grammar

inline auto build_tree(auto data) {
  std::unordered_map<std::string, std::pair<size_t, size_t>> tree;
  std::vector<std::string> current_path;
  size_t depth = 0;
  for (const auto &i : data) {
    if (i.cmd == grammar::cmd_t::_cd) {
      if (i.arg == "/") {
        current_path.resize(0);
        current_path.push_back(".");
        depth = 0;
      } else if (i.arg == "..") {
        current_path.pop_back();
        depth--;
      } else {
        current_path.push_back(i.arg);
        depth++;
      }
    } else if (i.cmd == grammar::cmd_t::_output && !i.is_dir) {
      std::ostringstream id;
      for (const auto &dir : current_path) {
        id << dir << "/";
        if (auto search = tree.find(id.str()); search != tree.end()) {
          search->second.first += i.size;
        } else {
          tree.insert({id.str(), {i.size, depth}});
        }
      }
    }
  }
  return tree;
}

AOC_IMPL(2022, 7) {
  using parser = grammar::parser;
  static constexpr auto part1 = [](auto data) {
    auto tree = build_tree(data);
    size_t total = 0;
    for (const auto &s : tree) {
      if (s.second.first < 100000) {
        total += s.second.first;
      }
    }
    return total;
  };
  static constexpr auto part2 = [](auto data) {
    auto tree = build_tree(data);
    size_t target = tree[std::string("./")].first - 40000000;
    size_t result = std::numeric_limits<size_t>::max();
    for (const auto &s : tree) {
      auto delta = s.second.first;
      if (delta >= target) {
        result = std::min(result, delta);
      }
    }
    return result;
  };
};

AOC_TEST_CASE(95437, 24933642, R"($ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
)")

}  // namespace
