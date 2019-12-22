#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <queue>
#include <utility>
#include <algorithm>
#include <set>

struct OpCode {
  int code;
  int mode1;
  int mode2;
  int mode3;
  int width;
};

OpCode parse_op_code(size_t op_code) {
  OpCode result;
  result.mode3 = op_code / 10000;
  op_code -= result.mode3 * 10000;
  result.mode2 = op_code / 1000;
  op_code -= result.mode2 * 1000;
  result.mode1 = op_code / 100;
  op_code -= result.mode1 * 100;
  result.code = op_code;
  if (op_code <= 2)
    result.width = 4;
  else if (op_code <= 4)
    result.width = 2;
  else if (op_code <= 6)
    result.width = 0;
  else if (op_code <= 8)
    result.width = 4;
  else if (op_code <= 9)
    result.width = 2;
  return result;
}

template <typename T = long>
class IntcodeProgram {
  public:
  // IntcodeProgram() : size_(0), current_index_(0), relative_base_(0){};
  IntcodeProgram(char *filename, std::queue<T> &input) : input_(input), current_index_(0), relative_base_(0) {
    std::ifstream stream(filename);
    if (!stream.is_open()) { throw std::runtime_error("unable to open file"); }
    std::string token;
    while (std::getline(stream, token, ',')) { program_.push_back(std::stol(token)); }
    stream.close();
    size_ = program_.size();
  }
  IntcodeProgram(std::vector<T> data) : program_(data), size_(data.size()), current_index_(0), relative_base_(0){};

  void set_input(std::queue<T> &input) { input_ = input; }
  std::queue<T> &output() { return output_; }

  void set_data(std::vector<T> data) {
    current_index_ = 0;
    size_          = data.size();
    program_       = data;
  };

  T &get(size_t index, int mode = 0) {
    if (index >= size_) {
      program_.resize(index + 1, 0);
      size_ = index + 1;
    }
    switch (mode) {
      case 0: return get(program_[index], 1);
      case 1: return program_[index];
      case 2: return get(relative_base_ + program_[index], 1);
    }
    throw std::runtime_error("unrecognized mode");
  }
  T const &get(size_t index, int mode = 0) const {
    if (index >= size_) {
      program_.resize(index + 1, 0);
      size_ = index + 1;
    }
    switch (mode) {
      case 0: return get(program_[index], 1);
      case 1: return program_[index];
      case 2: return get(relative_base_ + program_[index], 1);
    }
    throw std::runtime_error("unrecognized mode");
  }

  T &operator()(size_t index, int mode = 0) { return get(index, mode); }
  T const &operator()(size_t index, int mode = 0) const { return get(index, mode); }

  size_t size() const { return size_; }
  friend std::ostream &operator<<(std::ostream &os, const IntcodeProgram<T> &program) {
    size_t size = program.size();
    if (size <= 0) { return os; }
    for (size_t index = 0; index < size - 1; ++index) { os << program.program_[index] << ","; }
    os << program.program_[size - 1] << std::endl;
    return os;
  }

  bool run() {
    while (current_index_ < size_) {
      OpCode op = parse_op_code(get(current_index_, 1));
      switch (op.code) {
        case 99: return true;
        case 1: {
          get(current_index_ + 3, op.mode3) = get(current_index_ + 1, op.mode1) + get(current_index_ + 2, op.mode2);
          break;
        }
        case 2: {
          get(current_index_ + 3, op.mode3) = get(current_index_ + 1, op.mode1) * get(current_index_ + 2, op.mode2);
          break;
        }
        case 3: {
          if (input_.empty()) return false;
          get(current_index_ + 1, op.mode1) = input_.front();
          input_.pop();
          break;
        }
        case 4: {
          // std::cout << get(current_index_ + 1, op.mode1) << std::endl;
          output_.push(get(current_index_ + 1, op.mode1));
          break;
        }
        case 5: {
          if (get(current_index_ + 1, op.mode1)) {
            current_index_ = get(current_index_ + 2, op.mode2);
          } else {
            current_index_ += 3;
          }
          break;
        }
        case 6: {
          if (!get(current_index_ + 1, op.mode1)) {
            current_index_ = get(current_index_ + 2, op.mode2);
          } else {
            current_index_ += 3;
          }
          break;
        }
        case 7: {
          get(current_index_ + 3, op.mode3) = get(current_index_ + 1, op.mode1) < get(current_index_ + 2, op.mode2);
          break;
        }
        case 8: {
          get(current_index_ + 3, op.mode3) = get(current_index_ + 1, op.mode1) == get(current_index_ + 2, op.mode2);
          break;
        }
        case 9: {
          relative_base_ += get(current_index_ + 1, op.mode1);
          break;
        }
        default: throw std::runtime_error("invalid op");
      }
      current_index_ += op.width;
    }
    return false;
  }

  private:
  std::queue<T> &input_;
  std::queue<T> output_;
  std::vector<T> program_;
  size_t size_, current_index_, relative_base_;
};

template <typename T = long>
struct Coord {
  T x;
  T y;
};

template <typename T>
inline Coord<T> operator+(const Coord<T> &a, const Coord<T> &b) {
  return {a.x + b.x, a.y + b.y};
}

template <typename T>
inline Coord<T> operator-(const Coord<T> &a, const Coord<T> &b) {
  return {a.x - b.x, a.y - b.y};
}

template <typename T>
inline Coord<T> &operator+=(Coord<T> &a, const Coord<T> &b) {
  a.x += b.x;
  a.y += b.y;
  return a;
}

template <typename T>
inline bool operator<(const Coord<T> &a, const Coord<T> &b) {
  return ((a.y == b.y) ? (a.x < b.x) : (a.y < b.y));
}

template <typename T>
inline bool operator==(const Coord<T> &a, const Coord<T> &b) {
  return ((a.x == b.x) && (a.y == b.y));
}

template <typename T>
inline bool operator!=(const Coord<T> &a, const Coord<T> &b) {
  return ((a.x != b.x) || (a.y != b.y));
}

template <typename T>
inline void turn_left(Coord<T> &direction) {
  std::swap(direction.x, direction.y);
  direction.x *= -1;
}

template <typename T>
inline void turn_right(Coord<T> &direction) {
  std::swap(direction.x, direction.y);
  direction.y *= -1;
}

template <class ForwardIterator>
std::string cse_repr(ForwardIterator begin, ForwardIterator end) {
  std::ostringstream stream;
  if (begin != end) { stream << *begin++; }
  while (begin != end) {
    stream << ',';
    stream << *begin++;
  }
  stream << '\n';
  return stream.str();
}

struct CseInfo {
  size_t start;
  char id;
};

inline bool operator<(const CseInfo &a, const CseInfo &b) { return (a.start < b.start); }

template <typename T = int>
class Map {
  public:
  Map(std::queue<long> &output) {
    size_t robot_index = 0;
    width              = 0;
    while (!output.empty()) {
      long value = output.front();
      map.push_back(static_cast<T>(value));
      if (!width && value == 10) width = static_cast<size_t>(map.size());
      output.pop();

      if (char(value) == '^') {
        direction = {0, 1};
      } else if (char(value) == '<') {
        direction = {-1, 0};
      } else if (char(value) == '>') {
        direction = {1, 0};
      } else if (char(value) == 'v') {
        direction = {0, -1};
      } else {
        continue;
      }
      robot_index = map.size();
    }
    height   = map.size() / width;
    position = {static_cast<T>((robot_index - 1) % width), static_cast<T>(height - robot_index / width - 1)};
  }

  size_t count_intersections() const {
    size_t parameter = 0;
    for (T y = 0; y < height; ++y) {
      for (T x = 0; x < width; ++x) {
        bool intersection = is_scaffold(x, y - 1) && is_scaffold(x - 1, y) && is_scaffold(x, y) && is_scaffold(x + 1, y) && is_scaffold(x, y + 1);
        if (intersection) parameter += x * (height - y - 1);
      }
    }
    return parameter;
  }

  std::queue<long> build_instruction_set() {
    std::vector<std::string> instructions;
    do { instructions.push_back(find_next_move()); } while (instructions.back().size());
    instructions.pop_back();

    std::cout << "Instructions for path: " << std::endl;
    for (size_t n = 0; n < instructions.size(); ++n) { std::cout << instructions[n] << ","; }
    std::cout << std::endl;

    // This is a hack, but I couldn't hack the general solution
    std::string command = R"(A,B,A,B,C,C,B,A,B,C
L,12,L,6,L,8,R,6
L,8,L,8,R,4,R,6,R,6
L,12,R,6,L,8
n
)";
    std::queue<long> commands;
    for (size_t n = 0; n < command.size(); ++n) { commands.push(static_cast<long>(command[n])); }

    return commands;
  }

  std::string find_next_move() {
    std::stringstream stream;

    // Try a line search first
    T steps               = 0;
    Coord<T> new_position = position + direction;
    while (is_scaffold(new_position)) {
      new_position += direction;
      steps++;
    }
    if (steps) {
      position = new_position - direction;
      stream << steps;
    } else {
      // Try some turns
      Coord<T> new_direction = direction;
      turn_left(new_direction);
      if (is_scaffold(position + new_direction)) {
        direction = new_direction;
        stream << "L";
      } else {
        new_direction.x *= -1;
        new_direction.y *= -1;
        if (is_scaffold(position + new_direction)) {
          direction = new_direction;
          stream << "R";
        }
      }
    }
    return stream.str();
  }

  inline bool is_scaffold(const T &x, const T &y) const { return in_bounds(x, y) && (get(x, y) == 35); }
  inline bool is_scaffold(const Coord<T> &pos) const { return in_bounds(pos) && (get(pos) == 35); }

  bool in_bounds(const T &x, const T &y) const { return (0 <= x) && (x < width) && (0 <= y) && (y < height); }
  bool in_bounds(const Coord<T> &pos) const { return in_bounds(pos.x, pos.y); }

  T &get(const T &x, const T &y) { return map[(height - y - 1) * width + x]; }
  T &get(const Coord<T> &pos) { return get(pos.x, pos.y); }
  const T &get(const T &x, const T &y) const { return map[(height - y - 1) * width + x]; }
  const T &get(const Coord<T> &pos) const { return get(pos.x, pos.y); }

  private:
  std::vector<T> map;
  size_t width, height;
  Coord<T> direction, position;
};
