#include <iostream>
#include <fstream>
#include <vector>
#include <list>
#include <utility>
#include <algorithm>

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
  IntcodeProgram() : size_(0), current_index_(0), relative_base_(0){};
  IntcodeProgram(char *filename) : current_index_(0), relative_base_(0) {
    std::ifstream stream(filename);
    if (!stream.is_open()) { throw std::runtime_error("unable to open file"); }
    std::string token;
    while (std::getline(stream, token, ',')) { program_.push_back(std::stol(token)); }
    stream.close();
    size_ = program_.size();
  }
  IntcodeProgram(std::vector<T> data) : program_(data), size_(data.size()), current_index_(0), relative_base_(0){};

  void set_input(std::list<T> *input) { input_ = input; }
  std::list<T> *output() { return &output_; }

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
          if (!(input_->size())) return false;
          get(current_index_ + 1, op.mode1) = input_->front();
          input_->pop_front();
          break;
        }
        case 4: {
          // std::cout << get(current_index_ + 1, op.mode1) << std::endl;
          output_.push_back(get(current_index_ + 1, op.mode1));
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
  std::list<T> *input_;
  std::list<T> output_;
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

template <typename T = long>
class Grid {
  public:
  Grid() : current_index_(0), current_({0, 0}), direction_({0, 1}) {
    coords_.push_back(current_);
    values_.push_back(T(1));
  };

  size_t size() const { return coords_.size(); }

  T move(const T &argument) {
    if (argument) {
      int tmp      = direction_.x;
      direction_.x = -direction_.y;
      direction_.y = tmp;
    } else {
      int tmp      = direction_.x;
      direction_.x = direction_.y;
      direction_.y = -tmp;
    }
    current_ += direction_;
    auto iter      = std::lower_bound(coords_.begin(), coords_.end(), current_);
    current_index_ = iter - coords_.begin();
    if (current_index_ >= coords_.size() || coords_[current_index_] != current_) {
      coords_.insert(iter, current_);
      values_.insert(values_.begin() + current_index_, T(0));
      return T(0);
    }
    return values_[current_index_];
  }

  void paint(T color) { values_[current_index_] = color; }

  friend std::ostream &operator<<(std::ostream &os, const Grid<T> &grid) {
    size_t size = grid.size();
    if (size <= 0) { return os; }

    T min_x = 0, max_x = 0, min_y = 0, max_y = 0;
    for (const auto &coord : grid.coords_) {
      if (coord.x < min_x) min_x = coord.x;
      if (coord.x > max_x) max_x = coord.x;
      if (coord.y < min_y) min_y = coord.y;
      if (coord.y > max_y) max_y = coord.y;
    }

    size_t index = grid.size() - 1;
    for (T y = max_y; y >= min_y; --y) {
      for (T x = max_x; x >= min_x; --x) {
        auto coord = grid.coords_[index];
        if (x == coord.x && y == coord.y) {
          if (grid.values_[index])
            os << 'O';
          else
            os << ' ';
          index--;
        } else {
          os << ' ';
        }
      }
      os << std::endl;
    }
    return os;
  }

  private:
  size_t current_index_;
  std::vector<Coord<T>> coords_;
  std::vector<T> values_;
  Coord<T> current_, direction_;
};

template <typename T = long>
class Robot {
  public:
  Robot(char *filename) : program(filename){};

  size_t run(T initial) {
    std::list<T> input = {initial};
    program.set_input(&input);
    while (true) {
      if (program.run()) break;

      grid.paint(program.output()->front());
      program.output()->pop_front();

      T color = grid.move(program.output()->front());
      program.output()->pop_front();

      input.push_back(color);
    }
    return grid.size();
  }

  friend std::ostream &operator<<(std::ostream &os, const Robot<T> &robot) {
    os << robot.grid;
    return os;
  }

  private:
  Grid<T> grid;
  IntcodeProgram<T> program;
};
