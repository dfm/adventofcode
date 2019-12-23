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
