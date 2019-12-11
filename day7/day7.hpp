#include <iostream>
#include <vector>
#include <list>

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
  return result;
}

template <typename T>
class IntcodeProgram {
  public:
  IntcodeProgram() : size_(0), current_index_(0){};
  IntcodeProgram(std::vector<T> data) : program_(data), size_(data.size()), current_index_(0){};

  void set_input(std::list<T> *input) { input_ = input; }
  std::list<T> *output() { return &output_; }

  void set_data(std::vector<T> data) {
    current_index_ = 0;
    size_          = data.size();
    program_       = data;
  };

  T &get(size_t index, int mode = 0) {
    if (index >= size_) { throw std::runtime_error("out of bounds"); }
    if (mode == 0) { return operator()(program_[index], 1); }
    return program_[index];
  }
  T const &get(size_t index, int mode = 0) const {
    if (index >= size_) { throw std::runtime_error("out of bounds"); }
    if (mode == 0) { return operator()(program_[index], 1); }
    return program_[index];
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
      auto op = parse_op_code(get(current_index_, 1));
      if (op.code == 99) {
        return true;
      } else if (op.code == 1) {
        get(current_index_ + 3, op.mode3) = get(current_index_ + 1, op.mode1) + get(current_index_ + 2, op.mode2);
      } else if (op.code == 2) {
        get(current_index_ + 3, op.mode3) = get(current_index_ + 1, op.mode1) * get(current_index_ + 2, op.mode2);
      } else if (op.code == 3) {
        if (!(input_->size())) return false;
        get(current_index_ + 1, op.mode1) = input_->front();
        input_->pop_front();
      } else if (op.code == 4) {
        output_.push_back(get(current_index_ + 1, op.mode1));
      } else if (op.code == 5) {
        if (get(current_index_ + 1, op.mode1)) {
          current_index_ = get(current_index_ + 2, op.mode2);
        } else {
          current_index_ += 3;
        }
      } else if (op.code == 6) {
        if (!get(current_index_ + 1, op.mode1)) {
          current_index_ = get(current_index_ + 2, op.mode2);
        } else {
          current_index_ += 3;
        }
      } else if (op.code == 7) {
        get(current_index_ + 3, op.mode3) = get(current_index_ + 1, op.mode1) < get(current_index_ + 2, op.mode2);
      } else if (op.code == 8) {
        get(current_index_ + 3, op.mode3) = get(current_index_ + 1, op.mode1) == get(current_index_ + 2, op.mode2);
      } else {
        throw std::runtime_error("invalid op");
      }
      current_index_ += op.width;
    }
    return false;
  }

  private:
  std::list<T> *input_;
  std::list<T> output_;
  std::vector<T> program_;
  size_t size_, current_index_;
};

// class IntcodePipeline {

// };
