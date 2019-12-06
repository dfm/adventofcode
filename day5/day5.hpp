#include <iostream>
#include <vector>

template <typename T>
class IntcodeProgram {
  public:
  IntcodeProgram(std::vector<T> data) : program_(data), size_(data.size()){};

  T &operator()(size_t index, int mode = 0) {
    if (index >= size_) { throw std::runtime_error("out of bounds"); }
    if (mode == 0) { return operator()(program_[index], 1); }
    return program_[index];
  }
  T const &operator()(size_t index, int mode = 0) const {
    if (index >= size_) { throw std::runtime_error("out of bounds"); }
    if (mode == 0) { return operator()(program_[index], 1); }
    return program_[index];
  }

  size_t size() const { return size_; }
  friend std::ostream &operator<<(std::ostream &os, const IntcodeProgram<T> &program) {
    size_t size = program.size();
    if (size <= 0) { return os; }
    for (size_t index = 0; index < size - 1; ++index) { os << program.program_[index] << ","; }
    os << program.program_[size - 1] << std::endl;
    return os;
  }

  private:
  std::vector<T> program_;
  size_t size_;
};

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
T run_intcode_program(IntcodeProgram<T> &program) {
  size_t size  = program.size();
  size_t index = 0;
  while (index < size) {
    auto op = parse_op_code(program(index, 1));
    if (op.code == 99) {
      return program(0, 1);
    } else if (op.code == 1) {
      program(index + 3, op.mode3) = program(index + 1, op.mode1) + program(index + 2, op.mode2);
    } else if (op.code == 2) {
      program(index + 3, op.mode3) = program(index + 1, op.mode1) * program(index + 2, op.mode2);
    } else if (op.code == 3) {
      size_t arg;
      std::cin >> arg;
      program(index + 1, op.mode1) = arg;
    } else if (op.code == 4) {
      std::cout << program(index + 1, op.mode1) << std::endl;
    } else if (op.code == 5) {
      if (program(index + 1, op.mode1)) {
        index = program(index + 2, op.mode2);
      } else {
        index += 3;
      }
    } else if (op.code == 6) {
      if (!program(index + 1, op.mode1)) {
        index = program(index + 2, op.mode2);
      } else {
        index += 3;
      }
    } else if (op.code == 7) {
      program(index + 3, op.mode3) = program(index + 1, op.mode1) < program(index + 2, op.mode2);
    } else if (op.code == 8) {
      program(index + 3, op.mode3) = program(index + 1, op.mode1) == program(index + 2, op.mode2);
    } else {
      throw std::runtime_error("invalid op");
    }
    index += op.width;
  }
  return 0;
}