#include <iostream>
#include <vector>

template <typename T>
class IntcodeProgram {
  public:
  IntcodeProgram(std::vector<T> data) : program_(data), size_(data.size()){};

  T &operator()(size_t index) {
    if (index >= size_) { throw std::runtime_error("out of bounds"); }
    return program_[index];
  }
  T const &operator()(size_t index) const {
    if (index >= size_) { throw std::runtime_error("out of bounds"); }
    return program_[index];
  }

  size_t size() const { return size_; }
  friend std::ostream &operator<<(std::ostream &os, const IntcodeProgram<T> &program) {
    size_t size = program.size();
    if (size <= 0) { return os; }
    for (size_t index = 0; index < size - 1; ++index) { os << program(index) << ","; }
    os << program(size - 1) << std::endl;
    return os;
  }

  private:
  std::vector<T> program_;
  size_t size_;
};

template <typename T>
T run_intcode_program(IntcodeProgram<T> program) {
  size_t size = program.size();
  for (size_t index = 0; index < size; index += 4) {
    T op = program(index);
    if (op == 99) {
      return program(0);
    } else if (op == 1) {
      program(program(index + 3)) = program(program(index + 1)) + program(program(index + 2));
    } else if (op == 2) {
      program(program(index + 3)) = program(program(index + 1)) * program(program(index + 2));
    } else {
      throw std::runtime_error("invalid op");
    }
  }
  return 0;
}