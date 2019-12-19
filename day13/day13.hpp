#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <list>
#include <utility>
#include <algorithm>

#include <thread>
#include <chrono>
#include <ncurses.h>

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

template <typename T>
struct Coord {
  T x;
  T y;
  Coord() : x(0), y(0){};
  Coord(const T &x, const T &y) : x(x), y(y){};
  Coord(const Coord &other) : Coord(other.x, other.y){};
};

template <typename T, bool Draw = true>
class Game {
  public:
  Game(char *filename) : input_(0), program_(filename) { program_.set_input(&input_); }

  void add_tile(const T &x, const T &y, const T &val) {}

  void run() {
    program_.run();
    std::list<T> results = *(program_.output());
    size_t size          = results.size();
    size_t num_tiles     = size / 3;
    std::vector<T> x(num_tiles), y(num_tiles), val(num_tiles);
    for (size_t n = 0, ind = 0; n < size; n += 3, ++ind) {
      x[ind] = results.front();
      results.pop_front();
      y[ind] = results.front();
      results.pop_front();
      val[ind] = results.front();
      results.pop_front();
    }

    T max_x = 1 + *std::max_element(x.begin(), x.end());
    T max_y = 1 + *std::max_element(y.begin(), y.end());
    stride_ = size_t(max_x);
    grid_.resize(max_y * max_x);
    for (size_t n = 0; n < num_tiles; ++n) { grid_[y[n] * stride_ + x[n]] = val[n]; }
  }

  size_t count(const T &target) const { return std::count(grid_.begin(), grid_.end(), target); }

  void draw(const T &x, const T &y, const T &val) {
    if (x == -1 && y == 0) {
      std::stringstream stream;
      stream << val;
      mvprintw(0, 0, stream.str().c_str());
      score_ = val;
    } else {
      switch (val) {
        case 1: mvprintw(y + 1, x, "|"); break;
        case 2: mvprintw(y + 1, x, "B"); break;
        case 3: {
          mvprintw(y + 1, x, "_");
          paddle_.x = x;
          paddle_.y = y;
          break;
        }
        case 4: {
          mvprintw(y + 1, x, "O");
          ball_.x = x;
          ball_.y = y;
          break;
        }
        default: mvprintw(y + 1, x, " ");
      }
    }
  }

  void update(const T &x, const T &y, const T &val) {
    if (x == -1 && y == 0) {
      score_ = val;
    } else if (val == 3) {
      paddle_.x = x;
      paddle_.y = y;
    } else if (val == 4) {
      ball_.x = x;
      ball_.y = y;
    }
  }

  bool one_play() {
    bool finished        = program_.run();
    std::list<T> results = *(program_.output());
    size_t size          = results.size();
    for (size_t n = 0; n < size; n += 3) {
      T x = results.front();
      results.pop_front();
      T y = results.front();
      results.pop_front();
      T val = results.front();
      results.pop_front();

      if (Draw)
        draw(x, y, val);
      else
        update(x, y, val);
    }
    refresh();
    return finished;
  }

  T play(const T quarters) {
    program_.get(0, 1) = quarters;

    if (Draw) {
      initscr();
      noecho();
      curs_set(FALSE);
    }

    while (true) {
      if (one_play()) break;
      input_.push_back(T(ball_.x > paddle_.x) - T(ball_.x < paddle_.x));
      if (Draw) std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }

    if (Draw) endwin();

    return score_;
  }

  friend std::ostream &operator<<(std::ostream &os, const Game<T> &game) {
    size_t size = game.grid_.size();
    for (size_t n = 0; n < size;) {
      for (size_t k = 0; k < game.stride_; ++k, ++n) {
        switch (game.grid_[n]) {
          case 0: os << ' '; break;
          case 1: os << '|'; break;
          case 2: os << 'B'; break;
          case 3: os << '_'; break;
          case 4: os << 'O'; break;
          default: os << ' ';
        }
      }
      os << std::endl;
    }
    return os;
  }

  private:
  std::list<T> input_;
  IntcodeProgram<T> program_;

  T score_;
  size_t stride_;
  std::vector<T> grid_;
  Coord<T> prev_ball_, ball_, paddle_;
};
