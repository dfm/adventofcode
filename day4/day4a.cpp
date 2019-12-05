#include <iostream>
#include <array>
#include <cmath>

typedef std::array<unsigned, 6> Password;

bool is_valid(const Password &password) {
  bool has_double = false;
  for (unsigned i = 0; i < 5; ++i) {
    if (password[i] > password[i + 1]) return false;
    if (password[i] == password[i + 1]) has_double = true;
  }
  return has_double;
}

unsigned increment(Password &password, unsigned index) {
  if (password[index] < 9) {
    password[index]++;
    return password[index];
  }
  unsigned value  = increment(password, index - 1);
  password[index] = value;
  return value;
}

unsigned increment(Password &password) { return increment(password, 5); }

std::ostream &operator<<(std::ostream &os, const Password &password) {
  for (unsigned i = 0; i < 6; ++i) { os << password[i] << " "; }
  return os;
}

size_t to_long(const Password &pass) {
  size_t value = 0;
  for (unsigned i = 0; i < 6; ++i) { value += pass[i] * std::pow(10, 5 - i); }
  return value;
}

bool operator<(const Password &pass1, const Password &pass2) { return to_long(pass1) < to_long(pass2); }

int main() {
  size_t count      = 0;
  Password password = {2, 7, 8, 3, 8, 4};
  while (password < Password({8, 2, 4, 7, 9, 5})) {
    if (is_valid(password)) {
      count++;
      std::cout << to_long(password) << std::endl;
    }
    increment(password);
  }
  std::cout << "Count: " << count << std::endl;
}