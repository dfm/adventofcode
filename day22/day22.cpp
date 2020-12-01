// I couldn't figure it out myself. Here's the ref I used:
//  https://gist.github.com/romkatv/8ef7ea27ddce1de7b1b6f9b5a41838c4

#include <stdint.h>
#include <iostream>
#include <string>
#include <regex>

const int64_t m = 119315717514047, n = 101741582076661, needle = 2020;

// https://en.wikipedia.org/wiki/Exponentiation_by_squaring
const auto combine = [](auto f, int64_t unit, int64_t a, int64_t b) {
  for (int64_t r = unit;; b >>= 1, a = f(a, a)) {
    if (!b) return r;
    if (b & 1) r = f(r, a);
  }
};

static int64_t add(int64_t a, int64_t b) { return (m + (a + b) % m) % m; } // +  (mod m)
static int64_t mul(int64_t a, int64_t b) { return combine(add, 0, a, b); } // *  (mod m)
static int64_t pow(int64_t a, int64_t b) { return combine(mul, 1, a, b); } // ** (mod m)

int main() {
  std::smatch match;
  std::regex cut_re("cut ([\\-0-9]+)");
  std::regex deal_re("deal with increment ([\\-0-9]+)");

  int64_t k = 1, b = 0, x;
  for (std::string s; std::getline(std::cin, s);) {
    if (std::regex_search(s, match, deal_re)) {
      k = mul(k, x = std::stoll(match[1]));
      b = mul(b, x);
    } else if (std::regex_search(s, match, cut_re)) {
      b = add(b, -std::stoll(match[1]));
      b = b;
    } else {
      k = add(0, -k);
      b = add(-1, -b);
    }
  }
  x = mul(b, pow(k - 1, m - 2)); // compute (λ c => k*c + b)**-n and feed it needle
  std::cout << add(mul(add(x, needle), pow(pow(k, m - 2), n)), -x) << std::endl;
}

// #include <iostream>
// #include <vector>
// #include <algorithm>
// #include <regex>

// template <typename T>
// inline T mod(const T &a, const T &m) {
//   return (m + a % m) % m;
// }

// template <typename T>
// T gcd(const T &a, const T &b, T &x, T &y) {
//   if (!a) {
//     x = 0;
//     y = 1;
//     return b;
//   }
//   T x1, y1, val = gcd(b % a, a, x1, y1);
//   x = y1 - (b / a) * x1;
//   y = x1;
//   return val;
// }

// template <typename T>
// T inv_mod(const T &a, const T &m) {
//   T x, y;
//   T g = gcd(a, m, x, y);
//   if (g != 1) throw std::runtime_error("invalid inv_mod");
//   return mod(x, m);
// }

// template <typename T>
// T divide(const T &a, const T &b, const T &m) {
//   T inv = inv_mod(b, m);
//   return mod(a * inv, m);
// }

// template <typename T>
// T multiply(const T &a, const T &b, const T &m) {
//   return mod(a * b, m);
// }

// template <typename T>
// T power(const T &a, const T &k, const T &m) {
//   if (k <= 0) return 1;
//   T bit   = 1;
//   T power = mod(a, m);
//   T out   = 1;
//   while (bit <= k) {
//     if (k & bit) { out = multiply(out, power, m); }
//     power = multiply(power, power, m);
//     bit <<= 1;
//   }
//   return out;
// }

// int main(int argc, char *argv[]) {
//   typedef long long Integer;

//   if (argc != 3) {
//     std::cerr << "Must provide a deck size" << std::endl;
//     return 1;
//   }
//   size_t n_max      = std::stoul(argv[1]);
//   size_t n_shuffles = std::stoul(argv[2]);

//   Integer a = 1, b = 0;
//   std::string line;
//   while (std::getline(std::cin, line)) {
//     std::smatch m;
//     std::regex cut_re("cut ([\\-0-9]+)");
//     std::regex deal_re("deal with increment ([\\-0-9]+)");
//     Integer factor = 1, offset = 0;
//     if (std::regex_search(line, m, deal_re)) {
//       factor = std::stoi(m[1]);
//     } else if (std::regex_search(line, m, cut_re)) {
//       offset = -std::stoi(m[1]);
//     } else {
//       factor = -1;
//       offset = n_max - 1;
//     }
//     a = mod<Integer>(factor * a, n_max);
//     b = mod<Integer>(factor * b + offset, n_max);
//   }
//   // std::cout << a << " " << b << "\n";

//   if (n_max <= 10) {
//     for (size_t n = 0; n < n_max; ++n) { std::cout << mod<Integer>(a * n + b, n_max) << " "; }
//     std::cout << std::endl;
//   } else if (n_max == 10007) {
//     Integer index = mod<Integer>(a * 2019 + b, n_max);
//     std::cout << "Part 1: " << index << std::endl;
//   } else if (n_max == 119315717514047) {
//     // Still not right!
//     a = power<Integer>(a, n_shuffles, n_max);
//     b = multiply<Integer>(b, divide<Integer>(power<Integer>(a, n_shuffles, n_max) - 1, a - 1, n_max), n_max);
//     std::cout << "Part 2: " << divide<Integer>(mod<Integer>(2020 - b, n_max), a, n_max) << std::endl;
//   }

//   return 0;
// }