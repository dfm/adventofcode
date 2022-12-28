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
